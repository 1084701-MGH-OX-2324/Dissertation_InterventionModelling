###########Libraries
start.time <- Sys.time()

library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(reshape2)
library(openxlsx)

#setwd("<dir/name.")

file_name <- "Master_SARS-CoV-2_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx"
file <- read_excel("Master_SARS-CoV-2_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx", sheet="Runs")
results_summary <- list()

##################
# Functions
##################

### SEIR Model Function
disease_int <- function(t, Y, parameters) {
  with(as.list(c(Y, parameters)),
       {
         # Extract compartment values
         S <- Y[Sindex]
         E <- Y[Eindex]
         C <- Y[Cindex]
         H <- Y[Hindex]
         A <- Y[Aindex]
         R <- Y[Rindex]
         D <- Y[Dindex]
         
         # Combine contact matrices
         contacts <- contact_home + contact_other + contact_school + contact_work
         
         # Calculate total population
         P <- (S + E + C + R + H + A)
         
         # Lockdown logic
         if (sum(H) > hospital_beds_threshold) {
           if (lock == 0) {
             lock <<- 1
             lockdown_start <<- t + lockdown_lag
           }
         } else {
           if (t >= (lockdown_start + lockdown_duration)) {
             lockdown_start <<- 10^8
             lock <<- 0
           }
         }
         
         # Calculate lockdown effect
         lockdown <- ifelse(t >= lockdown_start & t < (lockdown_start + lockdown_duration),
                            (1-lockdown_effect), 1)
         
         # Shielding logic
         if (t >= shielding_start & t < (shielding_start+shielding_duration)){
           shielding <- 1 - (shielding_effect * shielding_effect_age)
         } else {
           shielding <- rep(1,21)
         }
         
         # Force of infection
         lam <- p * lockdown *
           contacts %*% (shielding * (rhoa * A + rho * E + C + rhoh *H) / P)
         
         # Differential equations
         dSdt <- -S * lam + tau * R + ageing %*% S
         dEdt <- S * lam - gamma * E + ageing %*% E
         dCdt <- ageing %*% C + gamma * pc * (1-ihr) * E - C * nuc
         dHdt <- ageing %*% H + gamma * ihr * E - H * nuh
         dAdt <- ageing %*% A + (1-pc)*(1-ihr) * gamma * E - nua*A
         dDdt <- ageing %*% D + nuc * cfr * C + nuh * hfr * H
         dRdt <- ageing %*% R + nua * A + (1-cfr) * nuc * C  + nuh * H * (1-hfr) - tau*R
         
         if (t >= 365) {
           lockdown_start <<- 10^8
           lock <<- 0
         }
         
         # Return the rate of change
         list(c(dSdt, dEdt, dCdt, dHdt, dAdt, dRdt, dDdt))
       }
  )
}

# Years of Life Lost (YLL) Calculation Function
yll <- function(scenario, country){
  life_expec <- read_excel(file_name, sheet="lifeexpec")
  col_names <- colnames(life_expec)
  country_col <- which(grepl(country, col_names))
  expec_country <- life_expec[, country_col]
  yll_age <- scenario * expec_country
  return(yll_age)
}

# Bar plots to compare values for age groups
barplot_ages <- function(data, metric){
  data_long <- melt(data, id.vars = "Age")
  options(repr.plot.width=10, repr.plot.height=6)
  y_axis_name <- paste0("Number of", metric)
  title <- paste0(metric,"Sums per Age Group")
  ggplot(data_long, aes(x = Age, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    labs(x = "Age Group Floor", y = y_axis_name, fill = "Scenario") +
    ggtitle(title) +
    theme_minimal() +  
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

##############################################################
#START

working_directory <- getwd()

# Define the results directory within the working directory
result_store <- file.path(working_directory, "results/.")

if (!dir.exists(result_store)) {
  dir.create(result_store)
}

run_start <- file$Index[1]
run_end <- nrow(file)

# Load general data from the Excel file
demog_data <- read_excel(file_name, sheet="population", col_types = c("text"))
hospital_data <- read_excel(file_name, sheet="hospitalbeds", col_types = c("text"))

# Define the time frame for the simulation
startdate <- as.Date("2020-01-01")
stopdate <- as.Date("2020-12-31")
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start+1, day_stop)

lockdown_lag <- 0

for (n in 1:(run_end-run_start+1)){
  
  print(paste0("This is run number:", n))
  row_index <- which(file$Index == n)
  country <- file$Country[n]
  disease  <- file$`Disease (age curve)`[n]
  scenario <- file[row_index, ]
  
  country_names <- demog_data$country
  matching_un_name_char <- country_names[str_detect(country_names, country)]
  country_data <- filter(demog_data, country == matching_un_name_char[1])
  
  popstruc <- as.numeric(country_data$population)
  A <- length(popstruc)
  
  # Read disease-specific ratios
  ratios <- read_excel(file_name, sheet=disease)
  ihr <- ratios$ihr
  hfr <- ratios$hfr
  cfr <- ratios$cfr
  
  ### Lockdown Threshold
  population_numeric <- as.numeric(country_data$population)
  col_names_hosp <- colnames(hospital_data)
  country_col_hops <- which(grepl(country, col_names_hosp))
  
  tot_pop <- sum(population_numeric)
  hospital_beds_capacity <- as.numeric(hospital_data[1,country])
  threshold <- as.numeric(hospital_data[2,country])
  hospital_beds_threshold <- 0
  lockdown_lag <- lockdown_lag
  
  # Shielding Effect
  shielding_effect_age <- ratios$Shielding
  
  contacts <- load("contacts.RDa")
  # Adjust the names to match UN data format
  country_names <- names(contact_home)
  matching_un_name <- toString(country_names[str_detect(country_names, country)])
  
  # Extract contact matrices for the specific country
  c_home <- as.matrix(contact_home[[matching_un_name]])
  c_school <- as.matrix(contact_school[[matching_un_name]])
  c_work <- as.matrix(contact_work[[matching_un_name]])
  c_other <- as.matrix(contact_other[[matching_un_name]])
  
  # Calculate the difference in the number of age groups between matrices
  nce <- A - length(c_home[1, ])
  
  # Filling in 4 higher age groups 75-80, 80-85, 85-90, 95-100, 100+
  contact_home <- matrix(0, nrow = A, ncol = A)
  contact_school <- matrix(0, nrow = A, ncol = A)
  contact_work <- matrix(0, nrow = A, ncol = A)
  contact_other <- matrix(0, nrow = A, ncol = A)
  
  for (i in 1:(A - nce)){
    for (j in 1:(A - nce)){
      contact_home[i, j] <- c_home[i, j]
      contact_school[i, j] <- c_school[i, j]
      contact_work[i, j] <- c_work[i, j]
      contact_other[i, j] <- c_other[i, j]
    }
  }
  
  for (i in (A + 1 - nce):A){
    for (j in 1:(A - nce)){
      contact_home[i, j] <- c_home[(A - nce), j]
      contact_school[i, j] <- c_school[(A - nce), j]
      contact_work[i, j] <- c_work[(A - nce), j]
      contact_other[i, j] <- c_other[(A - nce), j]
    }
  }
  for (i in 1:(A - nce)){
    for (j in (A + 1 - nce):A){
      contact_home[i, j] <- c_home[i, (A - nce)]
      contact_school[i, j] <- c_school[i, (A - nce)]
      contact_work[i, j] <- c_work[i, (A - nce)]
      contact_other[i, j] <- c_other[i, (A - nce)]
    }
  }
  for (i in (A + 1 - nce):A){
    for (j in (A + 1 - nce):A){
      contact_home[i, j] <- c_home[(A - nce),(A - nce)]
      contact_school[i, j] <- c_school[(A - nce),(A - nce)]
      contact_work[i, j] <- c_work[(A - nce),(A - nce)]
      contact_other[i, j] <- c_other[(A - nce),(A - nce)]
    }
  }
  
  ###########################################################################
  # Average contacts per day from POLYMOD matrices 
  c <- sum((contact_home + contact_other + contact_school + contact_work) %*%
             (popstruc / sum(popstruc)))
  
  # Per year ageing matrix
  dd <- seq(1:A) / seq(1:A)
  # Create an ageing matrix that accounts for age groups moving up due to ageing
  ageing <- t(diff(diag(dd), lag = 1) / (5 * 365.25))
  ageing <- cbind(ageing, 0 * seq(1:A)) # No ageing from last compartment
  
  # Population and index case initialization
  initP <- sum(popstruc) # Total population size
  ageindcase <- 20 # Age of the index case (years)
  aci <- floor((ageindcase / 5) + 1) # Age class of the index case
  
  # Define simulation time frame
  startdate <- as.Date("2020-01-01")
  stopdate <- as.Date("2020-12-31")
  day_start <- as.numeric(startdate-startdate)
  day_stop <- as.numeric(stopdate-startdate)
  times <- seq(day_start+1, day_stop)
  
  # Initialize compartments
  initE <- 0 * popstruc # Incubating
  initE[aci] <- 1 # Place the index case in the E compartment
  
  initC <- 0 * popstruc # Infected and symptomatic
  initH <- 0 * popstruc # Hospitalized 
  initA <- 0 * popstruc # Asymptomatic
  initR <- 0 * popstruc # Immune
  initD <- 0 * popstruc # Deceased
  initI <- 0 * popstruc # Placeholder for additional compartment if needed
  
  # Calculate the initial susceptible population (non-immune)
  initS <- popstruc - initE - initC - initR - initH - initA - initD
  
  # Initial conditions for the main solution vector
  Y <- c(initS, initE, initC, initH, initA, initR, initD)
  
  # Define the indices for each compartment in the solution vector
  Sindex <- 1 : A
  Eindex <- (A + 1) : (2 * A)
  Cindex <- (2 * A + 1) : (3 * A)
  Hindex <- (3 * A + 1) : (4 * A)
  Aindex <- (4 * A + 1) : (5 * A)
  Rindex <- (5 * A + 1) : (6 * A)
  Dindex <- (6 * A + 1) : (7 * A)
  
  lock <<- 0
  lockdown_start <<- 10^8
  
  run <- file
  case <- n
  parameters <- c(
    # General
    p = file[n,]$p,   # Probability of infection given a contact
    tau = file[n,]$tau, # Rate of loss of immunity = 1/(average duration of immunity)
    
    # C-compartent
    gamma = file[n,]$gamma, # Rate of incubation to infectious stage, E to C    
    pc = file[n,]$pc,  # Proportion of clinical cases
    rho = file[n,]$rho, # Infectiousness of clinical cases (lambda)
    nuc = file[n,]$nuc,  # Rate of recovery     
    
    # H-Compartment
    rhoh = file[n,]$rhoh, # Infectiousness of hospital (lambda)
    nuh = file[n,]$nuh,  # Rate of recovery 
    
    # Asymptomatic infections
    rhoa = file[n,]$rhoa, # Relative infectiousness*contacts of asymptomatic
    nua = file[n,]$nua,    # Recovery after asymptomatic infection
    
    # D-compartment
    report = file[n,]$report,          # Proportion of all infections that are reported
    
    # Interventions
    lockdown_duration = file[n,]$`Lockdown Duration`,
    lockdown_effect = file[n,]$`Lockdown Effect`,
    shielding_start = file[n,]$`Shielding Start`,
    shielding_duration = 365,
    shielding_effect = file[n,]$`Shielding Effect`
  )
  
  out <- ode(y = Y, times = times, func = disease_int, parms = parameters, method = euler)
  
  ## Deaths
  deaths_age_groups <- t(tail(out[, (Dindex + 1)],1))
  colnames(deaths_age_groups) <- "Deaths"
  
  inc_total_int <- parameters["gamma"] * out[, (Eindex + 1)]
  dailyinc_int <- rowSums(inc_total_int)
  
  # Total population per age group (remaining)
  pop_int_nat <- out[, (Sindex + 1)] + out[, (Eindex + 1)] + out[, (Cindex + 1)] +
    out[, (Rindex + 1)] + out[, (Hindex + 1)] + out[, (Aindex + 1)] +
    out[, (Dindex + 1)]
  tpop_int_nat <- rowSums(pop_int_nat)
  time <- as.Date(out[, 1] + startdate)
  
  # Calculate daily incidence
  inc_total_int_nat <- parameters["gamma"] * out[, (Eindex + 1)]
  daily_inc <- rowSums(inc_total_int_nat)
  
  # Calculate Infection Fatality Rate (IFR)
  inf_age_groups <- colSums(out[, (Eindex + 1)],1)*parameters["gamma"]
  inf <- rowSums(out[, (Eindex + 1)],1)*parameters["gamma"]
  ifr <- sum(deaths_age_groups)/sum(inf)
  ifr_age_groups <- deaths_age_groups/inf_age_groups
  
  # Calculate R0
  fac_cases <- (parameters["pc"] * (1 - ihr)) / parameters["nuc"]
  fac_hosp <- ihr / parameters["nuh"]
  fac_asymp <- ((1 - parameters["pc"]) * (1 - ihr)) / parameters["nua"]
  beta <- parameters["p"] * c
  
  R0_calc <- mean(beta * (fac_cases + fac_hosp + fac_asymp))
  
  # Calculate Hospitalization Fatality Rate (HFR)
  hosps_age <- colSums(out[, (Eindex + 1)],1)*parameters["gamma"]*ihr
  deaths_hosp <- colSums(out[, (Hindex + 1)],1)*hfr*parameters["nuh"]
  hfr_model <- deaths_hosp/hosps_age
  hfr_model_tot <- sum(deaths_age_groups)/sum(hosps_age)
  
  # R0 Calculation (from literature)
  d_exposed <- 1 / parameters["gamma"] * parameters["rho"]
  d_cases <- 1 / parameters["nuc"] + d_exposed
  d_hosp <- 1 / parameters["nuh"] * parameters["rhoh"] + d_exposed
  d_asym <- 1 / parameters["nua"] * parameters["rhoa"] + d_exposed
  
  weight_cases <- (1 - ihr) * parameters["pc"]
  weight_hosp <- ihr
  weight_asym <- (1 - ihr) * (1 - parameters["pc"])
  
  # Weighted average duration calculation
  weighted_average_duration_i <- d_cases * weight_cases + d_hosp * weight_hosp + d_asym * weight_asym
  
  # Doubling time calculation
  dd <- mean(weighted_average_duration_i)
  doub0 <- log(2) * dd / (log(inc_total_int[3 + dd] / inc_total_int[3]))
  
  # Growth rate calculation
  r <- log(2) / doub0
  
  # Basic reproduction number calculation
  Di <- mean(weighted_average_duration_i)
  De <- 1 / parameters["gamma"]
  
  R0_serial_sir <- 1 + r * Di
  R0_serial_seir <- (1 + r * Di) * (1 + r * De)
  
  # Final R0
  R0_serial <- R0_serial_seir
  
  # Herd Immunity Threshold (HIT) and Attack Rate
  HIT <- (1 - (1 / R0_serial))
  Attack <- sum(inf_age_groups) / sum(popstruc)
  
  summary <- data.frame("R0" = round(R0_calc,3), "IFR" = round(ifr,3), "HIT" = round(HIT, 3), "Attach" = round(Attack, 3), "R0-serial" = round(R0_serial,2))
  results_summary[[n]] <- summary
  
}

# Create a new workbook
wb_sum <- createWorkbook()
addWorksheet(wb_sum, "SummaryMetrics")

current_row <- 1
directory <- result_store

# Write the header for the summary metrics
writeData(wb_sum, sheet = "SummaryMetrics", results_summary[[1]], startRow = current_row, colNames = TRUE)

# Update the row for the next entries
current_row <- current_row + nrow(results_summary[[1]])

for (m in 1:(run_end - run_start+1)) {
  writeData(wb_sum, sheet = "SummaryMetrics", results_summary[[m]], startRow = current_row, colNames = FALSE)
  current_row <- current_row + nrow(results_summary[[m]])
}

file_name_sum <- paste0("Results ", file_name)
file_path_sum <- paste0(directory, file_name_sum)

# Save the workbook
saveWorkbook(wb_sum, file_path_sum, overwrite = TRUE)
