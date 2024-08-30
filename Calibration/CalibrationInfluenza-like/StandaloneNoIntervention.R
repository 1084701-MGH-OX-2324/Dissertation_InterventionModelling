
  
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
  
  
  ##### Since the actual SIM.R is commented, comments here are not as detailed
  
  ###############
  # Data and WD
  
  #setwd("<dir/name.")
  file <- "Influenza_FinalParams.xlsx"


  
  ##################
  # Functions
  ##################
  
  ### SEIR Model Function
  disease_int <- function(t, Y, parameters) {
    with(as.list(c(Y, parameters)),
         { # Index for each age group
           S <- Y[Sindex]
           E <- Y[Eindex]
           C <- Y[Cindex]
           H <- Y[Hindex]
           A <- Y[Aindex]
           R <- Y[Rindex]
           D <- Y[Dindex]
           
           contacts <- contact_home + contact_other + contact_school + contact_work
           P <- (S + E + C + R + H + A)
           
           
           #force of infection
           lam <- p * contacts %*% ((rhoa * A + rho * E + C + rhoh * H) / P)
           
           #average force of infection over all age groups - used to calculate the integral of force of infection
           lamda_avg <- mean(lam * sum(popstruc))
           #print(lamda_avg)
           lambda_list <<- append(lambda_list, lamda_avg)
           # print(dim(lam))
           
           
           
           ### ODE - System
           dSdt <- -S * lam + tau * R + ageing %*% S
           dEdt <- S * lam - gamma * E + ageing %*% E
           
           
           dCdt <- ageing %*% C + gamma * pc * (1-ihr) * E - C * nuc
           
           dHdt <- ageing %*% H + gamma * ihr * E - H * nuh
           
           dAdt <- ageing %*% A + (1-pc)*(1-ihr) * gamma * E - nua*A
           
           
           dDdt <- ageing %*% D + nuc * cfr * C + nuh * hfr * H
           
           
           dRdt <- ageing %*% R + nua * A + (1-cfr) * nuc * C  + nuh * H * (1-hfr) - tau*R
           
           
           
           
           # return values per time step t
           list(c(dSdt, dEdt, dCdt, dHdt, dAdt, dRdt, dDdt))
         }
    )
  }
  # Years of Life Lost (YLL) Calculation Function
  yll <- function(scenario, country) {
    # Load life expectancy data
    life_expec <- read_excel(file, sheet = "lifeexpec")
    
    # Find the column corresponding to the specified country
    col_names <- colnames(life_expec)
    country_col <- which(grepl(country, col_names))
    
    # Extract life expectancy for the specified country
    expec_country <- life_expec[, country_col]
    
    # Calculate YLL for each age group
    yll_age <- scenario * expec_country
    
    # Return the YLL per age group
    return(yll_age)
  }
  
  
  ### bar plots to compare values for age groups
  barplot_ages <- function(data, metric, disease, country){
    
    data_long <- melt(data, id.vars = "Age")
    
    options(repr.plot.width=10, repr.plot.height=6)
    
    y_axis_name <- paste0(metric, " - Value per Age Group")
    title <- paste0(disease, " in ", country)
    ggplot(data_long, aes(x = Age, y = value, fill = variable)) + 
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(x = "Age Group Floor", y = y_axis_name, fill = "Scenario") +
      ggtitle(title) +
      theme_minimal() +  
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = "none")
  }
  
  ##############################################################
  
  
  #START
  
 
  results_summary <- list()
  working_directory <- getwd()
  disease <- "Influenza"
  country <- "United Kingdom"
  intervention <- "None"
  
  ##read in master file
  #this is the big difference, its only ran for one simulation run
  run <- read_excel(file, sheet="Runs")
  #indicate the cases you want to simulate
  run_start <- 1
  run_end <- 1
  case <- 1
  
  working_directory <- getwd()
  
  # define the results directory within the working directory
  result_store <- file.path(working_directory, "results/.")
  
  
  if (!dir.exists(result_store)) {
    dir.create(result_store)
  }
  
  
  
  # Simulation starts
  ##########################################################
  
  # Select the scenario to run based on the index
  row_index <- which(run$Index == run_start)
  scenario <- run[row_index, ]
  
  # Load contact matrices
  load("contacts.RDa")
  
  # Load demographic data
  demog_data <- read_excel(file, sheet = "population", col_types = c("text"))
  
  # Find the matching country name in the demographic data
  country_names <- demog_data$country
  matching_un_name_char <- country_names[str_detect(country_names, country)]
  
  # Filter the demographic data for the selected country
  country_data <- filter(demog_data, country == matching_un_name_char)
  popstruc <- as.numeric(country_data$population)
  A <- length(popstruc) # Number of age groups
  
  # Read in contact matrices
  disease <- toString(scenario$`Disease (age curve)`)
  ratios <- read_excel(file, sheet = disease)
  
  # Extract ratios for infection, hospitalization, and case fatality
  ihr <- ratios$ihr
  hfr <- ratios$hfr
  cfr <- ratios$cfr
  
  ### Lockdown Threshold
  
  # Load hospital data
  hospital_data <- read_excel(file, sheet = "hospitalbeds", col_types = c("text"))
  
  # Extract hospital bed capacity and threshold
  population_numeric <- as.numeric(country_data$population)
  col_names_hosp <- colnames(hospital_data)
  country_col_hops <- which(grepl(country, col_names_hosp))
  
  tot_pop <- sum(population_numeric)
  hospital_beds_capacity <- as.numeric(hospital_data[1, country])
  threshold <- as.numeric(hospital_data[2, country])
  
  hospital_beds_threshold <- 0
  lockdown_lag <- 0
  shielding_effect_age <- ratios$Shielding
  
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
  
  
  # filling in 4 higher age groups 75-80, 80-85, 85-90, 95-100, 100+
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
  # average contacts per day from POLYMOD matrices 
  c <- sum((contact_home + contact_other + contact_school + contact_work) %*%
             (popstruc / sum(popstruc)))
  
  
  
  
  
  
  # Per year ageing matrix
  dd <- seq(1:A) / seq(1:A)
  # Create an ageing matrix that accounts for age groups moving up due to ageing
  ageing <- t(diff(diag(dd), lag = 1) / (5 * 365.25))
  ageing <- cbind(ageing, rep(0, A)) # No ageing from the last compartment
  
  # Population and index case initialization
  initP <- sum(popstruc) # Total population size
  ageindcase <- 20 # Age of the index case (years)
  aci <- floor((ageindcase / 5) + 1) # Age class of the index case
  
  # Define simulation time frame
  startdate <- as.Date("2020-01-01")
  stopdate <- as.Date("2020-12-31")
  day_start <- as.numeric(startdate - startdate)
  day_stop <- as.numeric(stopdate - startdate)
  times <- seq(day_start + 1, day_stop)
  
  # Initialize compartments
  initE <- rep(0, A) # Incubating
  initE[aci] <- 1 # Place the index case in the E compartment
  
  initC <- rep(0, A) # Infected and symptomatic
  initH <- rep(0, A) # Hospitalized 
  initA <- rep(0, A) # Asymptomatic
  initR <- rep(0, A) # Immune
  initD <- rep(0, A) # Died
  
  initS <- popstruc - initE - initC - initR - initH - initA - initD # Susceptible (non-immune)
  
  # Initial conditions for the main solution vector
  Y <- c(initS, initE, initC, initH, initA, initR, initD)
  
  
  
  Sindex <- 1 : A
  Eindex <- (A + 1) : (2 * A)
  Cindex <- (2 * A + 1) : (3 * A)
  Hindex <- (3 * A + 1) : (4 * A)
  Aindex <- (4 * A + 1) : (5 * A)
  Rindex <- (5 * A + 1) : (6 * A)
  Dindex <- (6 * A + 1) : (7 * A)
  
  
  lock <<- 0
  lockdown_start <<- 10^8
  
  parameters <- c(
    
    # General
    p = run[case,]$p,   # probabilty of infection given a contact
    tau = run[case,]$tau, # rate of loss of immunity = 1/(average duration of immunity)
    #equal for all the compartments
    
    # C-compartent
    gamma = run[case,]$gamma, # rate of incubation to unfectious stage, E to C    
    pc = run[case,]$pc,  # proportion of clinical cases
    # pc = 0.8, 
    
    rho = run[case,]$rho, # infectiousness of clincal cases (lamdba)
    nuc = run[case,]$nuc,  # rate of recovery     
    
    # H-Compartment
    rhoh = run[case,]$rhoh, # infectiousness of hospital (lamdba)
    nuh = run[case,]$nuh,  # rate of recovery 
    
    
    #### asymptomatic infections
    rhoa = run[case,]$rhoa, #relative infectiousness*contacts of asymtomatic
    nua = run[case,]$nua    #recovery after asymptomatc infection
  
    
  )
  
  out <- ode(y = Y, times = times, func = disease_int, parms =   parameters, method = euler)
  
  
  
  # Deaths
  deaths_age_groups <- t(tail(out[, (Dindex + 1)], 1))
  colnames(deaths_age_groups) <- "Deaths"
  print(deaths_age_groups)
  
  # Incidence
  inc_total_int <- parameters["gamma"] * out[, (Eindex + 1)]
  dailyinc_int <- rowSums(inc_total_int)
  plot(dailyinc_int)
  
  deaths_age_groups <- t(tail(out[, (Dindex + 1)], 1))
  
  # Population per age group (remaining)
  pop_int_nat <- out[, (Sindex + 1)] + out[, (Eindex + 1)] + out[, (Cindex + 1)] + out[, (Rindex + 1)] + out[, (Hindex + 1)] + out[, (Aindex + 1)] + out[, (Dindex + 1)]
  tpop_int_nat <- rowSums(pop_int_nat)
  time <- as.Date(out[, 1] + startdate)
  
  # Daily incidence
  inc_total_int_nat <- parameters["gamma"] * out[, (Eindex + 1)]
  daily_inc <- rowSums(inc_total_int_nat)
  plot(daily_inc)
  
  colnames(deaths_age_groups) <- "Deaths"
  
  # IFR and Hospitalizations
  inf_age_groups <- colSums(out[, (Eindex + 1)], 1) * parameters["gamma"]
  inf <- rowSums(out[, (Eindex + 1)], 1) * parameters["gamma"]
  ifr <- sum(deaths_age_groups) / sum(inf)
  ifr_age_groups <- deaths_age_groups / inf_age_groups
  
  hosps_day <- parameters["gamma"] * out[, (Hindex + 1)] * ihr
  daily_inc_hosps <- rowSums(hosps_day)
  plot(daily_inc_hosps)
  
  hosp_age_groups <- colSums(out[, (Eindex + 1)], 1) * parameters["gamma"] * ihr
  hosp_occ <- colSums(out[, (Hindex + 1)])
  
  ##### Create barplots
  Age <- c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", "85", "90", "95", "100")
  
  hosp_data <- data.frame("Age" = Age, "No_Intervention_Hospitalization" = hosp_age_groups)
  death_data <- data.frame("Age" = Age, "No_Intervention_Deaths" = deaths_age_groups)
  yll_data <- data.frame("Age" = Age, "No_Intervention_YLL" = yll(deaths_age_groups, country))
  ifr_data <- data.frame("Age" = Age, "IFR" = ifr_age_groups)
  
  barplot_ages(death_data, "Deaths", disease, country)
  barplot_ages(hosp_data, "Hospitalizations", disease, country)
  barplot_ages(yll_data, "YLL", disease, country)
  barplot_ages(ifr_data, "IFR", disease, country)
  
  hosps_age <- colSums(out[, (Eindex + 1)], 1) * parameters["gamma"] * ihr
  deaths_hosp <- colSums(out[, (Hindex + 1)], 1) * hfr * parameters["nuh"]
  hfr_model <- deaths_hosp / hosps_age
  hfr_model_tot <- sum(deaths_age_groups) / sum(hosps_age)
  
  # R0 Calculation
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
  
  fac_cases <- (parameters["pc"] * (1 - ihr)) / parameters["nuc"]
  fac_hosp <- ihr / parameters["nuh"]
  fac_asymp <- ((1 - parameters["pc"]) * (1 - ihr)) / parameters["nua"]
  beta <- parameters["p"] * c
  
  R0_calc <- beta * (fac_cases + fac_hosp + fac_asymp)
  R0_calc <- mean(R0_calc)
  
  # Herd Immunity Threshold (HIT) and Attack Rate
  HIT <- (1 - (1 / R0_serial))
  Attack <- sum(inf_age_groups) / sum(popstruc)
  
  summary <- data.frame(
    "R0" = round(R0_calc, 3),
    "IFR" = round(ifr, 3),
    "HIT" = round(HIT, 3),
    "Attack" = round(Attack, 3),
    "R0-serial" = round(R0_serial, 2)
  )
  
  # Print the summary results
  print(paste0("IFR: ", ifr))
  print(paste0("HIT: ", HIT))
  print(paste0("Final Epidemic Size: ", Attack))
  print(paste("The estimated R0 (serial) is:", round(R0_serial, 3)))
  print(paste("The estimated R0 (param) is:", round(R0_calc, 3)))
  
  plot(ifr_data)