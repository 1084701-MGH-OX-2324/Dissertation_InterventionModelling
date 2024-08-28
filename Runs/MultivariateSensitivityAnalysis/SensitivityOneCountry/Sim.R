
###########Libraries
start.time <- Sys.time()

require("deSolve") 

library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(reshape2)
library(pracma)
library(openxlsx)


################

#setwd("<dir/name.")

# put in the desired files
# file_name is used for generating the output file name accordingly
# file is the actual pre processing data containing the scenarios
file_name <-("Master_SARS-CoV-2_Influenza_Bolivia_Lockdown_Shielding_None_Range0.3_Steps3_ModelRuns.xlsx")
file <- read_excel("Master_SARS-CoV-2_Influenza_Bolivia_Lockdown_Shielding_None_Range0.3_Steps3_ModelRuns.xlsx", sheet = "Runs")



##################
# Functions
##################



### SEIR MODEL

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
         
         
         
         
         
         ## Implementation Lockdown
         ## Trigger is hospital occupancy 
         
         
         ## generate lockdown start date
         if (sum(H) > hospital_beds_threshold && lockdown_efficacy != 0) {
           #print(sum(H))
           #print(hospital_beds_threshold)
           #print(lock)
           if (lock == 0) {
             lock <<- 1
             lockdown_start <<- t + lockdown_lag
             # print(paste0("Start", lockdown_start))
             if (length(times) - (lockdown_start + lockdown_duration >= 0)){
               #  print("save this duration")
               lockdown_duration_list <<- append(lockdown_duration_list, lockdown_duration)
             }
             else {
               duration <- abs( length(times) - (lockdown_start + lockdown_duration))
               lockdown_duration_list <<- append(lockdown_duration_list, lockdown_duration)
             }
           }
           else if (lock == 1 && t >= (lockdown_start + lockdown_duration) ){
             lock <<- 0
             #  print(paste0("LockdownEnde", t))
           }
         } else {
           if (t >= (lockdown_start + lockdown_duration)) {
             lockdown_start <<- 10^8
             #lockdown_end <<- 10^8
             lock <<- 0
           }
         }
         
         
         # adjust contact matrices to lockdown
         if (lockdown_efficacy != 0){
           if (t >= lockdown_start && t < (lockdown_start+lockdown_duration)){
             lockdown <- 1-(lockdown_efficacy * lockdown_adherence)
             contacts_lock <- contact_home + (lockdown * (contact_other + contact_school + contact_work))
             contacts <- contacts_lock
             lock_switch <- 1
           }
           else {
             lock_switch <- 0
           } 
         }
         else {
            lock_switch <- 0
          }
         
         
         
         ##shielding implementation
         #trigger the start date with daily prevalence
         
         if ((sum(C+H+A)/sum(popstruc)*100) > shielding_threshold) {
           if (shiel == 0 && shielding_efficacy != 0) {
             shiel <<- 1
             shielding_start <<- t
             #print(shielding_start)
           }
         }
         
         if (shiel == 1 && shiel_effec_time == 0 && (sum(C+H+A)/sum(popstruc)*100) <= 0.05){
           #print(t)
           shiel_effec_time <<- 1
           effec_time_stop_shiel <<- t
         }
         else (
           effec_time_stop_shiel <<- length(times)
         )
         
         
         #adjust contact matrices to shielding
         
         if (t >= shielding_start && t < (shielding_start+shielding_duration) && shielding_efficacy != 0 ){
           shielding <- 1 - (shielding_efficacy * shielding_coverage * shielding_adherence)
           shielding_diag <- diag(shielding)  
           contacts_shiel <- contacts %*% shielding_diag
           contacts <- contacts_shiel
           if (t < effec_time_stop_shiel){
             shiel_switch <- 1}
           else {
             shiel_switch <- 0
           }
         } else{
           shiel_switch <- 0
        }
         
         
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
         
         
         shielding_coverage_tot <- sum((popstruc / sum(popstruc)) * shielding_coverage)
         
         shielding_list_integral <<- append(shielding_list_integral, shielding_efficacy * shielding_coverage_tot * shielding_adherence * shiel_switch)
         
         lockdown_list_integral <<- append(lockdown_list_integral, lockdown_efficacy * lock_switch * lockdown_adherence)
         
         
         
         # return values per time step t
         list(c(dSdt, dEdt, dCdt, dHdt, dAdt, dRdt, dDdt))
       }
  )
}


# calculates the year life lost for one simulation run for all age groups
# it extracts the life exoactancy'
yll <- function(scenario, country){
  life_expec <- read_excel(file_name, sheet="lifeexpec")
  
  col_names <- colnames(life_expec)
  
  country_col <- which(grepl(country, col_names))
  
  expec_country <- life_expec[, country_col]
  yll_age <- scenario * expec_country
  return(yll_age)
}


##############################################################
#Simulation Set-Up

results_summary <- list()

working_directory <- getwd()

# results directory in the same path
result_store <- file.path(working_directory, "results")
if (!dir.exists(result_store)) {
  dir.create(result_store)
}


# define desired scenarios which should be modelled
run_start <- file$Index[1]
#run_end <- file$Index[1] + 10
run_end <- nrow(file)


#read in general data
# population sum
demog_data <- read_excel(file_name, sheet="population", col_types = c("text"))
# hospital beds per 10000
hospital_data <- read_excel(file_name, sheet="hospitalbeds", col_types = c("text"))

#time frame
startdate <- as.Date("2020-01-01")
stopdate <- as.Date("2020-12-31")
day_start <- as.numeric(startdate-startdate)
day_stop <- as.numeric(stopdate-startdate)
times <- seq(day_start+1, day_stop)

#################################################################################
#Simulation start for all scenarios n 


for (n in 1:(run_end-run_start+1)){
  
  print(paste0("This is run number: ", n))
  
  
  #extract scenario characteristics
  row_index <- which(file$Index == n)
  country <- file$Country[n]
  disease  <- file$`Disease (age curve)`[n]
  scenario <- file[row_index, ]
  
  #print(country)
  #print(disease)
  #print(scenario)
  
  
  ## sometime the country names in the data sets differ, this is to align them
  ## to account for other countries in the future as well 
  country_names <- demog_data$country
  matching_un_name_char <- country_names[str_detect(country_names, country)]
  country_data <- filter(demog_data, country == matching_un_name_char[1])
  
  #print(matching_un_name_char)
  #country_name <- scenario$Country
  #print(country_names)
  
  #population structure in age groups
  popstruc <- as.numeric(country_data$population)
  #number of age groups
  A <- length(popstruc)
  
  
  
  #read in disease ratios
  ratios <- read_excel(file_name, sheet=disease)
  ihr <- ratios$ihr
  hfr <- ratios$hfr
  cfr <- ratios$cfr
  
  
  
  ### Lockdown Values
  
  
  population_numeric <- as.numeric(country_data$population)
  
  #filter which country to extract the values for the hospital beds for from the given excel
  col_names_hosp <- colnames(hospital_data)
  country_col_hops <- which(grepl(country, col_names_hosp))
  tot_pop <- sum(population_numeric)
  countryx <- toString(country)
  hospital_beds_capacity <- as.numeric(hospital_data[1,country])
  
  #extract the threshold from the file 
  threshold <-file[n,]$'Hosp. Threshold'
  hospital_beds_threshold <- threshold * ((tot_pop/10000)*hospital_beds_capacity)
  lockdown_lag <- file[n,]$'Lockdown Lag'
  
  
  
  #Shielding Values
  
  shielding_coverage <- ratios$Shielding
  
  
  
  #Read in country sepcific contact matrices
  
  contacts <- load("contacts.Rda")
  
  
  #adjust the names because the differ for the UN data
  country_names <- names(contact_home)
  matching_un_name <- toString(country_names[str_detect(country_names, country)])
  
  #matching_entries <- as.matrix(contact_home[[matching_un_name]])
  #print(matching_un_name)
  
  
  c_home <- as.matrix(contact_home[[matching_un_name]])
  
  c_school <- as.matrix(contact_school[[matching_un_name]])
  
  c_work <- as.matrix(contact_work[[matching_un_name]])
  
  c_other <- as.matrix(contact_other[[matching_un_name]])
  
  #result_identical <- identical(c_home, matching_entries)  # Should return TRUE
  
  #value of age groups needed to be added
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
  
  
  
  
  # per year ageing matrix
  dd <- seq(1:A) / seq(1:A)
  #diag - diagonal matrix (1 on the main diagonal)
  #generally we move the ages groups up, accounting for ageing
  ageing <- t(diff(diag(dd), lag = 1) / (5 * 365.25))
  ageing <- cbind(ageing, 0 * seq(1:A)) # no ageing from last compartment
  
  
  initP <- sum(popstruc) # population size
  ageindcase <- 20 # age of index case (years)
  aci <- floor((ageindcase / 5) + 1) # age class of index case
  
  startdate <- as.Date("2020-01-01")
  stopdate <- as.Date("2020-12-31")
  day_start <- as.numeric(startdate-startdate)
  day_stop <- as.numeric(stopdate-startdate)
  times <- seq(day_start+1, day_stop)
  
  initE <- 0 * popstruc # Incubating
  initE[aci] <- 1 # place random index case in E compartment
  
  
  initC <- 0 * popstruc # Infected and symptomatic
  initH <- 0 * popstruc # hospitalised 
  initA <- 0 * popstruc # Asymptomatic
  
  initR <- 0 * popstruc # Immune
  
  
  initD <- 0 * popstruc # died 
  initI <- 0 * popstruc
  
  initS <- popstruc - initE - initC -
    initR - initH - initA - initD# Susceptible (non-immune)
  
  # initial conditions for the main solution vector
  Y <- c(initS, initE, initC, initH, initA, initR, initD)
  
  
  Sindex <- 1 : A
  Eindex <- (A + 1) : (2 * A)
  Cindex <- (2 * A + 1) : (3 * A)
  Hindex <- (3 * A + 1) : (4 * A)
  Aindex <- (4 * A + 1) : (5 * A)
  Rindex <- (5 * A + 1) : (6 * A)
  Dindex <- (6 * A + 1) : (7 * A)
  #Iindex <- (7 * A + 1) : (8 * A)
  
  #### Define trigger for the interventions as well as high starting dates, to shut them off
  lock <<- 0
  lockdown_start <<- 10^8
  shiel <<- 0
  shielding_start <<- 10^8
  lockdown_duration_list <<- list()
  lambda_list <<- list()
  shiel_effec_time <<- 0
  effec_time_stop_shiel <<- 0
  shielding_list_integral <<- list()
  lockdown_list_integral <<- list()
  
  
  ##virological parameters
  ## tau is included for future analysis but set to a high number within this framework
  
  parameters <- c(
    
    # General
    p = file[n,]$p,   # probability of infection given a contact
    tau = file[n,]$tau,  # rate of loss of immunity = 1/(average duration of immunity)
    #equal for all the compartments
    
    # E-compartment
    gamma = file[n,]$gamma, # rate of incubation to infectious stage, E to C,A,H    
    pc = file[n,]$pc,  # proportion of symptomatic infections
    rho = file[n,]$rho, # relative infectivity of exposed individuals 
    #(Infectivity of C-Compartment == 1)
    
    
    #C-Compartment
    nuc = file[n,]$nuc,  # rate of non-severe infectious period 1/days    
    
    # H-Compartment
    rhoh = file[n,]$rhoh, # relative infectivity of hospitalized infections
    nuh = file[n,]$nuh,  # rate of hospitalised infectious period 1/days 
    
    
    #### asymptomatic infections
    rhoa = file[n,]$rhoa, #relative infectivity of asymptomatic infections
    nua = file[n,]$nua,    #rate of asymptomatic infectious period 1/days 
    
    #Interventions
    
    #Lockdown
    lockdown_duration = file[n,]$`Lockdown Duration`,
    lockdown_efficacy = file[n,]$`Lockdown Efficacy`,
    lockdown_adherence = file[n,]$`Lockdown Adherence`,
    
    
    
    #Shielding 
    
    shielding_duration = length(times),
    shielding_efficacy = file[n,]$`Shielding Efficacy`,
    shielding_adherence = file[n,]$`Shielding Adherence`,
    shielding_threshold = file[n,]$'Prev. Threshold'
  )
  
  
  #print("Parameters exit")
  
  #solution of the seir model for the current run n 
  out <- ode(y = Y, times = times, func = disease_int, parms =   parameters, method = euler)
  
  
  
  #############################################################################
  ## Output Evaluation
  
  
  
  #Validation check for constant population
  pop_int_nat <- out[, (Sindex + 1)] + out[, (Eindex + 1)] + out[, (Cindex + 1)] + out[, (Rindex + 1)] + out[, (Hindex + 1)]+ out[, (Aindex + 1)] + out[, (Dindex + 1)]
  tpop_int_nat <- rowSums(pop_int_nat)
  time <- as.Date(out[, 1] + startdate)
  #print(sum(popstruc) - tpop_int_nat)
  
  
  
  
  ########
  ## Looking at data over time
  
  #incidence
  inc_total_int <- parameters["gamma"] * out[, (Eindex + 1)]
  #total incidence for all age groups
  inf <- rowSums(out[, (Eindex + 1)],1)*parameters["gamma"]
  total_infections <- sum(inf)
  
  
  #hospitalizations - occupation
  hospreq_int <- rowSums(out[, (Hindex + 1)]) # requirement for hospital beds
  #plot(hospreq_int)
  
  #deaths
  death_inc <- rowSums((parameters["nuc"]*cfr*out[,(Cindex+1)])+(parameters["nuh"]*hfr*out[,(Hindex+1)]))
  
  
  ## potential saving of every time step for every run
  ## deactivated due to high amounts of runs
  time_data <- data.frame("Time" = times, "Incidence Infections" = inf, "Occupancy Hospitals " = hospreq_int, "Incidence Deaths" = death_inc)
  
  
  
  ########
  ## Looking at data summed over age groups
  
  ## Deaths
  #total deaths or all the age groups at end of the simulation
  deaths_age_groups <- t(tail(out[, (Dindex + 1)],1))
  #print(deaths_age_groups)
  colnames(deaths_age_groups) <- "Deaths"
  total_deaths <- sum(deaths_age_groups)
  
  
  #Hospitalizations
  hosp_age_groups <- colSums(out[, (Eindex + 1)],1)*parameters["gamma"]*ihr
  total_hosp <- sum(hosp_age_groups)
  
  Age <- c("0", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", "85", "90", "95","100")
  
  age_data <- data.frame("Age" = Age, "deaths_age_groups" = deaths_age_groups, "Hospitalizations" = hosp_age_groups)
  
  
  
  ### Sum data for the entire run as one value for all age groups as one output
  
  ###Years life lost
  yll_age <- yll(deaths_age_groups, country)
  absolute_yll <- sum(yll_age)
  
  
  
  
  ######intervention output]
  
  
  ##Effectiveness for interventions
  
  # proportion the intervention is applied to in the population
  # for shielding its age dependent
  shielding_coverage_tot <- sum((popstruc / sum(popstruc)) * shielding_coverage)
  
  # sum lockdown duration
  lockdown_values_duration <- sapply(lockdown_duration_list, function(x) x[1])
  if (length(lockdown_values_duration) == 0) {
    total_sum_lockdown_duration <- 0
    num_of_activations <- 0 
  }
  else {
    total_sum_lockdown_duration <- sum(lockdown_values_duration)
    num_of_activations <- length(lockdown_values_duration)
  }
  
  #sum shielding duration
  if (file[n,]$`Shielding Efficacy` == 0 ){
    total_sum_effective_shielding_duration <- 0
  }
  else { 
    if (effec_time_stop_shiel != 0 & shiel == 1){
      total_sum_effective_shielding_duration <- effec_time_stop_shiel - shielding_start
    }
    else {
      total_sum_effective_shielding_duration = 0
    }
  }
  
  
  #effectiveness = efficacy * coverage * adherence 
  # coverage 1 for lockdown
  Total_Shielding_effect_Post <- file[n,]$`Shielding Efficacy` * shielding_coverage_tot * file[n,]$`Shielding Adherence`
  Total_Lockdown_effect_Post <- file[n,]$`Lockdown Efficacy` * file[n,]$`Lockdown Adherence`
  
  #generate person days covered as a policy relevant metric
  person_days_covered_shielding <-  Total_Shielding_effect_Post *  total_sum_effective_shielding_duration
  person_days_covered_lockdown <- Total_Lockdown_effect_Post * total_sum_lockdown_duration
  
  
  ##integral of force of infection
  force_of_infection <- sapply(lambda_list, function(x) x[1])
  time_values <- 1:length(times)
  foi_integral <- trapz(time_values, force_of_infection)
  
  ##integral of shielding effect
  shielding_effect_integral <- sapply(shielding_list_integral, function(x) x[1])
  shiel_integral <- trapz(time_values, shielding_effect_integral)
  
  lockdown_effect_integral <- sapply(lockdown_list_integral, function(x) x[1])
  lock_integral <- trapz(time_values, lockdown_effect_integral)
  
  
  
  ##generate summary data frame
  summary <- data.frame("Index" = file$Index[n], "Total Infections" = round(total_infections), 
                        "Total Hosp" = round(total_hosp), "Total Deaths" = round(total_deaths), 
                        "YLL" = round(absolute_yll),  
                        "Lockdown Duration Total" = total_sum_lockdown_duration, 
                        "No of Lockdown Activations" = num_of_activations,
                        "Lockdown Effect" = Total_Lockdown_effect_Post,
                        "Person Days Covered Lockdown" = person_days_covered_lockdown,
                        "Shielding Duration Effective Total" = total_sum_effective_shielding_duration, 
                        "Coverage Shielding" = shielding_coverage_tot,
                        "Shielding Effect" = Total_Shielding_effect_Post,
                        "Person Days Covered Shielding" = person_days_covered_shielding,
                        "Integral Force of Infection" = foi_integral,
                        "ShieldingIntegral" = shiel_integral,
                        "LockIntegral" = lock_integral
  )
  #print(disease)
  
  results_summary[[n]] <- summary
  
}

################################################################################
## Save the results


# New workbook
wb_sum <- createWorkbook()
addWorksheet(wb_sum, "SummaryMetrics")

current_row <- 1
directory <- result_store

#header
writeData(wb_sum, sheet = "SummaryMetrics", results_summary[[1]], startRow = current_row, colNames = TRUE)

#update row
current_row <- current_row + nrow(results_summary[[1]])
run_end <- length(results_summary)
for (m in 1:(run_end - run_start+1)) {
  writeData(wb_sum, sheet = "SummaryMetrics", results_summary[[m]], startRow = current_row, colNames = FALSE)
  
  current_row <- current_row + nrow(results_summary[[m]])
}

file_name_sum <- paste0("Results ", file_name)
file_path_sum <- file.path(directory, file_name_sum)

saveWorkbook(wb_sum, file_path_sum, overwrite = FALSE)

