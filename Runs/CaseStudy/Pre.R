### Libraries

require("deSolve")
library(deSolve)
library(readxl)
library(viridis)
library(dplyr)
library(readxl)
library(openxlsx)


#Working Directory - File will be saved here
#setwd("<dir/name.")

### Functions

##function creates additional values deviating from the calibration values of the disease
# with a range of +/- percent and the desired number of steps
# returns the vector for the variable to use in the scenarios
range_vector <- function(parameter, range, steps) {
  par_up <- parameter + parameter * range
  par_low <- parameter - parameter * range
  return(seq(par_low, par_up, length.out = steps))
}

createGrid <- function(disease, country, intervention) {
  disease_params <- read_excel("DiseaseParams.xlsx", sheet = disease, col_types = c("text"))
  

  
  #not varied
  gamma <- as.numeric(disease_params$gamma)
  nua <- as.numeric(disease_params$nua)
  rhoa <- as.numeric(disease_params$rhoa)
  rho <- as.numeric(disease_params$rho)
  pc <- as.numeric(disease_params$pc)
  # included for future but has a really high value
  tau <- as.numeric(disease_params$tau)
  
                   
  #varied
  rhoh <- range_vector(as.numeric(disease_params$rhoh), range, steps)
  nuc <- range_vector(as.numeric(disease_params$nuc), range, steps)
  nuh <- range_vector(as.numeric(disease_params$nuh), range, steps)
  p <- range_vector(as.numeric(disease_params$p), range, steps)

  
  shielding_threshold <- 0.05
  lockdown_threshold <- 0.1
  
  if (intervention == "Shielding") {
    shielding_efficacy <- seq(0,0.9, by=0.1)
    shielding_adherence <- seq(0,1, by=0.1)
    
  } else {
    shielding_efficacy <- 0
    shieling_duration <- 0
    shielding_adherence <- 0
  }
  
  if (intervention == "Lockdown") {
    lockdown_duration <- 90
    lockdown_efficacy <- seq(0,0.9, by=0.1)
    lockdown_lag <- 7
    lockdown_adherence <- seq(0,1, by=0.1)
    
  } else {
    lockdown_duration <- 0
    lockdown_efficacy <- 0
    lockdown_lag <- 0
    lockdown_adherence <- 0
  }
  
  if (intervention == "None") {
    lockdown_duration <- 0
    lockdown_efficacy <- 0
    shielding_efficacy <- 0
    shielding_duration <- 0
  }
  
  par_tb <- expand.grid(list(disease, country, p, tau, gamma, pc, rho, nuc, rhoh, nuh, rhoa, nua, shielding_efficacy, shielding_adherence, shielding_threshold, lockdown_duration, lockdown_efficacy, lockdown_lag, lockdown_threshold, lockdown_adherence))
  
  # Add index column directly
  start_index <- if (nrow(cumulative_results) > 0) max(cumulative_results$Index) else 0
  par_tb$Index <- seq(start_index + 1, nrow(par_tb) + start_index, by = 1)
  par_tb <- par_tb[, c(ncol(par_tb), 1:(ncol(par_tb) - 1))]
  
  colnames(par_tb) <- c("Index", "Disease (age curve)", "Country", "p", "tau", "gamma", "pc", "rho", "nuc", "rhoh", "nuh", "rhoa", "nua", "Shielding Efficacy", "Shielding Adherence", "Prev. Threshold","Lockdown Duration", "Lockdown Efficacy", "Lockdown Lag", "Hosp. Threshold", "Lockdown Adherence")
  
  # Append results to the cumulative dataframe
  cumulative_results <<- rbind(cumulative_results, par_tb)
}



#Results frame
cumulative_results <- data.frame()

# All possible combinations (can be adjusted)
# Grid is created
disease <- c("SARS-CoV-2", "Influenza")
country <- c("South Africa", "United Kingdom", "Bolivia")
intervention <- c("Shielding", "Lockdown")


# Setting the variables for the grid
# Input for the range_vector function
range <- 0.3
steps <- 3


#For loop creating the grid
for (i in disease) {
  for (j in country) {
    for (k in intervention) {
      createGrid(i, j, k)
    }
  }
}

# File Name
disease_str <- paste(disease, collapse = "_")
country_str <- paste(country, collapse = "_")
intervention_str <- paste(intervention, collapse = "_")
file_name <- paste0("Master_", disease_str, "_", country_str, "_", intervention_str, "_Range", range, "_Steps", steps, "_ModelRuns.xlsx")


# Saving the pre-file
# Adding it to the rest of the data necessary for the simulation
existing_workbook <- loadWorkbook("ModelRuns.xlsx")
#print(filename)
addWorksheet(existing_workbook, "Runs")
writeData(existing_workbook, sheet = "Runs", cumulative_results)
saveWorkbook(existing_workbook, paste0(file_name), overwrite = TRUE)
