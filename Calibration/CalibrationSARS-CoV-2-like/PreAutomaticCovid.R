require("deSolve") 
# Set working directory for saving files
#setwd("<dir/name.")

# Load necessary libraries
start.time <- Sys.time()

library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(readxl)
library(openxlsx)

# Function to create a range vector for a parameter
range_vector <- function(parameter, range, steps){
  par_up <- parameter + parameter * range
  par_low <- parameter - parameter * range
  return(seq(par_low, par_up, length.out = steps))
}

# Function to create a grid for the parameters based on disease, country, and intervention
createGrid <- function(disease, country, intervention){
  
  disease <- disease
  country <- country
  intervention <- intervention
  
  # Read disease-specific parameters
  disease_params <- read_excel("DiseaseParams.xlsx", sheet=disease, col_types = c("text"))
  
  # Define parameter ranges and values for grid creation
  p <- seq(0.035, 0.055, by = 0.01)
  
  tau <- as.numeric(disease_params$tau)
  
  # Incubation period in 1/days
  gamma <- seq(0.3, 0.25)
  
  # Proportion of cases  
  pc <- seq(0.52, 0.82, by = 0.1)
  
  # Relative infectiveness of cases
  rho <- c(0.5)
  
  # Recovery in 1/days for cases
  nuc <- c(0.11, 0.125, 0.14)
  
  # Relative infectiveness of hospitalized
  rhoh <- c(0.6, 0.70, 0.8)
  
  # Recovery in 1/days for hospitalized cases
  nuh <- c(0.1, 0.11, 0.083)
  
  # Relative infectiveness of asymptomatic cases
  rhoa <- c(0.7, 1.1, by = 0.1)
  
  # Recovery in 1/days for asymptomatics
  nua <- nuc
  
  # Reporting rate
  report <- as.numeric(disease_params$report)
  
  # Intervention-specific parameters
  if (intervention == "Shielding"){
    shielding_start <- 30
    shielding_effect <- 0.5
  } else {
    shielding_start <- c(0)
    shielding_effect <- c(0)
  }
  
  if (intervention == "Lockdown"){
    lockdown_duration <- 90
    lockdown_effect <- 0.5
  } else {
    lockdown_duration <- 0
    lockdown_effect <- 0
  }
  
  if (intervention == "None"){
    lockdown_duration <- c(0)
    lockdown_effect <- c(0)
    shielding_start <- c(0)
    shielding_effect <- c(0)
  }
  
  # Create a data frame with all combinations of the parameters
  par_tb <- expand.grid(list(disease, 
                             country, 
                             p,
                             tau,
                             gamma,
                             pc,
                             rho,
                             nuc,
                             rhoh,
                             nuh,
                             rhoa,
                             nua,
                             report,
                             shielding_start,
                             shielding_effect,
                             lockdown_duration,
                             lockdown_effect))
  
  folder_path <- wd
  
  # List all files in the folder
  all_files <- list.files(path = folder_path)
  
  file_exists <- any(grepl("_ModelRuns\\.xlsx$", all_files))
  
  if (file_exists){
    
    model_runs_files <- all_files[grepl("_ModelRuns.xlsx$", all_files)]
    
    file_info <- file.info(model_runs_files)
    most_recent_file <- model_runs_files[which.max(file_info$mtime)]
    
   
    index_last_file <- read_excel(most_recent_file, sheet = "Runs")
    
    
    start_index <- tail(index_last_file$Index, 1)
    
   
    par_tb$Index <- seq(start_index + 1, nrow(par_tb) + start_index, by = 1)
    par_tb <- par_tb[, c(ncol(par_tb), 1:(ncol(par_tb) - 1))]
  } else {
    par_tb$Index <- seq(1, nrow(par_tb), by = 1)
    par_tb <- par_tb[, c(ncol(par_tb), 1:(ncol(par_tb) - 1))]
  }
  
  # Specify column names for the grid data frame
  colnames(par_tb) <- c("Index", 
                        "Disease (age curve)", 
                        "Country", 
                        "p", 
                        "tau", 
                        "gamma", 
                        "pc", 
                        "rho", 
                        "nuc", 
                        "rhoh", 
                        "nuh", 
                        "rhoa", 
                        "nua", 
                        "report", 
                        "Shielding Start", 
                        "Shielding Effect", 
                        "Lockdown Duration", 
                        "Lockdown Effect")
  
  filename <- paste0(disease, "_", country, "_", intervention,"_ModelRuns")
  
  existing_workbook <- loadWorkbook("ModelRuns.xlsx")
  
  addWorksheet(existing_workbook, "Runs")
  writeData(existing_workbook, sheet = "Runs", par_tb)
  
  # Save the workbook
  saveWorkbook(existing_workbook, paste0(filename, ".xlsx"), overwrite = TRUE)
}

# Define parameters for grid creation
disease <- c("SARS-CoV-2")
country <- c("United Kingdom")
intervention <- c("None")

range <- 0
steps <- 0

# Loop through each combination of disease, country, and intervention to create grids
for (i in disease){
  for (j in country){
    for (k in intervention){
      createGrid(i, j, k)
    }
  }
}

# SAVE THE MASTER FILE
all_files <- list.files(path = wd)

model_runs_files <- c(all_files[grepl("_ModelRuns\\.xlsx$", all_files)])

# Arrange files after generation
file_info <- file.info(model_runs_files)
sorted_files <- c(model_runs_files[order(file_info$ctime)])
master_df <- data.frame()

for (file in sorted_files) {
  print(file)
  df <- read_excel(file, sheet = "Runs")
  
  master_df <- rbind(master_df, df)
}

# Sort the master dataframe by index
sorted_master_df <- master_df[order(master_df$Index), ]
disease_str <- paste(disease, collapse = "_")
country_str <- paste(country, collapse = "_")
intervention_str <- paste(intervention, collapse = "_")
file_name_sum <- paste0("Master_", disease_str, "_", country_str, "_", intervention_str, "_Range", range, "_Steps", steps, "_ModelRuns")

existing_workbook <- loadWorkbook("ModelRuns.xlsx")

# Add a new sheet within dataframe
addWorksheet(existing_workbook, "Runs")
writeData(existing_workbook, sheet = "Runs", sorted_master_df)

saveWorkbook(existing_workbook, paste0(file_name_sum, ".xlsx"), overwrite = TRUE)
