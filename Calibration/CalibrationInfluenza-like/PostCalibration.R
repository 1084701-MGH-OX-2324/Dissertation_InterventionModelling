setwd("C:/Users/mschu/OneDrive - Nexus365/Placement/ModelScratch/ALL/CalibrationFlu") 

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


# Load necessary libraries
library(dplyr)

# Define file paths
parameters_file <- "Master_Influenza_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx"
results_file <- "./results/.Results Master_Influenza_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx"

parameters <- read_excel(parameters_file, sheet = "Runs")
results <- read_excel(results_file)


#filter potential parameter combinations
merged_data <- merge(parameters, results, by = "Index")

filtered_df <- subset(merged_data, R0.serial > 1 & R0.serial < 2)
filtered_df <- subset(filtered_df, IFR > 0.0009 & IFR < 0.0012)
filtered_df <- subset(filtered_df, Attach > 0.4 & Attach < 0.6)
filtered_df <- subset(filtered_df, p > 0.02 & p < 0.04)



print(filtered_df)
library(dplyr)

# Define target values for the disease (Influenza-like)
target_values <- data.frame(
  #R0 = 1.5,
  R0.serial = 1.2,
  IFR = 0.00100,
  Attach = 0.6,
  HIT = 0.35
)

# Calculate proximity measures for each target value
merged_data <- merged_data %>%
  mutate(
    #proximity_R0 = abs(R0 - target_values$R0),
    proximity_R0_serial = abs(R0.serial - target_values$R0.serial),
    proximity_IFR = abs(IFR - target_values$IFR),
    proximity_Attach = abs(Attach - target_values$Attach),
    proximity_HIT = abs(HIT - target_values$HIT)
  )

# Calculate total proximity (sum of individual proximities)
merged_data <- merged_data %>%
  mutate(
    total_proximity = proximity_IFR + proximity_Attach + proximity_HIT + proximity_R0_serial 
  )

# Sort data by total proximity and select the top row
filtered_data <- merged_data %>%
  arrange(total_proximity) %>%
  slice(1)  # Select the row with the smallest total proximity

print(filtered_data)