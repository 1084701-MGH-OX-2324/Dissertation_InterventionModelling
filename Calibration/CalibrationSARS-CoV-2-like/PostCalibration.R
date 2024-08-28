# Load necessary libraries
library(dplyr)
library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)
library(ggplot2)
library(reshape2)


#setwd("<dir/name.")


# Define file paths
parameters_file <- "Master_SARS-CoV-2_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx"
results_file <- "./results/.Results Master_SARS-CoV-2_United Kingdom_None_Range0_Steps0_ModelRuns.xlsx"

parameters <- read_excel(parameters_file, sheet = "Runs")
results <- read_excel(results_file)

merged_data <- merge(parameters, results, by = "Index")
merged_data <- merged_data %>%
  filter (rhoa != 0.1)


filtered_df <- subset(merged_data, R0.serial > 2 & R0.serial < 4)
filtered_df <- subset(filtered_df, IFR > 0.009 & IFR < 0.015)
filtered_df <- subset(filtered_df, Attach > 0.8 & Attach < 0.95)
filtered_df <- subset(filtered_df, p > 0.04 & p < 0.09)



print(filtered_df)




library(dplyr)

# Define your target values for SARS-CoV-2-like diseases
target_values <- data.frame(
  #R0 = 3,
  R0_serial = 3,
  IFR = 0.0100,
  Attach = 0.85
)

# Calculate proximity measures for each target value
merged_data <- merged_data %>%
  mutate(
    proximity_R0 = abs(R0.serial - target_values$R0_serial),
    #proximity_R0_serial = abs(R0_serial - target_values$R0_serial),
    proximity_IFR = abs(IFR - target_values$IFR),
    proximity_Attach = abs(Attach - target_values$Attach),
    #proximity_HIT = abs(HIT - target_values$HIT)
  )

# Calculate total proximity (sum of individual proximities)
merged_data <- merged_data %>%
  mutate(
    total_proximity = proximity_IFR  + proximity_Attach + proximity_R0
  )

# Sort data by total proximity s
filtered_data <- merged_data %>%
  arrange(total_proximity) %>%
  slice(1)  # Select the row with the smallest total proximity


filtered_data
