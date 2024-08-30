library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(grid)
library(gridExtra)
# Set working directory

########################################################################################

standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

#################################################################################### 
# Set working directory
#setwd("<dir/name.")
parameters_file <- "Master_SARS-CoV-2_Influenza_South Africa_United Kingdom_Bolivia_Lockdown_Shielding_Range0.3_Steps3_ModelRuns.xlsx"
results_file <- "./results/Results Master_SARS-CoV-2_Influenza_South Africa_United Kingdom_Bolivia_Lockdown_Shielding_Range0.3_Steps3_ModelRuns.xlsx"

parameters <- read_excel(parameters_file, sheet = "Runs")
results <- read_excel(results_file)


# Read demographic data
demog_data <- read_excel(parameters_file, sheet = "population", col_types = c("text"))

country_data_Uk <- filter(demog_data, country == "United Kingdom")
country_data_Bo <- filter(demog_data, country == "Bolivia (Plurinational State of)")
country_data_Sa <- filter(demog_data, country == "South Africa")

pop_Uk <- sum(as.numeric(country_data_Uk$population))
pop_Bo <- sum(as.numeric(country_data_Bo$population))
pop_Sa <- sum(as.numeric(country_data_Sa$population))
merged_data <- merge(parameters, results, by = "Index")


#create coloumn for population data
merged_data <- merged_data %>%
  mutate(Population = case_when(
    Country == "United Kingdom" ~ pop_Uk,
    Country == "Bolivia" ~ pop_Bo,
    Country == "South Africa" ~ pop_Sa,
    TRUE ~ NA_real_
  ))


  specific_countries <- c("Bolivia")
  specific_diseases <- c("Influenza", "SARS-CoV-2")  
  
  
  filtered_data <- merged_data %>%
    filter(Country %in% specific_countries & `Disease (age curve)` %in% specific_diseases)

  
  filtered_data <- merged_data %>%
    filter(p == 0.042 | p== 0.03)
  
  
  #filter results for the lockdown scenarios
  
  average_lock <- filtered_data %>%
    filter(`Lockdown Efficacy` != 0) %>%
    group_by(Country, `Disease (age curve)`, `Lockdown Adherence`, `Lockdown Efficacy`, `Lockdown.Effect`) %>%
    summarise(
      Avg_Deaths = round(mean((Total.Deaths / Population) * 100000, na.rm = TRUE), 0),
      SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
      CI_Lower = Avg_Deaths - 1.96 * SE_Deaths,
      CI_Upper = Avg_Deaths + 1.96 * SE_Deaths,
      IntegralAvg = mean(LockIntegral, na.rm = TRUE),
      SE_Integral = standard_error(LockIntegral),
      CI_Integral_Lower = IntegralAvg - 1.96 * SE_Integral,
      CI_Integral_Upper = IntegralAvg + 1.96 * SE_Integral
    ) %>%
    ungroup() %>%
    mutate(Intervention = "Lockdown")
  
  # Filter results for the shielding scenarios
  average_shielding <- filtered_data %>%
    filter(`Shielding Efficacy` != 0) %>%
    group_by(Country, `Disease (age curve)`, `Shielding Adherence`, `Shielding Efficacy`, `Shielding.Effect`) %>%
    summarise(
      Avg_Deaths = round(mean((Total.Deaths / Population) * 100000, na.rm = TRUE), 0),
      SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
      CI_Lower = Avg_Deaths - 1.96 * SE_Deaths,
      CI_Upper = Avg_Deaths + 1.96 * SE_Deaths,
      IntegralAvg = mean(ShieldingIntegral, na.rm = TRUE),
      SE_Integral = standard_error(ShieldingIntegral),
      CI_Integral_Lower = IntegralAvg - 1.96 * SE_Integral,
      CI_Integral_Upper = IntegralAvg + 1.96 * SE_Integral
    ) %>%
    ungroup() %>%
    mutate(Intervention = "Shielding")
  

  
  
  
  
  # rename and merge data frames
  average_shielding <- average_shielding %>%
    rename(`Adherence` = `Shielding Adherence`)
  average_lock <- average_lock %>%
    rename(`Adherence` = `Lockdown Adherence`)
  
  average_shielding <- average_shielding %>%
    rename(`Efficacy` = `Shielding Efficacy`)
  average_lock <- average_lock %>%
    rename(`Efficacy` = `Lockdown Efficacy`)
  
  average_shielding <- average_shielding %>%
    rename(`Effect` = `Shielding.Effect`)
  average_lock <- average_lock %>%
    rename(`Effect` = `Lockdown.Effect`)
  
  
  # final data frame for plot
  average_deaths <- bind_rows(average_lock, average_shielding)
  
  #rename disease type after applying variation from the baseline scenario
    average_deaths <- average_deaths %>%
    mutate(`Disease (age curve)` = recode(`Disease (age curve)`, "Influenza" = "Influenza-like Disease"))
  
  average_deaths <- average_deaths %>%
    mutate(`Disease (age curve)` = recode(`Disease (age curve)`, "SARS-CoV-2" = "SARS-CoV-2-like Disease"))
  
 
  # filter relevant values for intervention configurations
  average_deaths <- average_deaths %>% 
    filter(Adherence > 0.4)
  
  average_deaths <- average_deaths %>% 
    filter(Efficacy == 0.8 | Efficacy == 0.7 |Efficacy == 0.9)
  
  
  average_deaths <- average_deaths %>%
    rename(`Effective Coverage` = `Intervention`)
  
  
  
#### Visualization
  
  red_shades <- c("#fcbba1", "#fca488", "#fc8660", "#fb6a4a", "#f14c3b", "#ef3b2c", "#da251e", "#cb181d", "#a50f15")
  blue_shades <- c("#c6dbef", "#b0d1e9", "#9ecae1", "#85b5d7", "#6baed6", "#5a9bc9", "#4292c6", "#2979b0", "#2171b5")
  
  custom_colors <- c(
    blue_shades[1], red_shades[1],
    blue_shades[2], red_shades[2],
    blue_shades[3], red_shades[3],
    blue_shades[4], red_shades[4],
    blue_shades[5], red_shades[5],
    blue_shades[6], red_shades[6],
    blue_shades[7], red_shades[7],
    blue_shades[8], red_shades[8],
    blue_shades[9], red_shades[9]
  )
  
  
  ### Total deaths per 100.000 Population
  p_deaths <- ggplot(average_deaths, aes(x = Adherence, y = Avg_Deaths, color = interaction(`Effective Coverage`, Efficacy))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = Avg_Deaths - 1.96 * SE_Deaths, ymax = Avg_Deaths + 1.96 * SE_Deaths), alpha = 0.01) +
    labs(
      x = "Intervention Adherence", 
      y = expression("Average Deaths per 100000 Population (log"[10]*")"),
      color = "Effective Coverage"
    ) + 
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 14),        
      legend.title = element_text(size = 16),         
      axis.title.x = element_text(size = 16),         
      axis.title.y = element_text(size = 16),         
      axis.text.x = element_text(size = 14),          
      axis.text.y = element_text(size = 14),          
      strip.text = element_text(face = "bold", size = 14)  
    ) +
    facet_wrap(~`Disease (age curve)` + Country, scales = "free") +
    scale_color_manual(values = custom_colors)
  
  
  png("Deaths.png", width = 30, height = 25, units = "cm", res = 600)
  print(p_deaths)
  dev.off()
  
  

  
  # total contact reduction
  p_contact_reduc <- ggplot(average_deaths, aes(x = Adherence, y = Effect, color = interaction(`Effective Coverage`, Efficacy))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(x = "Intervention Adherence", 
         y = "Effective Contact Reduction in Total Population [%]",
         color = "Effective Coverage") + 
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 14),          
      legend.title = element_text(size = 16),         
      axis.title.x = element_text(size = 16),         
      axis.title.y = element_text(size = 16),         
      axis.text.x = element_text(size = 14),        
      axis.text.y = element_text(size = 14),          
      strip.text = element_text(face = "bold", size = 14) 
    ) +
    facet_wrap(~`Disease (age curve)` + Country, scales = "free") +
    scale_color_manual(values = custom_colors)
  

  png("ContactReduc.png", width = 30, height = 25, units = "cm", res = 600)
  print(p_contact_reduc)
  dev.off()
  
  
  
  # integral of contact reduction
  p_contact_reduc_integral <- ggplot(average_deaths, aes(x = Adherence, y = IntegralAvg, color = interaction(`Effective Coverage`, Efficacy))) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    #geom_ribbon(aes(ymin = IntegralAvg - 1.96 * IntegralAvg, ymax = IntegralAvg + 1.96 * SE_Integral), alpha= 0.01) +
    labs(x = "Intervention Adherence", 
         y = "Integral of Effective Contact Reduction \n in Total Population [% * days]",
         color = "Effective Coverage") +  
    theme_minimal() +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 14),          
      legend.title = element_text(size = 16),         
      axis.title.x = element_text(size = 16),         
      axis.title.y = element_text(size = 16),         
      axis.text.x = element_text(size = 14),          
      axis.text.y = element_text(size = 14),        
      strip.text = element_text(face = "bold", size = 14)  
    ) +
    facet_wrap(~`Disease (age curve)` + Country, scales = "free") +
    scale_color_manual(values = custom_colors)
  

  
  png("ContactReduc.png", width = 30, height = 25, units = "cm", res = 900)
  print(p_contact_reduc)
  dev.off()
  
  png("ContactReducInt.png", width = 30, height = 25, units = "cm", res = 900)
  print(p_contact_reduc_integral)
  dev.off()
  
  png("TotalDeaths.png", width = 30, height = 25, units = "cm", res = 900)
  print(p_deaths)
  dev.off()
  
  
  
  
  