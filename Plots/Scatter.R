  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(viridis)
  library(grid)
  library(gridExtra)

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
  
  
  #add coloun for total popualtion for scaling of the parameters
  merged_data <- merged_data %>%
    mutate(Population = case_when(
      Country == "United Kingdom" ~ pop_Uk,
      Country == "Bolivia" ~ pop_Bo,
      Country == "South Africa" ~ pop_Sa,
      TRUE ~ NA_real_
    ))
  
  
  
  specific_countries <- c("United Kingdom", "South Africa", "Bolivia")
  specific_diseases <- c("Influenza", "SARS-CoV-2")  
  
  
  filtered_data <- merged_data %>%
    filter(Country %in% specific_countries & `Disease (age curve)` %in% specific_diseases)
  
  filtered_data <- merged_data %>%
    filter(p == 0.042 | p== 0.03)
  
  
  
  
  # Filter results for the lockdown scenarios
  average_lock <- filtered_data %>%
    filter(`Lockdown Efficacy` != 0) %>%
    group_by(Country, `Disease (age curve)`, `Lockdown Adherence`, `Lockdown Efficacy`, `Lockdown.Effect`) %>%
    summarise(
      Avg_Deaths = round(mean((Total.Deaths / Population) * 100000, na.rm = TRUE), 0),
      SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
      LockIntegralAvg = mean(LockIntegral, na.rm = TRUE),
      Avg_YLL = round(mean((YLL / Total.Deaths), na.rm = TRUE), 0),
      SE_YLL = standard_error(YLL / Total.Deaths),
      CI_YLL_Lower = Avg_YLL - 1.96 * SE_YLL,
      CI_YLL_Upper = Avg_YLL + 1.96 * SE_YLL
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
      ShielIntegralAvg = mean(ShieldingIntegral, na.rm = TRUE),
      Avg_YLL = round(mean((YLL / Total.Deaths), na.rm = TRUE), 0),
      SE_YLL = standard_error(YLL / Total.Deaths),
      CI_YLL_Lower = Avg_YLL - 1.96 * SE_YLL,
      CI_YLL_Upper = Avg_YLL + 1.96 * SE_YLL
    ) %>%
    ungroup() %>%
    mutate(Intervention = "Shielding")
  
  
  #combined data frame
  average_deaths <- bind_rows(average_lock, average_shielding)
  
  
  
  #filter relevant combinations
  average_deaths <- average_deaths %>% 
    filter(average_deaths$`Shielding Adherence` == 0.9 | average_deaths$`Lockdown Adherence` == 0.7 | average_deaths$`Lockdown Adherence` == 0.8 | average_deaths$`Lockdown Adherence` == 0.9)
  
  average_deaths <- average_deaths %>% 
    filter(average_deaths$`Shielding Efficacy` == 0.9 | average_deaths$`Lockdown Efficacy` == 0.8)
  
  
  #rearrangements for plotting
  intervention_colors <- c("Lockdown" = "#0072B2", "Shielding" = "#ef3b2c")
  average_deaths$`Disease (age curve)`[average_deaths$`Disease (age curve)` == "Influenza"] <- "Influenza-like"
  average_deaths$`Disease (age curve)`[average_deaths$`Disease (age curve)`  == "SARS-CoV-2"] <- "SARS-CoV-2-like"
  disease_colors <- c("Influenza-like" ="#c6dbef", "SARS-CoV-2-like" = "#fcbba1")
  
  
  # rename coloumns for plotting data frame (merge adherences)
  average_deaths <- average_deaths %>%
    mutate(Adherence = case_when(
      !is.na(`Shielding Adherence`) ~ `Shielding Adherence`,
      !is.na(`Lockdown Adherence`) ~ `Lockdown Adherence`
    )) %>%
    select(-`Shielding Adherence`, -`Lockdown Adherence`)  
  
  
  
  scatter <- ggplot(average_deaths, 
                    aes(x = Avg_Deaths, 
                        y = Avg_YLL, 
                        color = Intervention, 
                        shape = Country, 
                        fill = `Disease (age curve)`, 
                        alpha = `Disease (age curve)`,
                        size = Adherence)) +  
    geom_point(stroke = 0.2) +  
    scale_color_manual(values = intervention_colors) + 
    scale_fill_manual(values = disease_colors) + 
    scale_alpha_manual(values = c(0.5, 1)) +  
    scale_size_continuous(range = c(3, 8),
                          breaks = c(0.7, 0.8, 0.9),  
                          labels = c("0.7", "0.8", "0.9")) + 
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = expression("Average Deaths per 100000 Population (log"[10]*")"), 
         y = "Years-Life-Lost per Death",
         color = "Intervention",
         shape = "Country",
         fill = "Disease",
         alpha = "Disease",
         size = "Adherence") +  # Label the size legend as "Adherence"
    theme_minimal(base_size = 20) +  # Increase the base font size for all text elements
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 20),  
      legend.text = element_text(size = 18),  
      legend.key.size = unit(1.5, "lines"),  
      axis.title = element_text(size = 20), 
      axis.text = element_text(size = 20), 
      panel.grid.major = element_line(color = "gray90"),  
      panel.grid.minor = element_blank(),  
      panel.background = element_rect(fill = "white", color = NA),  
      plot.background = element_rect(fill = "white", color = NA),
      axis.title.x = element_text(size = 22),         
      axis.title.y = element_text(size = 22),         
      axis.text.x = element_text(size = 22),          
      axis.text.y = element_text(size = 22),
      legend.box = "horizontal",  
      legend.spacing.y = unit(0.4, "cm"),  
      legend.spacing.x = unit(0.7, "cm") 
    ) +
    guides(
      color = guide_legend(nrow = 1, title.position = "top", override.aes = list(size = 6)),  
      shape = guide_legend(nrow = 1, title.position = "top", override.aes = list(size = 6)),  
      fill = guide_legend(nrow = 1, title.position = "top", override.aes = list(size = 6)),  
      alpha = guide_legend(nrow = 1, title.position = "top"), 
      size = guide_legend(nrow = 1, title.position = "top")  
    ) +
    scale_x_log10()
  
  scatter
  
  ggsave(filename = "Scatter.png", plot = scatter, width = 50, height = 35, units = "cm", dpi = 600)
  
  tiff("Scatter.tiff", width = 45, height = 30, units = "cm", res = 600)
  print(scatter)
  dev.off()