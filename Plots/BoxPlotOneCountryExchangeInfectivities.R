library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggsci)



####
# less comments made since procedure is similar to BoxPlotOneCountry.R
###


# Set working directory
#setwd("<dir/name.")
parameters_file <- "Master_SARS-CoV-2_Influenza_Bolivia_Shielding_Lockdown_None_Range0.3_Steps3_ModelRuns.xlsx"
results_file <- "./results/Results Master_SARS-CoV-2_Influenza_Bolivia_Shielding_Lockdown_None_Range0.3_Steps3_ModelRuns.xlsx"

parameters <- read_excel(parameters_file, sheet = "Runs")
results <- read_excel(results_file)


filter_country <- "Bolivia"


specific_countries <- c("United Kingdom", "Bolivia", "South Africa")
specific_diseases <- c("Influenza", "SARS-CoV-2")  # replace with actual disease names
efficacy_filter_lockdown <- 0.8
efficacy_filter_shielding <- 0.9
adherence_filter_lockdown <- 0.7
adherence_filter_shielding <- 0.9





# Read demographic data
demog_data <- read_excel(parameters_file, sheet = "population", col_types = c("text"))

country_data_Uk <- filter(demog_data, country == "United Kingdom")
country_data_Bo <- filter(demog_data, country == "Bolivia (Plurinational State of)")
country_data_Sa <- filter(demog_data, country == "South Africa")

pop_Uk <- sum(as.numeric(country_data_Uk$population))
pop_Bo <- sum(as.numeric(country_data_Bo$population))
pop_Sa <- sum(as.numeric(country_data_Sa$population))
merged_data <- merge(parameters, results, by = "Index")

merged_data <- merged_data %>%
  mutate(Population = case_when(
    Country == "United Kingdom" ~ pop_Uk,
    Country == "Bolivia" ~ pop_Bo,
    Country == "South Africa" ~ pop_Sa,
    TRUE ~ NA_real_
  ))




specific_countries <- c("United Kingdom", "Bolivia", "South Africa")
specific_diseases <- c("Influenza", "SARS-CoV-2")  # replace with actual disease names

filtered_data <- merged_data %>%
  filter(Country %in% specific_countries & `Disease (age curve)` %in% specific_diseases)
filtered_data <-filtered_data %>%
  filter((`Lockdown Efficacy` == efficacy_filter_lockdown & `Lockdown Adherence` == adherence_filter_lockdown) | (`Shielding Efficacy` == efficacy_filter_shielding & `Shielding Adherence` == adherence_filter_shielding) | (`Shielding Efficacy` == 0 & `Lockdown Efficacy` == 0))


merged_data <- filtered_data
merged_data[ , 'Intervention'] = NA
merged_data[ , 'Scaled.Deaths'] = (merged_data$Total.Deaths / merged_data$Population) * 100000
merged_data <- merged_data %>%
  filter(`Country`== filter_country)




for (i in 1:nrow(merged_data)){
  if (merged_data$`Lockdown Efficacy`[i] == 0 && merged_data$`Shielding Efficacy`[i] == 0 ){
    merged_data[,'Intervention'][i] <- "None"
    #merged_data[,'Scaled.Deaths'][i] <- (merged_data$Total.Deaths[i] / merged_data$Population[i]) * 100000
    }
  else if (merged_data$`Lockdown Efficacy`[i] == 0 && merged_data$`Shielding Efficacy`[i] == efficacy_filter_shielding ){
    merged_data[,'Intervention'][i] <- "Shielding"
    #merged_data[,'Scaled.Deaths'][i] <- (merged_data$Total.Deaths[i] / merged_data$Population[i]) * 100000
    }
  else if (merged_data$`Lockdown Efficacy`[i] == efficacy_filter_lockdown && merged_data$`Shielding Efficacy`[i] == 0 ){
    merged_data[,'Intervention'][i] <- "Lockdown"
    #merged_data[,'Scaled.Deaths'][i] <- (merged_data$Total.Deaths[i] / merged_data$Population[i]) * 100000
    }
}

merged_data <- merged_data %>%
  filter(`Shielding Efficacy` == efficacy_filter_shielding | `Lockdown Efficacy` == efficacy_filter_lockdown | merged_data$Intervention == "None")








plot_params <- c("rhoa", "nua", "rho", "nuc", "gamma")

results.df.ft <- merged_data

results.df.ft[plot_params] <- lapply(merged_data[plot_params], function(x) round(as.numeric(as.character(x)), 3))

results.df.ft[plot_params] <- lapply(results.df.ft[plot_params], factor)


results.df.ft_filter_flu <- results.df.ft %>%
  filter(`Disease (age curve)`!= "SARS-CoV-2")

results.df.ft_filter_flu <- results.df.ft_filter_flu %>%
  rename(`Disease Type A` = `Disease (age curve)`)


custom_colors <- c("Lockdown" = "#aec7e8", "None" = "#98df8a", "Shielding" =  "#ffbb78")  # Replace with your actual color codes and intervention names

results.df.ft_filter_flu <- results.df.ft %>%
  filter(`Disease (age curve)`!= "SARS-CoV-2")

results.df.ft_filter_flu <- results.df.ft_filter_flu %>%
  rename(`Disease Type:` = `Disease (age curve)`)


custom_colors <- c("Lockdown" = "#aec7e8", "None" = "#98df8a", "Shielding" =  "#ffbb78")  # Replace with your actual color codes and intervention names

#filter for influenza-like diseases

rhoa_flu <- ggplot(data = results.df.ft_filter_flu, aes(x = rhoa, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Relative Asymptomatic \n Infectivity", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),  
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),           
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

pc_flu <- ggplot(data = results.df.ft_filter_flu, aes(x = pc, y = Total.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Rate of Duration of Infection \n Asymptomatic Individuals", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),   
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

gamma_flu <- ggplot(data = results.df.ft_filter_flu, aes(x = gamma, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  labs(x = "Rate of Duration of Infection \n Symptomatic Individuals", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),   
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

rho_flu <- ggplot(data = results.df.ft_filter_flu, aes(x = rho, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Relative Presymptomatic \n Infectivity", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "bottom",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

flu <- grid.arrange(rhoa_flu, rho_flu, nrow = 2)


## covid
results.df.ft_filter_cov <- results.df.ft %>%
  filter(`Disease (age curve)`!= "Influenza")

results.df.ft_filter_cov <- results.df.ft_filter_cov %>%
  mutate(`Disease (age curve)` = ifelse(`Disease (age curve)` == "SARS-CoV-2", 
                                        "SARS-CoV-2-like Disease", 
                                        `Disease (age curve)`))

rhoa_cov <- ggplot(data = results.df.ft_filter_cov, aes(x = rhoa, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Relative Asymptomatic \n Infectivity", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),  
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),           
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

pc_cov <- ggplot(data = results.df.ft_filter_cov, aes(x = pc, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Rate of Duration of Infection \n Asymptomatic Individuals", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),   
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

gamma_cov <- ggplot(data = results.df.ft_filter_cov, aes(x = gamma, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Rate of Duration of Incubation Period", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.title.x = element_blank(),   
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

rho_cov <- ggplot(data = results.df.ft_filter_cov, aes(x = rho, y = Scaled.Deaths, fill = Intervention)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Relative Presymptomatic \n Infectivity", 
       y = "Average Deaths per 100000 Population") +
  facet_grid(~`Disease (age curve)`, labeller = label_value, drop = TRUE) +  
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13),    
        axis.text.y = element_text(size = 13),    
        axis.title.y = element_text(size = 13),            
        legend.position = "bottom",
        strip.text = element_text(size = 13)) +
  scale_fill_manual(values = custom_colors)

cov <- grid.arrange(rhoa_cov, rho_cov, nrow = 2)



#main_plot <- grid.arrange(flu, cov, ncol=2)

caption <- paste0(filter_country, ", Lockdown Adherence: ", adherence_filter_lockdown, ", Shielding Adherence: ", adherence_filter_shielding, ", Lockdown Efficacy: ", efficacy_filter_lockdown, ", Shielding Efficiacy: ", efficacy_filter_shielding)

main_plot<- grid.arrange(
  flu, cov, 
  ncol = 2,
  top = textGrob(
    caption,
    gp = gpar(fontsize = 14, fontface = "italic"),
    just = "center",
    #hjust = -2
  )
)


ggsave(filename = "BoxPlot.png", plot = main_plot, width = 25, height = 30, units = "cm", dpi = 600)


