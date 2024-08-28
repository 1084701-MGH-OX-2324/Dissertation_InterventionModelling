###########Libraries

library(deSolve) 
library(readxl) 
library(viridis)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)


# Set working directory
#setwd("<dir/name.")

file_name <- ("ModelRuns.xlsx")

current_diseases <- c("Influenza", "SARS-CoV-2")
age_groups <- read_excel(file_name, sheet= "Influenza")$agefloor



plot_data <- data.frame()

# Create data frame for plotting
for (i in 1:length(current_diseases)) {
  ratios <- read_excel(file_name, sheet=current_diseases[i])
  ihr <- ratios$ihr
  hfr <- ratios$hfr
  cfr <- ratios$cfr
  
  disease_data <- data.frame(
    AgeGroup = age_groups,
    IHR = ihr,
    HFR = hfr,
    CFR = cfr,
    Disease = current_diseases[i]
  )
  
  plot_data <- rbind(plot_data, disease_data)
}


# Arrange data
plot_data_melt <- melt(plot_data, id.vars = c("AgeGroup", "Disease"), 
                       variable.name = "Metric", value.name = "Rate")

plot_data_influenza <- subset(plot_data_melt, Disease == "Influenza")
plot_data_covid <- subset(plot_data_melt, Disease == "SARS-CoV-2")


# Influenza-like diseases
plot_influenza <- ggplot(plot_data_influenza, aes(x = AgeGroup, y = Rate)) +
  geom_line(size = 1.2, color = "grey") +
  facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
  labs(title = "Influenza-like Disease", x = NULL, y = "Ratio in %")  +
  theme_minimal(base_size = 10)  +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

# Covid-like diseases
plot_covid <- ggplot(plot_data_covid, aes(x = AgeGroup, y = Rate)) +
  geom_line(size = 1.2, color = "black") +
  facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
  labs(title = "SARS-CoV-2-like Disease",
       x = "Age Group", y = "Ratio in %") +
  theme_minimal(base_size = 10)+
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12)
  )


ratios <- grid.arrange(plot_influenza, plot_covid, nrow = 2)

ggsave("Ratios.png", plot= ratios, width = 25, height = 18, units = "cm", dpi = 600)

##scaled ratios



# extract data
demog_data <- read_excel(file_name, sheet="population", col_types = c("text"))
demog_data_UK <- filter(demog_data, country == "United Kingdom")
demog_data_BO <- filter(demog_data, country == "Bolivia (Plurinational State of)")
demog_data_SA <- filter(demog_data, country == "South Africa")

popstruc_UK <- as.numeric(demog_data_UK$population)
popstruc_UK_ratios <- (popstruc_UK / sum(popstruc_UK))

popstruc_BO <- as.numeric(demog_data_BO$population)
popstruc_BO_ratios <- (popstruc_BO / sum(popstruc_BO))

popstruc_SA <- as.numeric(demog_data_SA$population)
popstruc_SA_ratios <- (popstruc_SA / sum(popstruc_SA))


ihr_cov <- (filter(plot_data_covid, Metric == "IHR"))$Rate
ihr_flu <- (filter(plot_data_influenza, Metric == "IHR"))$Rate

# sclae the risk group to poulation
scaled_ratio_cov_UK <- popstruc_UK_ratios * ihr_cov
scaled_ratio_cov_BO <- popstruc_BO_ratios * ihr_cov
scaled_ratio_cov_SA <- popstruc_SA_ratios * ihr_cov



#covid
df_cov_UK <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_UK_ratios * ihr_cov,
  Disease = "COVID",
  Country = "United Kingdom"
)

df_cov_BO <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_BO_ratios * ihr_cov,
  Disease = "COVID",
  Country = "Bolivia (Plurinational State of)"
)



df_cov_SA <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_SA_ratios * ihr_cov,
  Disease = "COVID",
  Country = "South Africa"
)

# Influenza
df_flu_UK <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_UK_ratios * ihr_flu,
  Disease = "Influenza",
  Country = "United Kingdom"
)

df_flu_BO <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_BO_ratios * ihr_flu,
  Disease = "Influenza",
  Country = "Bolivia (Plurinational State of)"
)

df_flu_SA <- data.frame(
  AgeCategory = disease_data$AgeGroup,  
  ScaledRatio = popstruc_SA_ratios * ihr_flu,
  Disease = "Influenza",
  Country = "South Africa"
)

all_data <- rbind(df_cov_UK, df_cov_BO, df_cov_SA, df_flu_UK, df_flu_BO, df_flu_SA)



all_data$Country[all_data$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
all_data$Disease[all_data$Disease == "Influenza"] <- "Influenza-like"
all_data$Disease[all_data$Disease == "COVID"] <- "SARS-CoV-2-like"

options(scipen = 999)

scaled_ratios <- ggplot(all_data, aes(x = AgeCategory, y = ScaledRatio, color = Disease, group = Disease)) +
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  facet_grid(Disease ~ Country, scales = "free_y") + 
  labs(
    x = "Age Category",
    y = "Infection Hospitalization Ratio in % Scaled to Total Population"
  ) +
  theme_classic() +  # Use a classic theme
  scale_color_brewer(palette = "Set1") +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12), 
    axis.title = element_text(size = 14), 
    panel.grid.major = element_line(color = "gray90"),  
    panel.grid.minor = element_blank(),  
    panel.spacing = unit(1.5, "lines"),  
    strip.placement = "outside",  
    strip.background = element_blank(),  
    strip.switch.pad.grid = unit(0.1, "cm")  
  )



#panel for target groups
risk_group_influenza <- c(0, 5, 10, 65, 70, 75, 80, 85, 90, 95, 100)  # 0-10 and 65+
risk_group_covid <- c(60,65, 70, 75, 80, 85, 90, 95, 100)  # 65+

calculate_risk_group_proportion <- function(population_ratios, age_groups, risk_group) {
  risk_population <- sum(population_ratios[age_groups %in% risk_group])
  return(risk_population)
}


# calulate proportion of risk group
proportion_risk_influenza_UK <- calculate_risk_group_proportion(popstruc_UK_ratios, age_groups, risk_group_influenza)
proportion_risk_influenza_BO <- calculate_risk_group_proportion(popstruc_BO_ratios, age_groups, risk_group_influenza)
proportion_risk_influenza_SA <- calculate_risk_group_proportion(popstruc_SA_ratios, age_groups, risk_group_influenza)

proportion_risk_covid_UK <- calculate_risk_group_proportion(popstruc_UK_ratios, age_groups, risk_group_covid)
proportion_risk_covid_BO <- calculate_risk_group_proportion(popstruc_BO_ratios, age_groups, risk_group_covid)
proportion_risk_covid_SA <- calculate_risk_group_proportion(popstruc_SA_ratios, age_groups, risk_group_covid)


# create labels
cat("Proportion of Population in Risk Group:\n")
cat("Influenza - UK:", proportion_risk_influenza_UK, "\n")
cat("Influenza - Bolivia:", proportion_risk_influenza_BO, "\n")
cat("Influenza - South Africa:", proportion_risk_influenza_SA, "\n")
cat("COVID - UK:", proportion_risk_covid_UK, "\n")
cat("COVID - Bolivia:", proportion_risk_covid_BO, "\n")
cat("COVID - South Africa:", proportion_risk_covid_SA, "\n")


# print the labels
annotation_data <- data.frame(
  Country = c("United Kingdom", "Bolivia", "South Africa", "United Kingdom", "Bolivia", "South Africa"),
  Disease = c("Influenza-like", "Influenza-like", "Influenza-like", "SARS-CoV-2-like", "SARS-CoV-2-like", "SARS-CoV-2-like"),
  Annotation = c(
    paste0(round(proportion_risk_influenza_UK * 100, 2), "%"),
    paste0(round(proportion_risk_influenza_BO * 100, 2), "%"),
    paste0(round(proportion_risk_influenza_SA * 100, 2), "%"),
    paste0(round(proportion_risk_covid_UK * 100, 2), "%"),
    paste0(round(proportion_risk_covid_BO * 100, 2), "%"),
    paste0(round(proportion_risk_covid_SA * 100, 2), "%")
  )
)

# add panels
scaled_ratios_panel <- ggplot(all_data, aes(x = AgeCategory, y = ScaledRatio, color = Disease, group = Disease)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  facet_wrap(Disease ~ Country, scales = "free_y") +  
  labs(
    x = "Age Category",
    y = "Scaled Infection Hospitalization Ratio (% of Total Population)"
  ) +
  theme_classic() +  
  scale_color_brewer(palette = "Set1") +  
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),  
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    panel.grid.major = element_line(color = "gray90"),  
    panel.grid.minor = element_blank(),  
    panel.spacing = unit(1.5, "lines"),  
    strip.placement = "outside",  
    strip.background = element_blank(),  
    strip.switch.pad.grid = unit(0.1, "cm")  
  ) +
  geom_label(data = annotation_data, aes(x = 90, y = Inf, label = Annotation), 
             vjust = 1.5, hjust = 0.5, size = 4, fill = "white", color = "black", inherit.aes = FALSE)

ggsave("ScaledRatios.png", plot = scaled_ratios_panel, width = 25, height = 20, units = "cm", dpi = 600)

scaled_ratios_panel


