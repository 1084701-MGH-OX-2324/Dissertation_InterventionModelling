library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggalluvial)
library(tidyr)
library(dplyr)
library(readr)
library(grid)
library(gridExtra)
library(kableExtra)


###

# Function to calculate standard error
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}


###### Filter for chosen scenario

efficacy_filter_lockdown <- 0.8
efficacy_filter_shielding <- 0.9
adherence_filter_lockdown <- 0.7
adherence_filter_shielding <- 0.9

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


#add coloumn for total poulation 
merged_data <- merged_data %>%
  mutate(Population = case_when(
    Country == "United Kingdom" ~ pop_Uk,
    Country == "Bolivia" ~ pop_Bo,
    Country == "South Africa" ~ pop_Sa,
    TRUE ~ NA_real_
  ))

is_alluvia_form(as.data.frame(merged_data), axes = 1:3, silent = TRUE)



specific_countries <- c("United Kingdom", "Bolivia", "South Africa")
specific_diseases <- c("Influenza", "SARS-CoV-2")  # replace with actual disease names



#filter data as needed

filtered_data <- merged_data %>%
  filter(Country %in% specific_countries & `Disease (age curve)` %in% specific_diseases)
filtered_data <- filtered_data %>%
  filter((`Lockdown Efficacy` == efficacy_filter_lockdown & `Lockdown Adherence` == adherence_filter_lockdown) | (`Shielding Efficacy` == efficacy_filter_shielding & `Shielding Adherence` == adherence_filter_shielding) | (`Lockdown Efficacy` == 0 & `Shielding Efficacy` == 0))

#filter for specific tansmission probability
filtered_data <- filtered_data %>%
  filter(p == 0.042 | p== 0.03)
filtered_data <- filtered_data %>% drop_na()




### organize data after interventions

## lockdown
average_lock <- filtered_data %>%
  filter(`Lockdown Efficacy` != 0) %>%
  group_by(Country, `Disease (age curve)`, `Lockdown Efficacy`) %>%
  summarise(
    Avg_Deaths = mean((Total.Deaths / Population) * 100000, na.rm = TRUE),
    SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
    Avg_YLL = mean(YLL/Total.Deaths, na.rm = TRUE),
    SE_YLL = standard_error(YLL/Total.Deaths),
    Avg_Person.Days.Covered.Lockdown = mean(Person.Days.Covered.Lockdown),
    Avg_Hosp = mean((Total.Hosp / Population) * 100000, na.rm = TRUE),
    SE_Hosp = standard_error((Total.Hosp / Population) * 100000),
    Avg_Infec =mean((Total.Infections / Population)*100, na.rm = TRUE),
    SE_Infec = standard_error((Total.Infections / Population)*100),
    IFR = mean((Total.Deaths/Total.Infections)*100, na.rm = TRUE),
    SE_IFR = standard_error((Total.Deaths/Total.Infections)*100)
  ) %>%
  ungroup() %>%
  mutate(Intervention = "Lockdown")

#shielding
average_shielding <- filtered_data %>%
  filter(`Shielding Efficacy` != 0) %>%
  group_by(Country, `Disease (age curve)`, `Shielding Efficacy`) %>%
  summarise(
    Avg_Deaths = mean((Total.Deaths / Population) * 100000, na.rm = TRUE),
    SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
    Avg_YLL = mean(YLL/Total.Deaths, na.rm = TRUE),
    SE_YLL = standard_error(YLL/Total.Deaths),
    Avg_Person.Days.Covered.Shielding = mean(Person.Days.Covered.Lockdown),
    Avg_Hosp = mean((Total.Hosp / Population) * 100000, na.rm = TRUE),
    SE_Hosp = standard_error((Total.Hosp / Population) * 100000),
    Avg_Infec =mean((Total.Infections / Population)*100, na.rm = TRUE),
    SE_Infec = standard_error((Total.Infections / Population)*100),
    IFR = mean((Total.Deaths/Total.Infections)*100, na.rm = TRUE),
    SE_IFR = standard_error((Total.Deaths/Total.Infections)*100)
  ) %>%
  ungroup() %>%
  mutate(Intervention = "Shielding")






#zero values, for deaths averted
average_none <- filtered_data %>%
  filter(`Lockdown Efficacy` == 0 & `Shielding Efficacy` == 0) %>%
  group_by(Country, `Disease (age curve)`, `Lockdown Efficacy`) %>%
  summarise(
    Avg_Deaths = mean((Total.Deaths / Population) * 100000, na.rm = TRUE),
    SE_Deaths = standard_error((Total.Deaths / Population) * 100000),
  ) %>%
  ungroup() %>%
  mutate(
    Intervention = "None"
  )



zero_lockdown <- average_none %>%
  mutate(Intervention = "Lockdown", `Avg_Person.Days.Covered.Lockdown` = 0)

zero_shielding <- average_none %>%
  mutate(Intervention = "Shielding", `Avg_Person.Days.Covered.Lockdown` = 0)

#renaming for merge

average_shielding <- average_shielding %>%
  rename(`Avg_Person.Days.Covered.Lockdown` = `Avg_Person.Days.Covered.Shielding`)

average_shielding <- average_shielding %>%
  rename(`Efficacy` = `Shielding Efficacy`)
average_lock <- average_lock %>%
  rename(`Efficacy` = `Lockdown Efficacy`)

# Merge the No Intervention data with the Lockdown and Shielding data
average_deaths <- bind_rows(average_lock, average_shielding)




# Rename columns to prepare for merging
average_none <- average_none %>%
  rename(
    Avg_Deaths_None = Avg_Deaths,
    SE_Deaths_None = SE_Deaths
  )

# Merge average_deaths with average_none on Country and Disease (age curve)
average_deaths_averted <- average_deaths %>%
  left_join(average_none, by = c("Country", "Disease (age curve)"))

# Calculate the deaths averted for each scenario
average_deaths_averted <- average_deaths_averted %>%
  mutate(
    Deaths_Averted = Avg_Deaths_None - Avg_Deaths,
    SE_Deaths_Averted = sqrt(SE_Deaths_None^2 + SE_Deaths^2)  # Corrected formula here
  )

average_deaths_averted <- average_deaths_averted %>%
  rename(`Disease` = `Disease (age curve)`)

average_deaths_averted <- average_deaths_averted %>%
  rename(`Intervention` = `Intervention.x`)

##generate the final dataframe for the plot

alluvial_data <- average_deaths_averted %>%
  group_by(Country, Disease, Intervention) %>%
  summarize(Avg_Deaths = round(mean(Avg_Deaths), 0),
            SE_Deaths = SE_Deaths,
            Avg_YLL = round(mean(Avg_YLL), 0),
            SE_YLL = SE_YLL,
            Avg_Hosp = round(mean(Avg_Hosp), 0),
            SE_Hosp = SE_Hosp,
            Avg_Infec = round(mean(Avg_Infec), 1),
            SE_Infec = SE_Infec,
            IFR = round(mean(IFR),3),
            SE_IFR = SE_IFR,
            Avg_Deaths_Averted = round(mean(Deaths_Averted),0),
            SE_Deaths_Averted = SE_Deaths_Averted) %>%
  ungroup()

alluvial_data <- alluvial_data %>%
  mutate(Scenario = row_number())


##organize scenarios for legend

tag_list <- vector("list", nrow(alluvial_data))

tag_list <- sapply(1:nrow(alluvial_data), function(i) {
  print(i)
  x <- toString(i)
  tag_list[i] <- paste0(toString(alluvial_data$Country[i]),": ", toString(alluvial_data$Intervention[i]))
})

alluvial_data <- alluvial_data %>%
  mutate(Scenarios = tag_list)

alluvial_data <- alluvial_data[!is.na(alluvial_data$Scenarios), ]


#legend colors
fill_colors <- c(
  "Bolivia: Lockdown" = "#1f77b4",      # Dark Blue
  "Bolivia: Shielding" = "#aec7e8",     # Light Blue
  "South Africa: Lockdown" = "#2ca02c", # Dark Green
  "South Africa: Shielding" = "#98df8a",# Light Green
  "United Kingdom: Lockdown" = "#ff7f0e",# Dark Orange
  "United Kingdom: Shielding" = "#ffbb78"# Light Orange
)



alluvial_data_flu <- alluvial_data %>%
  filter(Disease == "Influenza")
alluvial_data_cov <- alluvial_data %>%
  filter(Disease == "SARS-CoV-2")


lockdown_adherence <- filtered_data %>%
  filter(`Lockdown Adherence` != 0)
label_lockdown_adherence <- mean(lockdown_adherence$`Lockdown Adherence`)

shielding_adherence <- filtered_data %>%
  filter(`Shielding Adherence` != 0)
label_shielding_adherence <- mean(shielding_adherence$`Shielding Adherence`)

subtitle <- paste0("(Lockdown Adherence: ", adherence_filter_lockdown, ", Shielding Adherence: ", adherence_filter_shielding, ", Lockdown Effective Coverage: ", efficacy_filter_lockdown, ", Shielding Effective Coverage: ", efficacy_filter_shielding, ")")



## plot for influenza-like disease
p2 <- ggplot(alluvial_data_flu, aes(axis1 = Avg_Deaths, axis2 = Avg_YLL, axis3 = Avg_Hosp, axis4= IFR, axis5 = Avg_Infec, axis6 = Avg_Deaths_Averted, fill = Scenarios)) +
  geom_alluvium(width = 1/12) +
  geom_stratum(width = 1/12, fill = "white", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4.5, nudge_x = 0, color = "black") +
  scale_x_discrete(limits = c("Total Deaths \n per 100000", "Years-Life-Lost \n per Death", "Total Hospitalisations \n per 100000", "Infection-Fatality-Ratio \n (%)", "Final Epidemic \n Size (%)", "Total Deaths Averted \n per 100000"), expand = c(0.15, 0.05)) +
  scale_fill_manual(values = fill_colors, na.translate = FALSE) +  
  scale_color_viridis_b() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.text.y = element_text(size = 14, angle = 90, vjust = 1, hjust = 0.5),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0),  
    plot.subtitle = element_text(size = 12, hjust = 0),
    legend.text = element_text(size = 14),  # Change the size of the legend text
    legend.title = element_text(size = 16))+
  coord_flip() + 
  labs(title = "Influenza-like Outbreak", subtitle = subtitle)
p2


#plot for covid-like disease
p3 <- ggplot(alluvial_data_cov, aes(axis1 = Avg_Deaths, axis2 = Avg_YLL, axis3 = Avg_Hosp, axis4= IFR, axis5 = Avg_Infec, axis6 = Avg_Deaths_Averted, fill = Scenarios)) +
  geom_alluvium(width = 1/12) +
  geom_stratum(width = 1/12, fill = "white", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4.5, nudge_x = 0, color = "black") +  # Increased size from 3 to 4.5
  scale_x_discrete(limits = c("Total Deaths \n per 100000", "Years-Life-Lost \n per Death", "Total Hospitalisations \n per 100000", "Infection-Fatality-Ratio \n (%)", "Final Epidemic \n Size (%)", "Total Deaths Averted \n per 100000"), expand = c(0.15, 0.05)) +
  scale_fill_manual(values = fill_colors, na.translate = FALSE) + 
  scale_color_viridis_b() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),  
    axis.line.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.text.y = element_text(size = 14, angle = 90, vjust = 1, hjust = 0.5),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0),  
    plot.subtitle = element_text(size = 12, hjust = 0),
    legend.text = element_text(size = 14),  # Change the size of the legend text
    legend.title = element_text(size = 16))+
  coord_flip() +
  labs(title = "SARS-CoV-2-like Outbreak", subtitle = subtitle)




print(p3)


#main_plot <- grid.arrange(p2,p3,nrow=2)



ggsave(filename = "AlluvialCov.png", plot = p3, width = 25, height = 40, units = "cm", dpi = 600)
#ggsave(filename = "AlluvialInf.png", plot = p2, width = 25, height = 40, units = "cm", dpi = 600)


##Tiff
tiff("Inf.tiff", width = 10, height = 15, units = "in", res = 300)
print(p2)
dev.off()


##Tiff
tiff("Cov.tiff", width = 10, height = 15, units = "in", res = 300)
print(p3)
dev.off()



###Generating Latex Tables

# Calculate CI 
calculate_ci <- function(df, avg_col, se_col) {
  df %>%
    mutate(
      CI_Lower = round(!!sym(avg_col) - 1.96 * !!sym(se_col), 2),
      CI_Upper = round(!!sym(avg_col) + 1.96 * !!sym(se_col), 2),
      !!se_col := round(!!sym(se_col), 2),  # Rounding SE
      CI = paste0("[", CI_Lower, ", ", CI_Upper, "]")  # Exclude "95% CI" from each row
    ) %>%
    select(-CI_Lower, -CI_Upper)  # Remove individual CI columns
}


rank_deaths <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange((Avg_Deaths)) %>%
  select(Country, Disease, Intervention, Avg_Deaths, SE_Deaths) %>%
  calculate_ci("Avg_Deaths", "SE_Deaths") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `Average Deaths` = Avg_Deaths,
    `SE` = SE_Deaths,
    `95% CI` = CI
  )


rank_YLL <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange(round(Avg_YLL,2)) %>%
  select(Country, Disease, Intervention, Avg_YLL, SE_YLL) %>%
  calculate_ci("Avg_YLL", "SE_YLL") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `Average Years of Life Lost Per Death` = Avg_YLL,
    `SE` = SE_YLL,
    `95% CI` = CI
  )


rank_hosp <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange((Avg_Hosp)) %>%
  select(Country, Disease, Intervention, Avg_Hosp, SE_Hosp) %>%
  calculate_ci("Avg_Hosp", "SE_Hosp") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `Average Hospitalizations` = Avg_Hosp,
    `SE` = SE_Hosp,
    `95% CI` = CI
  )


rank_infec <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange((Avg_Infec)) %>%
  select(Country, Disease, Intervention, Avg_Infec, SE_Infec) %>%
  calculate_ci("Avg_Infec", "SE_Infec") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `Average Infections` = Avg_Infec,
    `SE` = SE_Infec,
    `95% CI` = CI
  )

rank_ifr <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange((IFR)) %>%
  select(Country, Disease, Intervention, IFR, SE_IFR) %>%
  calculate_ci("IFR", "SE_IFR") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `IFR` = IFR,
    `SE` = SE_IFR,
    `95% CI` = CI
  )

rank_deaths_averted <- alluvial_data %>%
  group_by(Country, Disease, Intervention) %>%
  arrange(desc(Avg_Deaths_Averted)) %>%
  select(Country, Disease, Intervention, Avg_Deaths_Averted, SE_Deaths_Averted) %>%
  calculate_ci("Avg_Deaths_Averted", "SE_Deaths_Averted") %>%
  rename(
    `Country` = Country,
    `Disease` = Disease,
    `Intervention` = Intervention,
    `Average Deaths Averted` = Avg_Deaths_Averted,
    `SE` = SE_Deaths_Averted,
    `95% CI` = CI
  )




#latex table output in console

table_deaths <- kable(rank_deaths, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_deaths

table_yll <- kable(rank_YLL, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_yll

table_hosp <- kable(rank_hosp, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_hosp

table_infec <- kable(rank_infec, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_infec

table_ifr <- kable(rank_ifr, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_ifr

table_deaths_averted <- kable(rank_deaths_averted, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = "hold_position")

table_deaths_averted


### Generating the death deltas 


# delat in deaths
death_deltas <- average_deaths %>%
  filter(Intervention %in% c("Lockdown", "Shielding")) %>%
  select(Country, `Disease (age curve)`, Intervention, Avg_Deaths) %>%
  spread(key = Intervention, value = Avg_Deaths) %>%
  mutate(
    Delta_Deaths = abs(Lockdown - Shielding)
  )

#standard error
se_deltas <- average_deaths %>%
  filter(Intervention %in% c("Lockdown", "Shielding")) %>%
  select(Country, `Disease (age curve)`, Intervention, SE_Deaths) %>%
  spread(key = Intervention, value = SE_Deaths) %>%
  rename(
    Lockdown_SE = Lockdown,
    Shielding_SE = Shielding
  ) %>%
  mutate(
    SE_Delta = sqrt(Lockdown_SE^2 + Shielding_SE^2)
  )

#merge
death_deltas <- death_deltas %>%
  left_join(se_deltas %>% select(Country, `Disease (age curve)`, SE_Delta),
            by = c("Country", "Disease (age curve)"))

#rename disease types
death_deltas <- death_deltas %>%
  mutate(
    `Disease (age curve)` = case_when(
      `Disease (age curve)` == "Influenza" ~ "Influenza-like",
      `Disease (age curve)` == "SARS-CoV-2" ~ "SARS-CoV-2-like",
      TRUE ~ `Disease (age curve)`
    )
  )





#plot with error bars
ggplot(death_deltas, aes(x = Country, y = Delta_Deaths, fill = `Disease (age curve)`)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7, color = "black") +
  geom_errorbar(aes(ymin = Delta_Deaths - SE_Delta, ymax = Delta_Deaths + SE_Delta),
                position = position_dodge(width = 0.7), width = 0.25, color = "black", alpha = 0.5) +
  geom_text(aes(label = round(Delta_Deaths, 0)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4, color = "black") +
  labs(
    x = NULL,
    y = "Difference in Deaths per 100,000 Population"
  ) +
  scale_fill_manual(values = c("Influenza-like" = "#fc8660", "SARS-CoV-2-like" = "#9ecae1")) +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )