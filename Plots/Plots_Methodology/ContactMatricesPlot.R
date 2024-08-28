# Load necessary libraries
library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(patchwork)
library(ggalluvial)
library(tidyr)
library(dplyr)
library(readr)
library(grid)
library(gridExtra)
library(cowplot)



#setwd("<dir/name.")
contacts <- load("contacts.Rda")

#country <- "United Kingdom of Great Britain"

contact_home$`United Kingdom of Great Britain`
matrix_plot <- function(country){
  
  # Extract data for country
  contacts_uk <-  contact_school[[country]] + contact_work[[country]] + contact_other[[country]] + contact_home[[country]]
  
  # Ensure matrix form
  contacts_uk <- as.matrix(contacts_uk)
  print(contacts_uk)

  # Create age group labels (adjust according to your data)
  age_groups <- seq(0, 75, by = 5) # Example: 0-4, 5-9, ..., 75-79
  
  
  # Convert data for plotting
  contacts_uk_long <- melt(contacts_uk)
  
  colnames(contacts_uk_long) <- c("Age_Individual", "Age_HH_Contact", "Contact_Count")
  
  # Convert age group indices to actual age group labels
  contacts_uk_long$Age_Individual <- factor(contacts_uk_long$Age_Individual, labels = age_groups)
  contacts_uk_long$Age_HH_Contact <- factor(contacts_uk_long$Age_HH_Contact, labels = age_groups)
  
  # Plotting the heatmap
  ggplot(contacts_uk_long, aes(x = Age_Individual, y = Age_HH_Contact, fill = Contact_Count)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("white", "lightblue", "darkblue"), limits = c(0, 15)) +
    labs(y = "Age of Contact", fill = "Contact Count") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",
          panel.grid.major = element_line(color = "black", size = 0.5), 
          panel.grid.minor = element_blank())  
    #ggtitle(paste0(country))

}

plot_sa <- matrix_plot("South Africa")
plot_bo <- matrix_plot("Bolivia (Plurinational State of")
plot_uk <- matrix_plot("United Kingdom of Great Britain")


combined_plot_mat <- plot_grid(plot_uk, plot_bo, plot_sa, 
                           nrow = 1, 
                           align = "v", 
                           axis = "lr") 

mat <- ggdraw() +
  draw_plot(combined_plot_mat, 0.1, 0.1, 0.9, 0.9) +
  draw_label("Age of the Individual (Age Floor)", 
             x = 0.5, y = -0.05, vjust = -1, size = 16) +
  draw_label("Age of the Contact (Age Floor)", 
             x = -0.05, y = 0.5, angle = 90, vjust = 1, size = 16)

matrix <- grid.arrange(plot_uk, plot_bo, plot_sa, ncol = 3)

###########DEMOG PLOT

load("demog.Rda")

# Population data

sa_pop <- population %>%
  filter(population$country == "South Africa")

uk_pop <- population %>%
  filter(population$country == "United Kingdom")

bo_pop <- population %>%
  filter(population$country == "Bolivia (Plurinational State of)")




calculate_proportions <- function(df) {
  age_groups <- seq(0,100, by=5)
  
  total_population <- sum(df$pop)
  df$ages <- seq(0,100, by=5) 
  
  df %>%
    group_by(age_category) %>%
    mutate(proportion = pop / total_population)
    
  
}

sa_pop <- calculate_proportions(sa_pop)
uk_pop <- calculate_proportions(uk_pop)
bo_pop <- calculate_proportions(bo_pop)


sa_pop_plot <- ggplot(sa_pop, aes(x = ages, y = proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(y = "Population", title = "South Africa") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  coord_flip()


uk_pop_plot <- ggplot(uk_pop, aes(x = ages, y = proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Age Floor", y = "Population", title = "United Kingdom") +
  theme_minimal() +
  theme(axis.title.x = element_blank()) +
  coord_flip()

bo_pop_plot <- ggplot(bo_pop, aes(x = ages, y = proportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(y = "Population", title = "Bolivia") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  coord_flip()

grid.arrange(uk_pop_plot, bo_pop_plot, sa_pop_plot, ncol = 3)



combined_plot <- plot_grid(uk_pop_plot, bo_pop_plot, sa_pop_plot, 
                           nrow = 1, 
                           align = "v", 
                           axis = "lr")  

demo <- ggdraw() +
  draw_plot(combined_plot, 0.05, 0.05, 0.95, 0.95) +  
  draw_label("Proportion of the Population [%]", 
             x = 0.0, y = 0.0, angle = 0, vjust = -0.5, hjust = -1.9, size = 12)


main_plot <- grid.arrange(demo, mat, nrow=2)

ggsave(filename = "Demog.png", plot = main_plot, width = 35, height = 30, units = "cm", dpi = 600)
