# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(GGally)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
Corr_plot <- ggpairs(Data_aug,
                     mapping = aes(colour = Location),
                     columns = c("Age","Sex", "Chest_pain_type",
                                 "Resting_blood_pressure","Serum_cholestoral",
                                 "Fasting_blood_sugar","Resting_electrocardiographic",
                                 "Maximum_heart_rate_achieved","Exercise_induced_angina",
                                 "ST_depression_induced_by_exercise",
                                 "The_slope_of_the_peak_exercise_ST_segment",
                                 "diagnosis_of_heart_disease")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Correlation plots of of environmental factors by country")
Corr_plot
