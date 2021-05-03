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
                     mapping = aes(colour = Diagnosis_of_disease),
                     columns = c("Age","Sex_cat", "Chest_pain_type",
                                 "Resting_blood_pressure","Serum_cholestoral",
                                 "Fasting_blood_sugar_cat","Resting_electrocardiographic_cat",
                                 "Maximum_heart_rate_achieved","Exercise_induced_angina_cat",
                                 "ST_depression_induced_by_exercise",
                                 "The_slope_of_the_peak_exercise_ST_segment",
                                 "Diagnosis_of_disease"),
                     upper = list(continuous = "blank", combo = "blank", discrete = "blank", na = "na"),
                     lower = list(continuous = "points", combo = "box", discrete = "box", na = "na")) +
  theme_minimal() +
  labs(title = "Correlation plots of predictive variables stratified on diagnosis")
Corr_plot
