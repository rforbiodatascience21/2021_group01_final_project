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
                     columns = c("Age","Sex_cat", "Chest_pain_type_cat",
                                 "Resting_blood_pressure","Serum_cholestoral",
                                 "Fasting_blood_sugar_cat","Resting_electrocardiographic_cat",
                                 "Maximum_heart_rate_achieved","Exercise_induced_angina_cat",
                                 "ST_depression_induced_by_exercise",
                                 "The_slope_of_the_peak_exercise_ST_segment",
                                 "Diagnosis_of_disease"),
                     columnLabels = c("Age","Sex", "Chest pain type",
                                 "Resting blood pressure","Serum cholestoral",
                                 "Fasting blood sugar","Resting electrocardiographic",
                                 "Maximum heart rate","Exercise induced angina",
                                 "ST depression",
                                 "slope of ST segment",
                                 "Diagnosis"),
                     upper = list(continuous = "blank", combo = "blank", discrete = "blank", na = "na"),
                     lower = list(continuous = "points", combo = "box_no_facet", discrete = "ratio", na = "na"),
                     labeller = label_wrap_gen(10)) +
  labs(title = "Correlation plots of predictive variables stratified on diagnosis") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Corr_plot
png(filename="/cloud/project/results/Summary_correlation_plots.png",  width = 1000, height = 800)
Corr_plot
dev.off()
