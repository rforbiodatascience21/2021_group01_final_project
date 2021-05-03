# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Cleveland <- read_tsv(file = "data/Cleveland.tsv.gz")
Hungarian <- read_tsv(file = "data/Hungarian.tsv.gz")
Switzerland <- read_tsv(file = "data/Switzerland.tsv.gz")
Long_Beach <- read_tsv(file = "data/Long_Beach.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data <- Cleveland %>%
  full_join(Hungarian, by = c("Age", "Sex", "Chest_pain_type", "Resting_blood_pressure",
                              "Serum_cholestoral", "Fasting_blood_sugar", 
                              "Resting_electrocardiographic", "Maximum_heart_rate_achieved", 
                              "Exercise_induced_angina", "ST_depression_induced_by_exercise", 
                              "The_slope_of_the_peak_exercise_ST_segment", 
                              "Number_of_major_vessels_colored_by_flourosopy", "Thal", 
                              "diagnosis_of_heart_disease", "Location")) %>%
  full_join(Switzerland, by = c("Age", "Sex", "Chest_pain_type", "Resting_blood_pressure",
                              "Serum_cholestoral", "Fasting_blood_sugar", 
                              "Resting_electrocardiographic", "Maximum_heart_rate_achieved", 
                              "Exercise_induced_angina", "ST_depression_induced_by_exercise", 
                              "The_slope_of_the_peak_exercise_ST_segment", 
                              "Number_of_major_vessels_colored_by_flourosopy", "Thal", 
                              "diagnosis_of_heart_disease", "Location")) %>%
  full_join(Long_Beach, by = c("Age", "Sex", "Chest_pain_type", "Resting_blood_pressure",
                                "Serum_cholestoral", "Fasting_blood_sugar", 
                                "Resting_electrocardiographic", "Maximum_heart_rate_achieved", 
                                "Exercise_induced_angina", "ST_depression_induced_by_exercise", 
                                "The_slope_of_the_peak_exercise_ST_segment", 
                                "Number_of_major_vessels_colored_by_flourosopy", "Thal", 
                                "diagnosis_of_heart_disease", "Location"))

# Changing 0 in cholesterol and blood pressure to NA
Data <- Data %>% 
  mutate(Serum_cholestoral = case_when(Serum_cholestoral > 0 ~ Serum_cholestoral),
         Resting_blood_pressure = case_when(Resting_blood_pressure > 0 ~ Resting_blood_pressure))


# Write data --------------------------------------------------------------
write_tsv(x = Data, file = "data/02_data.tsv.gz")
