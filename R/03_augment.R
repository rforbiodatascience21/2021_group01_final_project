# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/02_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data_aug <- Data %>%
  mutate(Location_num = factor(case_when(Location == "Cleveland" ~ 0, 
                                  Location == "Switzerland" ~ 1, 
                                  Location == "Hungarian" ~ 2, 
                                  Location == "Long Beach" ~ 3)),
         Diagnosis_of_disease = case_when(diagnosis_of_heart_disease == 0 ~ "Not present", 
                                          diagnosis_of_heart_disease >= 1 ~ "Present"),
         Sex_cat = case_when(Sex == 0 ~ "Female", 
                             Sex == 1 ~ "Male"),
         Chest_pain_type_cat = case_when(Chest_pain_type == 1 ~ "typical", 
                                         Chest_pain_type == 2 ~ "atypcal",
                                         Chest_pain_type == 3 ~ "non-anginal",
                                         Chest_pain_type == 4 ~ "asymptomatic"),
         Fasting_blood_sugar_cat = case_when(Fasting_blood_sugar == 0 ~ "Low", 
                                             Fasting_blood_sugar == 1 ~ "High"),
         Resting_electrocardiographic_cat = case_when(Resting_electrocardiographic == 0 ~ "Normal", 
                                                      Resting_electrocardiographic == 1 ~ "ST-T abnormality", 
                                                      Resting_electrocardiographic == 2 ~ "LV hypertrophy"),
         Exercise_induced_angina_cat = case_when(Exercise_induced_angina == 1 ~ "Present", 
                                                 Exercise_induced_angina == 0 ~ "Not present"),
         slope_of_ST_cat = case_when(The_slope_of_the_peak_exercise_ST_segment == 1 ~"uplsoping",
                                     The_slope_of_the_peak_exercise_ST_segment == 2 ~"flat",
                                     The_slope_of_the_peak_exercise_ST_segment == 3 ~"downsloping"),
         Thal_cat = case_when(Thal == 3 ~"normal",
                              Thal == 6 ~"fixed defect",
                              Thal == 7 ~"reversable defect"),
         Age_class = case_when(Age < 20 ~ ">20",
                   20 <= Age & Age < 30 ~ "20-30",
                   30 <= Age & Age < 40 ~ "30-40",
                   40 <= Age & Age < 50 ~ "40-50",
                   50 <= Age & Age < 60 ~ "50-60",
                   60 <= Age & Age < 70 ~ "60-70",
                   70 <= Age & Age < 80 ~ "70-80",
                   80 <= Age ~ "80<"))

# Write data --------------------------------------------------------------
write_tsv(x = Data_aug,
          file = "data/03_data_aug.tsv.gz")
