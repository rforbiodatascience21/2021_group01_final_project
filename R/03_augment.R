# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/02_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data_aug <- Data %>%
  mutate(Location_num = case_when(Location == "Cleveland" ~ 0, 
                                  Location == "Switzerland" ~ 1, 
                                  Location == "Hungarian" ~ 2, 
                                  Location == "Long Beach" ~ 3),
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
                                                 Exercise_induced_angina == 0 ~ "Not present"))

# Write data --------------------------------------------------------------
write_tsv(x = Data_aug,
          file = "data/03_data_aug.tsv.gz")
