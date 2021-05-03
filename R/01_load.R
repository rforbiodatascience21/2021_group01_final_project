# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ----------------------------------------------------------
Cleveland_raw <- read_csv("data/_raw/processed.cleveland.data", 
                          col_names = c("Age","Sex", "Chest_pain_type",
                                        "Resting_blood_pressure","Serum_cholestoral",
                                        "Fasting_blood_sugar","Resting_electrocardiographic",
                                        "Maximum_heart_rate_achieved","Exercise_induced_angina",
                                        "ST_depression_induced_by_exercise",
                                        "The_slope_of_the_peak_exercise_ST_segment",
                                        "Number_of_major_vessels_colored_by_flourosopy",
                                        "Thal","diagnosis_of_heart_disease"),na = "?")

Hungarian_raw <- read_csv("data/_raw/processed.hungarian.data", 
                          col_names = c("Age","Sex", "Chest_pain_type",
                                        "Resting_blood_pressure","Serum_cholestoral",
                                        "Fasting_blood_sugar","Resting_electrocardiographic",
                                        "Maximum_heart_rate_achieved","Exercise_induced_angina",
                                        "ST_depression_induced_by_exercise",
                                        "The_slope_of_the_peak_exercise_ST_segment",
                                        "Number_of_major_vessels_colored_by_flourosopy",
                                        "Thal","diagnosis_of_heart_disease"),na = "?")

Switzerland_raw <- read_csv("data/_raw/processed.switzerland.data", 
                          col_names = c("Age","Sex", "Chest_pain_type",
                                        "Resting_blood_pressure","Serum_cholestoral",
                                        "Fasting_blood_sugar","Resting_electrocardiographic",
                                        "Maximum_heart_rate_achieved","Exercise_induced_angina",
                                        "ST_depression_induced_by_exercise",
                                        "The_slope_of_the_peak_exercise_ST_segment",
                                        "Number_of_major_vessels_colored_by_flourosopy",
                                        "Thal","diagnosis_of_heart_disease"),na = "?")


Long_Beach_VA_raw <- read_csv("data/_raw/processed.va.data", 
                            col_names = c("Age","Sex", "Chest_pain_type",
                                          "Resting_blood_pressure","Serum_cholestoral",
                                          "Fasting_blood_sugar","Resting_electrocardiographic",
                                          "Maximum_heart_rate_achieved","Exercise_induced_angina",
                                          "ST_depression_induced_by_exercise",
                                          "The_slope_of_the_peak_exercise_ST_segment",
                                          "Number_of_major_vessels_colored_by_flourosopy",
                                          "Thal","diagnosis_of_heart_disease"),na = "?")


# Wrangle data ------------------------------------------------------------
Cleveland_raw <- Cleveland_raw %>%
  mutate(Location = "Cleveland")

Hungarian_raw <- Hungarian_raw %>%
  mutate(Location = "Hungarian")

Switzerland_raw <- Switzerland_raw %>%
  mutate(Location = "Switzerland")

Long_Beach_VA_raw <- Long_Beach_VA_raw %>%
  mutate(Location = "Long Beach")

# Write data --------------------------------------------------------------
write_tsv(x = Cleveland_raw,file = "data/Cleveland.tsv.gz")
write_tsv(x = Hungarian_raw,file = "data/Hungarian.tsv.gz")
write_tsv(x = Switzerland_raw,file = "data/Switzerland.tsv.gz")
write_tsv(x = Long_Beach_VA_raw,file = "data/Long_Beach.tsv.gz")

