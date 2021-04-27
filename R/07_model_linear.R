# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Model data ------------------------------------------------------------
lmmodel1 <- Data %>%
  select_if(is.numeric) %>%
  glm(formula = diagnosis_of_heart_disease ~ Age + Sex + Chest_pain_type + Resting_blood_pressure 
      + Serum_cholestoral + Fasting_blood_sugar + Resting_electrocardiographic + 
        Maximum_heart_rate_achieved + Exercise_induced_angina + ST_depression_induced_by_exercise +
        The_slope_of_the_peak_exercise_ST_segment + Number_of_major_vessels_colored_by_flourosopy +
        Thal + Location_num,
      family = gaussian)

summary(lmmodel1)


# Write data --------------------------------------------------------------
#write_tsv(x = ,file = )
#write_tsv(x = ,file = )
