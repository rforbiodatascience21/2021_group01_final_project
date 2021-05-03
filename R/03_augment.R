# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/02_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data_aug <- Data %>%
  mutate(Location_num = case_when(Location == "Cleveland" ~ 0, Location == "Switzerland" ~ 1, Location == "Hungarian" ~ 2, Location == "Long Beach" ~ 3),
         Diagnosis_of_disease = case_when(diagnosis_of_heart_disease == 0 ~ "Not present", diagnosis_of_heart_disease >= 1 ~ "Present"))

# Write data --------------------------------------------------------------
write_tsv(x = Data_aug,
          file = "data/03_data_aug.tsv.gz")
