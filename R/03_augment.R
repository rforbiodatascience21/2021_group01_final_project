# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/02_data.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data_aug <- Data %>%
  mutate(Country_Name = case_when(Country == "T" ~ "Tanzania", Country == "V" ~ "Vietnam"),
         Latrine_Depth_cm = Latrine_Depth*20)

# Write data --------------------------------------------------------------
write_tsv(x = Data_aug,
          file = "data/03_data_aug.tsv.gz")
