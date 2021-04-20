# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
SPE_raw <- read_csv(file = "data/_raw/SPE_pitlatrine.csv",col_names = TRUE)
ENV_raw <- read_csv(file = "data/_raw/ENV_pitlatrine.csv",col_names = TRUE)

# Wrangle data ------------------------------------------------------------
SPE_raw <- SPE_raw %>%
  as_tibble()

ENV_raw <- ENV_raw %>%
  as_tibble()

# Write data --------------------------------------------------------------
write_tsv(x = SPE_raw,file = "data/01_SPE.tsv")
write_tsv(x = ENV_raw,file = "data/01_ENV.tsv")
