# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
ENV <- read_tsv(file = "data/01_ENV.tsv.gz")
SPE <- read_tsv(file = "data/01_SPE.tsv.gz")


# Wrangle data ------------------------------------------------------------
Data <- SPE %>%
  pivot_longer(!Taxa,names_to = "Samples",values_to = "Value") %>%
  full_join(ENV,by="Samples") %>%
  separate(Samples, c("Country", "Latrine_No","Latrine_Depth"),"\\_")


# Write data --------------------------------------------------------------
write_tsv(x = Data,
          file = "data/02_data.tsv.gz")
