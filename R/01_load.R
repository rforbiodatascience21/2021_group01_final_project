# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Download data -----------------------------------------------------------
if (!file.exists("processed.cleveland.data")) {
  download.file(url = "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data", destfile = "processed.cleveland.data")
}
require(tools)
md5sum("processed.cleveland.data")


# Load data ---------------------------------------------------------------
Cleveland_raw <- read.csv("processed.cleveland.data", header = FALSE)

# Wrangle data ------------------------------------------------------------
Cleveland_raw <- Cleveland_raw %>%
  as_tibble()


# Write data --------------------------------------------------------------
write_tsv(x = Cleveland_raw,file = "data/Cleveland.tsv.gz")
