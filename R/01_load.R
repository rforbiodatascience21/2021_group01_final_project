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
<<<<<<< HEAD
Heart_cleve_raw <- read_file(file = "data/_raw/cleveland.data")

# Wrangle data ------------------------------------------------------------
# change placment of \n

Data <- Heart_cleve_raw %>% 
  str_split("\n") 

class(Data)

# Write data --------------------------------------------------------------

write_tsv(x = Data,file = "data/01_Data_temp.tsv.gz",delim =" ")
=======
Cleveland_raw <- read.csv("processed.cleveland.data", header = FALSE)

# Wrangle data ------------------------------------------------------------
Cleveland_raw <- Cleveland_raw %>%
  as_tibble()


# Write data --------------------------------------------------------------
write_tsv(x = Cleveland_raw,file = "data/Cleveland.tsv.gz")
>>>>>>> 9a2ab9295997b11482110a4da104451c8af603d2
