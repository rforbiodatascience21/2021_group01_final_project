# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
Heart_cleve_raw <- read_file(file = "data/_raw/cleveland.data")

# Wrangle data ------------------------------------------------------------
# change placment of \n

Data <- Heart_cleve_raw %>% 
  str_split("\n") 

class(Data)

# Write data --------------------------------------------------------------

write_tsv(x = Data,file = "data/01_Data_temp.tsv.gz",delim =" ")
