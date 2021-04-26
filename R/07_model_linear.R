# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------
Data <- Data %>%
  mutate(Country_binary = case_when(Country == "T" ~ 0, Country == "V" ~ 1),
         Taxa_factor = factor(Taxa) ) %>%
  select(!c(Country,Latrine_Depth, Country_Name,Taxa), )
  

# Model data ------------------------------------------------------------
lmmodel1 <- Data %>%
  select_if(is.numeric) %>%
  glm(Country_binary ~ .,
      data = .,
      family = binomial(link = "logit"))

summary(lmmodel1)
drop1(lmmodel1,test="F")


# Write data --------------------------------------------------------------
#write_tsv(x = ,file = )
#write_tsv(x = ,file = )
