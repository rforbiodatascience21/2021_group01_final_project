# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
Temp_plot <- Data_aug %>%
  distinct(Location, Resting_blood_pressure) %>%
  ggplot(mapping = aes(x = Resting_blood_pressure,
                       y = Location)) +
  geom_boxplot() + theme_classic()
Temp_plot
