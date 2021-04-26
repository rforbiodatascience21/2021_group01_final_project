# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
Temp_plot <- Data_aug %>%
  distinct(Country_Name, Temp) %>%
  ggplot(mapping = aes(x = Temp,
                       y = Country_Name)) +
  geom_boxplot() + theme_classic()
Temp_plot
