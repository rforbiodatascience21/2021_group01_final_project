# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(GGally)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
Corr_plot <- ggpairs(Data_aug,
                     mapping = aes(colou = Country_Name),
                     columns = c('pH', 'Temp', 'TS', 'VS', 'VFA', 'CODt', 'CODs', 
                                 'perCODsbyt', 'NH4', 'Prot', 'Carbo', 'Latrine_Depth_cm'),
                     columnLabels = c('pH', 'Temp', 'TS', 'VS', 'VFA', 'CODt', 'CODs', 
                                 'perCODsbyt', 'NH4', 'Prot', 'Carbo', 'Latrine depth')) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Correlation plots of of environmental factors by country")
Corr_plot
