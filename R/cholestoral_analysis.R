# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
# Location cholesterol plot
Location_cholesterol_plot <- Data_aug %>%
  distinct(Location, Serum_cholestoral) %>%
  drop_na(Serum_cholestoral) %>%
  ggplot(mapping = aes(x = Serum_cholestoral,
                       y = Location)) +
  geom_boxplot(aes(fill = Location)) +
  ggtitle("Cholesterol_location plot") +
  theme_classic() +
  theme(legend.position = "none" )

mean_cholesterol <- Data_aug %>% 
  distinct(Location, Serum_cholestoral) %>% 
  drop_na(Serum_cholestoral) %>% 
  group_by(Location) %>% 
  summarise(mean = round(mean(Serum_cholestoral)))

mean_string <- format_tsv(
  mean_cholesterol,
  na = "NA",
  append = FALSE,
  quote_escape = "double",
  eol = "\n")

Location_cholesterol_plot + annotate("text", x = 550, y = 3, label = mean_string)

# Location and heart disease plot
# Here the serum cholesterol is also excluded since this is what is compared.
Location_disease_plot <- Data_aug %>%
  distinct(Location, Diagnosis_of_disease ,Serum_cholestoral) %>%
  drop_na(Serum_cholestoral) %>%
  ggplot(aes(y = Location,
             fill = Diagnosis_of_disease)) +
  geom_bar(position = position_dodge()) +
  theme_classic()

Diagnosis_percentage <- Data_aug %>% 
  distinct(Location, Serum_cholestoral) %>% 
  drop_na(Serum_cholestoral) %>% 
  group_by(Location) %>% 
  summarise(mean = round(mean(Serum_cholestoral)))

ggarrange(Location_disease_plot, 
          common.legend = TRUE)
