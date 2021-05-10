# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)

# Functions ---------------------------------------------------------------
source(file = "R/99_functions.R")

# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------

Data <- Data_selection(data = Data_aug,var = c("Location","Serum_cholestoral","Diagnosis_of_disease"), rm_na = TRUE, rm_na_from = c("Serum_cholestoral")) 

Location_cholesterol_plot <- Data %>%
  ggplot(mapping = aes(x = Serum_cholestoral,
                       y = Location)) +
  geom_boxplot(aes(fill = Location),alpha = 0.5) +
  theme_classic() +
  theme(legend.position = "none" ) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE)

mean_cholesterol <- Data %>% 
  group_by(Location) %>% 
  summarise(mean = round(mean(Serum_cholestoral)))

mean_string <- format_tsv(
  mean_cholesterol,
  na = "NA",
  append = FALSE,
  quote_escape = "double",
  eol = "\n")

# Location and heart disease plot
Location_disease_plot <- Data %>%
  ggplot(aes(y = Location,
             fill = Diagnosis_of_disease)) +
  geom_bar(position = position_dodge(),alpha = 0.5) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Purple = people with disease not present
       Yellow = people with disease present") +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE)

datadist_diagnosis_present <- Data %>%
  group_by(Location,Diagnosis_of_disease) %>% 
  summarise(count = n()) %>% 
  mutate(Dis_percentage = round(count/sum(count)*100),4) %>%
  filter(Diagnosis_of_disease == "Present") %>%
  select(Dis_percentage)

diagnosis_present_string <- format_tsv(
  datadist_diagnosis_present,
  na = "NA",
  append = FALSE,
  quote_escape = "double",
  eol = "\n")

# Make annotated plots
Location_cholesterol_plot <- Location_cholesterol_plot + annotate("text", x = 550, y = 3, label = mean_string)
Location_disease_plot <- Location_disease_plot + annotate("text", x = 150, y = 3, label = diagnosis_present_string)
combined_plot <- ggarrange(Location_cholesterol_plot,Location_disease_plot)
combined_plot
Location_plot <- annotate_figure(combined_plot,
                top = ("Locations relation with cholesterol and heart disease"))

ggsave(Location_plot, filename="/cloud/project/results/06_Location_plot.png", width = 16, height = 9, dpi = 72)

