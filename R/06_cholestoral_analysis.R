# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)

# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz")

# Plot data ---------------------------------------------------------------
# Location cholesterol plot
Location_cholesterol_plot <- Data_aug %>%
  select(Location, Serum_cholestoral) %>%
  drop_na(Serum_cholestoral) %>%
  ggplot(mapping = aes(x = Serum_cholestoral,
                       y = Location)) +
  geom_boxplot(aes(fill = Location)) +
  theme_classic() +
  theme(legend.position = "none" )

mean_cholesterol <- Data_aug %>% 
  select(Location, Serum_cholestoral) %>% 
  drop_na(Serum_cholestoral) %>% 
  group_by(Location) %>% 
  summarise(mean = round(mean(Serum_cholestoral)))

mean_string <- format_tsv(
  mean_cholesterol,
  na = "NA",
  append = FALSE,
  quote_escape = "double",
  eol = "\n")

# Location and heart disease plot
# Here the serum cholesterol is also excluded since this is what is compared.
Location_disease_plot <- Data_aug %>%
  select(Location, Diagnosis_of_disease ,Serum_cholestoral) %>%
  drop_na(Serum_cholestoral) %>%
  ggplot(aes(y = Location,
             fill = Diagnosis_of_disease)) +
  geom_bar(position = position_dodge(),color = "black") +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Grey = people with disease not present\n Orange = people with disease present")

Diagnosis_percentage <- Data_aug %>% 
  select(Location, Serum_cholestoral,Diagnosis_of_disease) %>% 
  drop_na(Serum_cholestoral) %>% 
  group_by(Location) %>% 
  count(Diagnosis_of_disease)

Diagnosis_percentage
#(97/(104+97))*100
#(80/(112+80))*100
#(77/(34+77))*100
diagnosis_string = "% of disease present\n Cleveland\t48.26%\n Hungarian\t41.67%\n Long Beach\t69.37%"

# Make annotated plots
Location_cholesterol_plot <- Location_cholesterol_plot + annotate("text", x = 550, y = 3, label = mean_string)
Location_disease_plot <- Location_disease_plot + annotate("text", x = 100, y = 3, label = diagnosis_string)
combined_plot <- ggarrange(Location_cholesterol_plot,Location_disease_plot)
combined_plot
Location_plot <- annotate_figure(combined_plot,
                top = ("Locations relation with cholesterol and heart disease"))

ggsave(Location_plot, filename="/cloud/project/results/Location_plot.png", width = 16, height = 9, dpi = 72)

