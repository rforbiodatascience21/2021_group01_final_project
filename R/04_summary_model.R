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
                     mapping = aes(colour = Diagnosis_of_disease),
                     columns = c("Age","Sex_cat", "Chest_pain_type_cat",
                                 "Resting_blood_pressure","Serum_cholestoral",
                                 "Fasting_blood_sugar_cat","Resting_electrocardiographic_cat",
                                 "Maximum_heart_rate_achieved","Exercise_induced_angina_cat",
                                 "ST_depression_induced_by_exercise",
                                 "The_slope_of_the_peak_exercise_ST_segment",
                                 "Diagnosis_of_disease"),
                     columnLabels = c("Age","Sex", "Chest pain type",
                                 "Resting blood pressure","Serum cholestoral",
                                 "Fasting blood sugar","Resting electrocardiographic",
                                 "Maximum heart rate","Exercise induced angina",
                                 "ST depression",
                                 "slope of ST segment",
                                 "Diagnosis"),
                     upper = list(continuous = "blank", combo = "blank", discrete = "blank", na = "na"),
                     lower = list(continuous = "points", combo = "box_no_facet", discrete = "ratio", na = "na"),
                     labeller = label_wrap_gen(10)) +
  labs(title = "Correlation plots of predictive variables stratified on diagnosis") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Corr_plot
png(filename="/cloud/project/results/Summary_correlation_plots.png",  width = 1000, height = 800)
Corr_plot
dev.off()


# NA plots---------------------------------
plt1 <- Data_aug %>% mutate(Age = case_when(!is.na(Age) ~ "Not NA")) %>%
  count(Age) %>%
  ggplot(aes(x = Age, y=n)) +
  geom_col() + 
  labs(x = "Age",
       y = "")

plt2 <- Data_aug %>% mutate(Sex = case_when(!is.na(Sex) ~ "Not NA")) %>%
  count(Sex) %>%
  ggplot(aes(x = Sex, y=n)) +
  geom_col() + 
  labs(x = "Sex",
       y = "")

plt3 <- Data_aug %>% mutate(Chest_pain_type = case_when(!is.na(Chest_pain_type) ~ "Not NA")) %>%
  count(Chest_pain_type) %>%
  ggplot(aes(x = Chest_pain_type, y=n)) +
  geom_col() + 
  labs(x = "Chest pain type",
       y = "")

plt4 <- Data_aug %>% mutate(Resting_blood_pressure = case_when(!is.na(Resting_blood_pressure) ~ "Not NA")) %>%
  count(Resting_blood_pressure) %>%
  ggplot(aes(x = Resting_blood_pressure, y=n)) +
  geom_col() + 
  labs(x = "Resting blood pressure",
       y = "")

plt5 <- Data_aug %>% mutate(Serum_cholestoral = case_when(!is.na(Serum_cholestoral) ~ "Not NA")) %>%
  count(Serum_cholestoral) %>%
  ggplot(aes(x = Serum_cholestoral, y=n)) +
  geom_col() + 
  labs(x = "Serum cholestoral",
       y = "")

plt6 <- Data_aug %>% mutate(Fasting_blood_sugar = case_when(!is.na(Fasting_blood_sugar) ~ "Not NA")) %>%
  count(Fasting_blood_sugar) %>%
  ggplot(aes(x = Fasting_blood_sugar, y=n)) +
  geom_col() + 
  labs(x = "Fasting blood sugar",
       y = "")

plt7 <- Data_aug %>% mutate(Resting_electrocardiographic = case_when(!is.na(Resting_electrocardiographic) ~ "Not NA")) %>%
  count(Resting_electrocardiographic) %>%
  ggplot(aes(x = Resting_electrocardiographic, y=n)) +
  geom_col() + 
  labs(x = "Electrocardiographic",
       y = "")

plt8 <- Data_aug %>% mutate(Maximum_heart_rate_achieved = case_when(!is.na(Maximum_heart_rate_achieved) ~ "Not NA")) %>%
  count(Maximum_heart_rate_achieved) %>%
  ggplot(aes(x = Maximum_heart_rate_achieved, y=n)) +
  geom_col() + 
  labs(x = "Maximum heart rate",
       y = "")

plt9 <- Data_aug %>% mutate(Exercise_induced_angina = case_when(!is.na(Exercise_induced_angina) ~ "Not NA")) %>%
  count(Exercise_induced_angina) %>%
  ggplot(aes(x = Exercise_induced_angina, y=n)) +
  geom_col() + 
  labs(x = "Exercise induced angina",
       y = "")

plt10 <- Data_aug %>% mutate(ST_depression_induced_by_exercise = case_when(!is.na(ST_depression_induced_by_exercise) ~ "Not NA")) %>%
  count(ST_depression_induced_by_exercise) %>%
  ggplot(aes(x = ST_depression_induced_by_exercise, y=n)) +
  geom_col() + 
  labs(x = "ST depression",
       y = "")

plt11 <- Data_aug %>% mutate(The_slope_of_the_peak_exercise_ST_segment = case_when(!is.na(The_slope_of_the_peak_exercise_ST_segment) ~ "Not NA")) %>%
  count(The_slope_of_the_peak_exercise_ST_segment) %>%
  ggplot(aes(x = The_slope_of_the_peak_exercise_ST_segment, y=n)) +
  geom_col() + 
  labs(x = "Slope of ST",
       y = "")

plt12 <- Data_aug %>% mutate(Number_of_major_vessels_colored_by_flourosopy = case_when(!is.na(Number_of_major_vessels_colored_by_flourosopy) ~ "Not NA")) %>%
  count(Number_of_major_vessels_colored_by_flourosopy) %>%
  ggplot(aes(x = Number_of_major_vessels_colored_by_flourosopy, y=n)) +
  geom_col() + 
  labs(x = "Flouroscopy",
       y = "")

plt13 <- Data_aug %>% mutate(Thal = case_when(!is.na(Thal) ~ "Not NA")) %>%
  count(Thal) %>%
  ggplot(aes(x = Thal, y=n)) +
  geom_col() + 
  labs(x = "Thal",
       y = "")

plt14 <- Data_aug %>% mutate(Diagnosis_of_disease = case_when(!is.na(Diagnosis_of_disease) ~ "Not NA")) %>%
  count(Diagnosis_of_disease) %>%
  ggplot(aes(x = Diagnosis_of_disease, y=n)) +
  geom_col() + 
  labs(x = "Diagnosis",
       y = "")

NA_Bar <- ggarrange(plt1, plt2, plt3, plt4, plt5, plt6, plt7,
                    plt8, plt9, plt10, plt11, plt12, plt13, plt14,
                    ncol = 7,
                    nrow = 2)

ggsave(NA_Bar, filename="/cloud/project/results/NA_Bar.png", width = 16, height = 9, dpi = 72)
