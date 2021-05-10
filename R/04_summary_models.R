# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(GGally)


# Load data ---------------------------------------------------------------
Data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz", col_types = cols(
  Age = col_double(),
  Resting_blood_pressure = col_double(),
  Serum_cholestoral = col_double(),
  Maximum_heart_rate_achieved = col_double(),
  ST_depression_induced_by_exercise = col_double()))

# Plot data ---------------------------------------------------------------
#Correlation plot of all variables, a bit messy, but gives an ovrview of the
#whole dataset
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

ggsave(NA_Bar, filename="/cloud/project/results/04_NA_Bar.png", width = 16, height = 9, dpi = 72)


# Summary density plots---------------------------------------------
pltchol = Data_aug %>% ggplot(aes(Serum_cholestoral)) + 
  geom_density(aes(color = Diagnosis_of_disease, fill = Diagnosis_of_disease),alpha=0.2) + 
  labs(x = "Serum Cholestoral",
       color = "Diagnosis",
       fill = "Diagnosis") + 
  theme_minimal()

pltblood = Data_aug %>% ggplot(aes(Resting_blood_pressure)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "Resting blood pressure",
       color = "Diagnosis",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

pltage = Data_aug %>% ggplot(aes(Age)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "Age",
       color = "Diagnosis",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

pltmax = Data_aug %>% ggplot(aes(Maximum_heart_rate_achieved)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "Maximum heart rate achieved",
       color = "Diagnosis",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

pltST = Data_aug %>% ggplot(aes(ST_depression_induced_by_exercise)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "ST depression",
       color = "Diagnosis",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

pltnum = Data_aug %>% ggplot(aes(Number_of_major_vessels_colored_by_flourosopy)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "Major vessels colored",
       color = "Diagnosis",
       fill = "Diagnosis") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

Summary_density = ggarrange(pltchol,                                                 
                            pltblood,
                            pltage,
                            pltmax,
                            pltST,
                            pltnum,
                            common.legend = TRUE) 

ggsave(Summary_density, filename="/cloud/project/results/04_Summary_density_plots.png", width = 16, height = 9, dpi = 72)

#Summary bar plots ---------------------------------------------
datadist_sex <- Data_aug %>% group_by(Sex_cat) %>% 
  count(Diagnosis_of_disease) 

plot_sex = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Sex_cat) %>%
  ggplot(aes(y = Sex_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Sex",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_chest = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Chest_pain_type_cat) %>%
  ggplot(aes(y = Chest_pain_type_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Chest pain type",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_sugar = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Fasting_blood_sugar_cat) %>%
  ggplot(aes(y = Fasting_blood_sugar_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Fasting blood sugar",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_electro = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Resting_electrocardiographic_cat) %>%
  ggplot(aes(y = Resting_electrocardiographic_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Electrocardiographic",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_exercise = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Exercise_induced_angina_cat) %>%
  ggplot(aes(y = Exercise_induced_angina_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Exercise induced angina",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_ST = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(slope_of_ST_cat) %>%
  ggplot(aes(y = slope_of_ST_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Slope of ST",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

plot_Thal = Data_aug  %>% group_by(Diagnosis_of_disease) %>% count(Thal_cat) %>%
  ggplot(aes(y = Thal_cat, x=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.2,
           position = "dodge") + 
  labs(x = "Thal",
       y = "",
       color = "Diagnosis of disease",
       fill = "Diagnosis of disease")

Summary_bar = ggarrange(plot_sex,                                                 
                        plot_chest,
                        plot_sugar,
                        plot_electro,
                        plot_exercise,
                        plot_ST,
                        plot_Thal,
                        common.legend = TRUE) 

ggsave(Summary_bar, filename="/cloud/project/results/04_Summary_bar_plots.png", width = 16, height = 9, dpi = 72)

#Summary table of data--------------------------------------------------------
# summary overview of attributes
Summary_table <- Data_aug %>% 
  select(Age, 
         Resting_blood_pressure, 
         Serum_cholestoral,
         Maximum_heart_rate_achieved,
         ST_depression_induced_by_exercise) %>% 
  summarise_all(funs(mean, sd, min,max),na.rm = TRUE) %>% 
  gather(key = key, value = value) %>% 
  separate(key, into = c("type", "stat"), sep = "_(?=[^_]+$)") %>% 
  spread(key = stat, value = value) %>% 
  mutate("mean w. sd" = paste0(round(mean, 2), " (", intToUtf8("177"), round(sd, 2), ")")) %>% 
  select(type, 'mean w. sd', min, max) 

save(x = Summary_table,
     file = "/cloud/project/results/04_Summary_table.RData")
