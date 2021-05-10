# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Functions ---------------------------------------------------------------
source(file = "R/99_functions.R")


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("rlang")
library("viridis")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz", 
                 col_types = cols(.default = col_double(),
                                  Location = col_character(),
                                  Diagnosis_of_disease = col_character(),
                                  Sex_cat = col_character(),
                                  Chest_pain_type_cat = col_character(),
                                  Fasting_blood_sugar_cat = col_character(),
                                  Resting_electrocardiographic_cat = 
                                    col_character(),
                                  Exercise_induced_angina_cat = col_character(),
                                  slope_of_ST_cat = col_character(),
                                  Thal_cat = col_character(),
                                  Age_class = col_character()))

Plot1 <- Data %>%
  ggplot(aes(Age, fill = Location)) +
  geom_density(alpha = 0.5) +
  facet_grid(~ diagnosis_of_heart_disease) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(color = "Location", title =
         "Density plot of the age distribution for each decree of heart disease", 
       subtitle = "0 corresponse to no degree of heart disease" ) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

Plot2 <- Data %>%
  ggplot(aes(Age, fill = Location, color = Location)) +
  geom_histogram(position = "dodge", binwidth = 5, alpha = 0.5) +
  facet_grid(~ diagnosis_of_heart_disease) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(fill = "Location", color = "Location") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)


Plot3 <- Data %>%
  ggplot(aes(Age, fill = Location, color = Location)) +
  geom_density(alpha = 0.5) +
  facet_grid(~ Diagnosis_of_disease) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(color = "Location", fill = "Location") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

# Data wrangling ---------------------------------------------------------------

#To get around the problem with the different data sizes, 
# we remove the rows containing NA

Data <- Data_selection(data = Data,
                       var = c("Diagnosis_of_disease_No",
                               "Age",
                               "Sex",
                               "Chest_pain_type", 
                               "id"), 
                       rm_na = TRUE) %>%
  mutate(Diagnosis_of_disease_No = factor(Diagnosis_of_disease_No))


Data_model <- Data %>%
  sample_frac(.70)

Data_test <- Data %>%
  anti_join(Data_model, by = "id")

set.seed(365)

lmmodel1 <- Data_model %>%
  glm(formula =  Diagnosis_of_disease_No~ Age,
      family = binomial())
tidy(lmmodel1)
AIC(lmmodel1)


lmmodel2 <- Data_model %>%
  glm(formula = Diagnosis_of_disease_No ~ Age+Sex+Chest_pain_type,
      family = binomial())
tidy(lmmodel2)
AIC(lmmodel2)


Matrix_conf<- Confusion_matrix(lmmodel1, Data_test) %>%
  mutate(Model = "lmmodel1",
         Model_formular = "Diagnosis_of_disease_No ~ Age")


Matrix_conf1<- Confusion_matrix(lmmodel2, Data_test) %>%
  mutate(Model = "lmmodel2",
         Model_formular = "Diagnosis_of_disease_No ~ Age+Sex+Chest_pain_type")


Matrix_conf<-Matrix_conf %>%
  bind_rows(Matrix_conf1)
  
Data <- Data %>%mutate(Predict1 = predict(lmmodel1, Data, type = "response"),
                       Predict2 = predict(lmmodel2, Data, type = "response"),
                       Diagnosis_of_disease_No = 
                         as_double(Diagnosis_of_disease_No)-1)

Plot4 <- Data %>%
  ggplot(aes(Age, y = Diagnosis_of_disease_No)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = (Predict1))) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Logistic regression model of the diagnosis of the Disease", 
       y = "Predicted probability") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)


# Write data --------------------------------------------------------------
save(x = Matrix_conf,
          file = "results/05_Matrix_conf.RData")
ggsave(Plot1, filename = "results/05_plot_density_of_disease.png", 
       width = 16, height = 9, dpi = 72)
ggsave(Plot2, filename = "results/05_plot_histogram_of_disease.png", 
       width = 16, height = 9, dpi = 72)
ggsave(Plot3, filename = "results/05_plot_density_of_disease_mutated.png", 
       width = 16, height = 9, dpi = 72)
ggsave(Plot4, filename = "results/05_plot_binomial_regression.png", 
       width = 16, height = 9, dpi = 72)
