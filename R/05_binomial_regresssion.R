# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Functions ---------------------------------------------------------------
source(file = "R/99_functions.R")


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("rlang")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

Plot1 <- Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_density(alpha=0.5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")


Plot2 <- Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_histogram(position="dodge",binwidth = 5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")


Plot3 <- Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_density(alpha=0.5)+
  facet_grid(~ Diagnosis_of_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")

# Data wrangling ---------------------------------------------------------------

#To get around the problem with the different data sizes, 
# we remove the rows containing NA

Data <- Data %>%
  mutate(id = row_number(),
         Diagnosis_of_disease_No = case_when(Diagnosis_of_disease == "Present" ~ 1, Diagnosis_of_disease == "Not present" ~ 0))

Data_model <- Data %>%
  na.omit(Age+Sex+Chest_pain_type) %>%
  select(is.numeric) %>%
  select(!diagnosis_of_heart_disease) %>%
  sample_frac(.70)

Data_test <- Data %>%
  na.omit(Age+Sex+Chest_pain_type) %>%
  select(is.numeric) %>%
  select(!diagnosis_of_heart_disease) %>%
  anti_join(Data_model, by="id")

lmmodel1 <- Data_model %>%
  glm(formula = Diagnosis_of_disease_No ~ Age,
      family = binomial())

lmmodel2 <- Data_model %>%
  glm(formula = Diagnosis_of_disease_No ~ Age+Sex+Chest_pain_type,
      family = binomial())
tidy(lmmodel2)
AIC(lmmodel2)


Matrix_conf<- Confusion_matrix(lmmodel1, Data_test)%>%
  mutate(Model = "lmmodel1",
         Model_formular = "Diagnosis_of_disease_No ~ Age")


Matrix_conf1<- Confusion_matrix(lmmodel2, Data_test)%>%
  mutate(Model = "lmmodel2",
         Model_formular = "Diagnosis_of_disease_No ~ Age+Sex+Chest_pain_type")


Matrix_conf<-Matrix_conf %>%
  bind_rows(Matrix_conf1)
  

Plot4 <- Data %>%
  ggplot(aes(Age, Diagnosis_of_disease_No))+
  geom_point(alpha=.5)+
  stat_smooth(method="glm", se=FALSE, fullrange=TRUE, 
              method.args = list(family=binomial))+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(title = "Logistic regression model of the diagnosis of the Disease", y = "Predicted probability")


# Write data --------------------------------------------------------------
save(x = Matrix_conf,
          file = "results/Matrix_conf.RData")
ggsave(Plot1, filename = "results/07_plot_density_of_disease.png", width = 16, height = 9, dpi = 72)
ggsave(Plot2, filename = "results/07_plot_histogram_of_disease.png", width = 16, height = 9, dpi = 72)
ggsave(Plot3, filename = "results/07_plot_density_of_disease_mutated.png", width = 16, height = 9, dpi = 72)
ggsave(Plot4, filename = "results/07_plot_binomial_regression.png", width = 16, height = 9, dpi = 72)
