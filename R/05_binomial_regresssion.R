# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Functions ---------------------------------------------------------------
source(file = "R/99_functions.R")


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")
library("rlang")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz", col_types = cols(.default = col_double(),
                                                        Location = col_factor(),
                                                        Diagnosis_of_disease = col_factor(),
                                                        Sex_cat = col_factor(),
                                                        Chest_pain_type_cat = col_factor(),
                                                        Fasting_blood_sugar_cat = col_factor(),
                                                        Resting_electrocardiographic_cat = col_factor(),
                                                        Exercise_induced_angina_cat = col_factor(),
                                                        slope_of_ST_cat = col_factor(),
                                                        Thal_cat = col_factor(),
                                                        Age_class = col_factor()))

Plot1 <- Data %>%
  ggplot(aes(Age, fill = Location))+
  geom_density(alpha=0.5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location", title = "Density plot of the age distribution for each decree of heart disease", subtitle = "0 corresponse to no degree of heart disease" )


Plot2 <- Data %>%
  ggplot(aes(Age, fill = Location))+
  geom_histogram(position="dodge",binwidth = 5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")


Plot3 <- Data %>%
  ggplot(aes(Age, fill = Location))+
  geom_density(alpha=0.5)+
  facet_grid(~ Diagnosis_of_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")

# Data wrangling ---------------------------------------------------------------

#To get around the problem with the different data sizes, 
# we remove the rows containing NA
Data <- Data %>%
  mutate(Diagnosis_of_disease_No=factor(Diagnosis_of_disease_No))%>%
  select(Diagnosis_of_disease_No,Age,Sex,Chest_pain_type, id) %>%
  na.omit()

Data_model <- Data %>%
  sample_frac(.70)

Data_test <- Data %>%
  anti_join(Data_model, by="id")

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
