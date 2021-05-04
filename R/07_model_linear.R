# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("broom")


# Load data ---------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_density(alpha=0.5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")


Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_histogram(position="dodge",binwidth = 5)+
  facet_grid(~ diagnosis_of_heart_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")


Data %>%
  ggplot(aes(Age, fill = as.factor(Location)))+
  geom_density(alpha=0.5)+
  facet_grid(~ Diagnosis_of_disease)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(color="Location")

# Data wrangling ---------------------------------------------------------------

#To get around the problem with the different data sizes, 
# we remove the rows containing NA

Data_model <- Data %>%
  na.omit() %>%
  select(Diagnosis_of_disease,is.numeric) %>%
  select(!diagnosis_of_heart_disease)


lmmodel1 <- Data_model %>%
  glm(formula = as.factor(Diagnosis_of_disease) ~ Age,
      family = binomial())

Data_model <- Data_model %>%
  bind_cols(predict(lmmodel1,Data_model, type = "link",se.fit = TRUE))

Data_model %>%
  ggplot(aes(Age,as.factor(Diagnosis_of_disease)))+
  geom_point()



plot(lmmodel1)

summary(lmmodel1)

tidy(lmmodel1)

# Write data --------------------------------------------------------------
#write_tsv(x = ,file = )
#write_tsv(x = ,file = )
