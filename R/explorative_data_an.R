
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

###### distribution of age wrt heart disease 
pltbox = Data %>% 
  ggplot(mapping = aes(x = Age,
                       y=Diagnosis_of_disease, 
                       fill = Diagnosis_of_disease)) + 
  geom_boxplot() + 
  xlim(0,80) +
  labs(x = "Age", y=" ") + 
  theme_minimal() + 
  theme(axis.title=element_blank(), 
        axis.text.y=element_blank()) 

pltbar = Data %>% 
  ggplot(aes(Age)) + 
  geom_freqpoly(aes(color = Diagnosis_of_disease,
                    fill = Diagnosis_of_disease)) + 
  xlim(0,80) +
  theme_minimal()

ggarrange(pltbox,                                                 # First row with scatter plot
          pltbar, # Second row with box and dot plots
          nrow = 2, common.legend = TRUE, 
          heights = c(1, 2), align = "v") 

#pltbar=Data %>%
#  ggplot(aes(x = Age, fill = Diagnosis_of_disease)) + geom_histogram(aes(color = Diagnosis_of_disease), alpha=0.3,
#                 position = "dodge") 

###### distribution of of Blood pressure and cholestrol w.r.t Heart Disease

# heart attack distribution over cholesterole 
pltchol = Data %>% ggplot(aes(Serum_cholestoral)) + 
  geom_density(aes(color = Diagnosis_of_disease, fill = Diagnosis_of_disease),alpha=0.2) + 
  labs(x = "Serum cholesterole", y = "Density") + 
  theme_minimal()

pltblood = Data %>% ggplot(aes(Resting_blood_pressure)) + 
  geom_density(aes(color = Diagnosis_of_disease,fill = Diagnosis_of_disease),alpha=0.2) +
  labs(x = "Resting blood pressure") +
  theme_minimal() +
  theme(axis.title.y=element_blank())

ggarrange(pltchol,                                                 
          pltblood, 
          common.legend = TRUE) 
