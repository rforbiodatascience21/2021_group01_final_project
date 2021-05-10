# Clear workspace ---------------------------------------------------------
rm(list = ls())

#get Data ----------------------------------------------------------------------
Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

### Exploratory Data Analysis -------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(broom)
library(cowplot)
library(viridis)

#Checking the Data Distribution

datadist <- Data %>% group_by(Diagnosis_of_disease) %>% 
  summarise(count = n()) %>% 
  mutate(Percentage = count/sum(count)*100)

paste("Heart attack: ", datadist %>% 
        filter(Diagnosis_of_disease == "Present") %>% 
        select(Percentage) %>% round(2), "%") 

paste("No heart attack: ", datadist %>% 
        filter(Diagnosis_of_disease == "Not present") %>% 
        select(Percentage) %>% round(2), "%") 

Heart_attack_dist <- Data %>% 
  count(Diagnosis_of_disease) %>%
  ggplot(aes(x = Diagnosis_of_disease, 
             y = n,
             fill = Diagnosis_of_disease,
             color = Diagnosis_of_disease)) +
  geom_col(alpha = 0.5) + 
  labs(title = "Distribution of diagnosis") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

ggsave(Heart_attack_dist, filename="/cloud/project/results/07_Heart_attack_distribution.png", width = 16, height = 9, dpi = 72)


' observation: 
Heart Attack percentage is 54 and No Heart Attack percentage is 46. 
So, the dataset is balanced.
'

# Distribution of Sex with respect to Heart Disease
# Sex vs heart attack 
datadist_sex_present <- Data %>% filter(Diagnosis_of_disease == "Present") %>%
  group_by(Sex_cat) %>% 
  summarise(Percentage = n()) %>% 
  mutate(Percentage = Percentage/sum(Percentage)*100) 

sex_present_plt <- Data  %>% filter(Diagnosis_of_disease == "Present") %>% 
  count(Sex_cat) %>%
  ggplot(aes(x = n, y = Sex_cat,
             color = Sex_cat,
             fill = Sex_cat)) +
  geom_col(alpha=0.5) + 
  labs(title = "Gender vs. heart attack",
       y = "Sex",
       fill = "Sex",
       color = "Sex") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

sex_annotated <- sex_present_plt + 
  annotate("text", 
           x = 500, 
           y = 2, 
           label = datadist_sex_present %>% 
             filter(Sex_cat == "Male") %>% 
             select(Percentage) %>% 
             round(2)) + 
  annotate("text", 
           x = 500, 
           y = 1, 
           label = datadist_sex_present %>% 
             filter(Sex_cat == "Female") %>% 
             select(Percentage) %>% 
             round(2))

ggsave(sex_annotated, filename="/cloud/project/results/07_sex_present.png", width = 16, height = 9, dpi = 72)

'
The Heart attack percentage for females is around 10 % and for males is around 90% i.e. 
males have more chances for having Heart Attack
'

# Distribution of age wrt heart disease 
pltbox = Data %>% 
  ggplot(mapping = aes(x = Age,
                       y=Diagnosis_of_disease, 
                       fill = Diagnosis_of_disease,
                       color = Diagnosis_of_disease)) + 
  geom_boxplot(alpha = 0.5) + 
  xlim(0,80) +
  labs(x = "Age", y="") + 
  theme_minimal() + 
  theme(axis.title=element_blank(), 
        axis.text.y=element_blank()) + 
  labs(title = "Distribution of age Pain w.r.t heart attack") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

pltbar = Data %>% 
  ggplot(aes(Age)) + 
  geom_freqpoly(aes(color = Diagnosis_of_disease),
                alpha = 1) + 
  xlim(0,80) +
  theme_minimal() +
  scale_color_viridis(discrete = TRUE)

age <- ggarrange(pltbox,                                                 
          pltbar, 
          nrow = 2, common.legend = TRUE, 
          heights = c(1, 2), align = "v") 

ggsave(age, filename="/cloud/project/results/07_age.png", width = 16, height = 9, dpi = 72)


#Data %>%
#  ggplot(aes(x = Age, fill = Diagnosis_of_disease)) + geom_histogram(alpha=0.3,
#                 position = "dodge") + 
#  geom_freqpoly(aes(color = Diagnosis_of_disease,
#                    fill = Diagnosis_of_disease)) 


###### other 
Data = Data %>% 
  mutate(Age_class = case_when(Age < 20 ~ ">20",
                               20 <= Age & Age < 30 ~ "20-30",
                               30 <= Age & Age < 40 ~ "30-40",
                               40 <= Age & Age < 50 ~ "40-50",
                               50 <= Age & Age < 60 ~ "50-60",
                               60 <= Age & Age < 70 ~ "60-70",
                               70 <= Age & Age < 80 ~ "70-80",
                               80 <= Age ~ "80<"))

Data <- Data %>%
  mutate(Age_class = factor(Age_class, levels =  c("20-30", 
                                                   "30-40", 
                                                   "40-50",
                                                   "50-60",
                                                   "60-70",
                                                   "70-80",
                                                   "80<"
                                                   )))

Data %>% group_by(Age_class)  %>% 
  summarise(n = n()) 


'
Age_class     n
20-30         4
30-40        76
40-50       212
50-60       375
60-70       222
70-80        31
'
# age and disease presents
Data  %>% group_by(Diagnosis_of_disease)  %>% count(Age_class)

age_class <- Data  %>% group_by(Diagnosis_of_disease)  %>% count(Age_class) %>%
  ggplot(aes(x = Age_class, y=n)) +
  geom_col(aes(color = Diagnosis_of_disease,
               fill = Diagnosis_of_disease),
           alpha=0.5) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

ggsave(age_class, filename="/cloud/project/results/07_age_class.png", width = 16, height = 9, dpi = 72)

#### 
#Checking the distribution of Chest Pain with respect to Heart Disease

chest_dist_plt <- Data  %>% group_by(Diagnosis_of_disease) %>% 
  count(Chest_pain_type_cat) %>%
  ggplot(aes(x = Chest_pain_type_cat, y=n, 
             fill = Diagnosis_of_disease,
             color = Diagnosis_of_disease)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5,
           alpha = 0.5) + 
  labs(title = "Distribution of Chest Pain w.r.t heart attack") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)


ggsave(chest_dist_plt, filename="/cloud/project/results/07_chest_dist_plt.png", width = 16, height = 9, dpi = 72)


# chest pain vs heart attack percentage 
datadist_chest_present <- Data %>% filter(Diagnosis_of_disease =="Present") %>%
  group_by(Chest_pain_type_cat) %>% 
  summarise(Percentage = n()) %>% 
  mutate(Percentage = Percentage/sum(Percentage)*100)

chest_present_plt <- datadist_chest_present  %>%
  ggplot(aes(x = Chest_pain_type_cat,
             y = Percentage,
             color = Chest_pain_type_cat,
             fill = Chest_pain_type_cat)) +
  geom_col(alpha = 0.5) + 
  labs(title = "Chest pain vs heart attack percentage") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

ggsave(chest_present_plt, filename="/cloud/project/results/07_chest_present_plt.png", width = 16, height = 9, dpi = 72)


'
Value 1: typical angina
Value 2: atypical angina
Value 3: non-anginal pain
Value 4: asymptomatic

People with chest pain type 4 (asymptomatic) are more 
prone to heart attack when compared to other chest pain categories.
'

# cholesterole values 
# normal range is <200 mg/dL. 

Data %>% filter(Serum_cholestoral > 200) %>% count(Diagnosis_of_disease) 

'
Diagnosis_of_disease     n for chol > 200
Not present            290
Present                296
'

sex_v_cholestoral <- Data %>% ggplot(mapping = aes(x = Sex_cat,
                              y = Serum_cholestoral,
                              fill = Diagnosis_of_disease)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_viridis(discrete = TRUE)

ggsave(sex_v_cholestoral, filename="/cloud/project/results/07_sex_v_cholestoral.png", width = 16, height = 9, dpi = 72)



