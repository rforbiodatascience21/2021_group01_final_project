# Clear workspace ---------------------------------------------------------
rm(list = ls())

library(broom)
library(tidyverse)

# Load augmented data
data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz", 
                 col_types = cols(.default = col_double(),
                                  Age = col_double(),
                                  Resting_blood_pressure = col_double(),
                                  Serum_cholestoral = col_double(),
                                  Maximum_heart_rate_achieved = col_double(),
                                  ST_depression_induced_by_exercise = col_double(), 
                                  Location = col_character(),
                                  Diagnosis_of_disease= col_character(), 
                                  Sex_cat = col_character(), 
                                  Chest_pain_type_cat = col_character(),
                                  Fasting_blood_sugar_cat = col_character(),
                                  Resting_electrocardiographic_cat = col_character(),
                                  Exercise_induced_angina_cat = col_character(),
                                  slope_of_ST_cat = col_character(),
                                  Thal_cat = col_character(),
                                  Age_class = col_character(), 
                                  diagnosis_of_heart_disease =col_character() )) %>% 
  select(Age,
         Resting_blood_pressure,
         Serum_cholestoral, 
         Maximum_heart_rate_achieved,
         ST_depression_induced_by_exercise, 
         Location, 
         Diagnosis_of_disease,
         Sex_cat, 
         Chest_pain_type_cat,
         Fasting_blood_sugar_cat,
         Resting_electrocardiographic_cat,
         slope_of_ST_cat,
         Thal_cat,
         Age_class, diagnosis_of_heart_disease) %>% drop_na()

# vigtigt at fjerne NA inden pca
# PCA ---------------------------------------------------------------------

# Create PCA object
data_pca <- data_aug %>%
  select(where(is.numeric)) %>% 
  scale() %>% # scale data
  prcomp() # do PCA


# plot principal components 
pca_plot1 <- data_pca %>%
  augment(data_aug) %>% # add original dataset back in
  ggplot(aes(.fittedPC1, .fittedPC2, color = Diagnosis_of_disease)) +
  geom_point(size = 1.5) +
  #scale_color_manual(
  #  values = c(O = "violet", 1 = "#D55E00", 2 ="#0072B", 3 ="red", 4 = "blue") 
  #) +
  theme_classic() + background_grid()

# #Tidy summarizes information about the components of a model.
# plot Scree plot of eigenvalues  
eigenvalues_plot <- data_pca %>%
  tidy(matrix = "eigenvalues") %>%
  top_n(10, percent) %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col() + 
  theme_minimal_hgrid(12)


ggsave("results/08_eigenvalues_plot.png", plot = eigenvalues_plot, device = "png")

# Augment to add original dataset back in
data_pca_aug <- data_pca %>% 
  augment(data_aug)

# Plot principal components  with diagnosis of disease as labels
plot_pca <- data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = Diagnosis_of_disease)) +
  geom_point(size = 1.5) 


# Save plot as png
ggsave("results/08_plot_pca.png", plot = plot_pca, device = "png")

# K-means -----------------------------------------------------------------

# Perform kmeans on principal components => reduce error 
data_kmeans <- data_pca_aug %>%
  select(contains("PC")) %>% 
  kmeans(centers = 2)

# Add cluster column to augmented pca table
data_kmeans_aug <- data_kmeans %>%
  augment(data_pca_aug) %>%
  rename(Cluster = .cluster)

# Plot kmeans on two first principal components
kmeans <- data_kmeans_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             shape = Diagnosis_of_disease)) +
  geom_point(aes(color = Cluster), alpha = 0.8) 

# Save plot as png
ggsave("results/08_kmeans.png", device = "png")