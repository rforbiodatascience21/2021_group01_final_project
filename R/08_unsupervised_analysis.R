# Clear workspace ---------------------------------------------------------
rm(list = ls())

library(broom)
library(tidyverse)
library(viridis)

# Functions ---------------------------------------------------------------
source(file = "R/99_functions.R")

# Load augmented data
data_aug <- read_tsv(file = "data/03_data_aug.tsv.gz", 
                 col_types = cols(.default = col_double(),
                                  Age = col_double(),
                                  Resting_blood_pressure = col_double(),
                                  Serum_cholestoral = col_double(),
                                  Maximum_heart_rate_achieved = col_double(),
                                  ST_depression_induced_by_exercise 
                                  = col_double(),
                                  Number_of_major_vessels_colored_by_flourosopy 
                                  = col_double(),
                                  ST_depression_induced_by_exercise = 
                                    col_double(), 
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
                                  Age_class = col_character(), 
                                  diagnosis_of_heart_disease = col_character())) 

Data <- Data_selection(data = data_aug, 
                       var = c("Age",
                               "Resting_blood_pressure",
                               "Serum_cholestoral",
                               "Maximum_heart_rate_achieved",
                               "ST_depression_induced_by_exercise",
                               "Location",
                               "Diagnosis_of_disease",
                               "Sex_cat", 
                               "Chest_pain_type_cat",
                               "Fasting_blood_sugar_cat",
                               "Resting_electrocardiographic_cat",
                               "slope_of_ST_cat",
                               "Thal_cat",
                               "Age_class",
                               "diagnosis_of_heart_disease",
                               "Number_of_major_vessels_colored_by_flourosopy"), 
                       rm_na = TRUE) 

# Important to remove NA before pca
# PCA ---------------------------------------------------------------------


# Create PCA object
data_pca <- Data %>%
  select(where(is.numeric)) %>% 
  scale() %>% # scale data
  prcomp() # do PCA

# #Tidy summarizes information about the components of a model.
# plot Scree plot of eigenvalues  
eigenvalues_plot <- data_pca %>%
  tidy(matrix = "eigenvalues") %>%
  top_n(10, percent) %>%
  ggplot(aes(x = PC, y = percent,
             fill = PC,
             color = PC)) +
  geom_col(alpha = 0.5) + 
  labs(title = "Amount of total variance explained by PCs")+ 
  theme(legend.position = "none") +
  scale_fill_viridis() +
  scale_color_viridis()

ggsave("results/08_eigenvalues_plot.png", plot = eigenvalues_plot, 
       device = "png")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# plot rotation matrix
rotation_matrix <- data_pca %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = c("Age", "Blood Pressure", "Cholestoral", 
                  "Heart rate", "ST depression", "flouroscopy")),
    hjust = 1, nudge_x = -0.02, 
    color ="#904C2F"
  ) +
  xlim(-1.7, .5) + ylim(-.3, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  labs(title = "Rotation matrix") +
  theme_minimal_grid(12) 

ggsave("results/08_rotation_matrix.png", plot = eigenvalues_plot, 
       device = "png")

# Augment to add original dataset back in
data_pca_aug <- data_pca %>% 
  augment(Data)


# Plot principal components  with diagnosis of disease as labels
plot_pca <- data_pca_aug %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             colour = Diagnosis_of_disease)) +

  geom_point(size = 1.5) + 
  labs(title = "Principal components") 
  geom_point(size = 1.5) +
  scale_color_viridis(discrete = TRUE)

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
             y = .fittedPC2)) +
  geom_point(aes(color = Cluster), alpha = 0.8) +
  labs(title = "Kmeans clustering analysis for K = 2") +
  scale_color_viridis(discrete = TRUE)

# Save plot as png
ggsave("results/08_kmeans.png", device = "png")


# seperation of class 
dist_cluster <- data_kmeans_aug %>% 
  group_by(Cluster) %>% 
  count(Diagnosis_of_disease) %>% ggplot(aes(x = Cluster,
                                             y = n, 
                                             fill = Diagnosis_of_disease,
                                             color = Diagnosis_of_disease)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           width = 0.5,
           alpha = 0.5) + 
  labs(title = "Distribution of presence of disease w.r.t assigned cluster") +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE)

# Save plot as png
ggsave("results/08_kmeans_dist.png", device = "png")

Data <- data_kmeans_aug %>%
  mutate(Predict = case_when(Cluster == 1 ~ 0, Cluster == 2 ~1),
         Diagnosis_of_disease_No=case_when(diagnosis_of_heart_disease == 0 ~ 0, 
                                           diagnosis_of_heart_disease >= 1 ~ 1)) 

  Matrix_conf2 = Confusion_matrix(Data_test = Data)

save(x = Matrix_conf2,
       file = "results/08_Matrix_conf.RData")
