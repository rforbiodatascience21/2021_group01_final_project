# Clear workspace ---------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(ggpubr)
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)
library(tidymodels)

Data <- read_tsv(file = "data/03_data_aug.tsv.gz")

'
This algorithm can only deal with numeric data as it calculates distances from centroids.
'
km_data <- Data %>% select(Age, Serum_cholestoral) %>% drop_na()

#kclust <- kmeans(km_data, centers = 2)
#kclust

#augment(kclust, km_data)
#tidy(kclust)
#glance(kclust)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(km_data, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, km_data)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))


p1 <- 
  ggplot(assignments, aes(x = Age, y = Serum_cholestoral)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

'
represents the variance within the clusters. It decreases as k increases
'
p2 <- ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()
p2
# k = 2
kclust <- kmeans(km_data, centers = 2)
kclust

kclusts_final <- 
  tibble(k = 2) %>%
  mutate(
    kclusts_final = map(k, ~kmeans(km_data, .x)),
    tidied = map(kclusts_final, tidy),
    glanced = map(kclusts_final, glance),
    augmented = map(kclusts_final, augment, km_data)
  )

assignments_final <- 
  kclusts_final %>% 
  unnest(cols = c(augmented))

#each point colored according to the predicted cluster
p3 <- 
  ggplot(assignments_final, aes(x = Age, y = Serum_cholestoral)) +
  geom_point(aes(color = .cluster), alpha = 0.8) 
p3

# evt overlay med om der er heart disease eller ej

diagnose <- Data %>% select(Age, Serum_cholestoral, Diagnosis_of_disease ) %>% drop_na()
assignments_final_labelled <- assignments_final %>%
  left_join(diagnose)

p4 <- 
  ggplot(assignments_final_labelled, aes(x = Age, y = Serum_cholestoral, shape = Diagnosis_of_disease )) +
  geom_point(aes(color = .cluster), alpha = 0.8) 
p4

##
ggarrange(p1,                                                 # First row with scatter plot
          ggarrange(p2, p3, ncol = 2, labels = c("B", "C")),
          p4,
          nrow = 3, 
          labels = c("A","D")                                        # Labels of the scatter plot
) 
Kmeans_plot <- ggarrange(p1,p2,p3,p4,
          nrow=2,ncol=2,
          labels =c("A", "B", "C","D"))   

ggsave(Kmeans_plot, filename="/cloud/project/results/Kmeans_plot.png", width = 16, height = 9, dpi = 72)
