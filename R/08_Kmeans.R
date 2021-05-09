library(tidymodels)

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
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

kclust <- kmeans(km_data, centers = 2)
kclust

kclusts_final <- 
  tibble(k = 2) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

assignments_final <- 
  kclusts_final %>% 
  unnest(cols = c(augmented))

#each point colored according to the predicted cluster
p2 <- 
  ggplot(assignments_final, aes(x =  x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) 
p2

# evt overlay med om der er heart disease eller ej

ggsave(PCA_plot, filename="/cloud/project/results/PCA_plot.png", width = 16, height = 9, dpi = 72)





