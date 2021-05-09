# Clear workspace ---------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(ggpubr)
library(broom)  # devtools::install_github("tidymodels/broom")
library(cowplot)


PCA_analysis<-function(data){
  
  pca_fit <- Data %>%  
    select(where(is.numeric)) %>% # retain only numeric columns
    scale() %>% # scale data
    prcomp() # do PCA
  
  # plot principal components 
  pca_plot1 <- pca_fit %>%
    augment(Data) %>% # add original dataset back in
    ggplot(aes(.fittedPC1, .fittedPC2, color = diagnosis_of_heart_disease)) +
    geom_point(size = 1.5) +
    #scale_color_manual(
    #  values = c(O = "violet", 1 = "#D55E00", 2 ="#0072B", 3 ="red", 4 = "blue") 
    #) +
    theme_classic() + background_grid()
  
  #------------------------------------------------------------------
  # extract rotation matrix
  pca_fit %>%
    tidy(matrix = "rotation")
  
  # define arrow style for plotting
  arrow_style <- arrow(
    angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
  )
  
  # plot rotation matrix
  rotation_matrix <- pca_fit %>%
    tidy(matrix = "rotation") %>%
    pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
    ggplot(aes(PC1, PC2)) +
    geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
    geom_text(
      aes(label = column),
      hjust = 1, nudge_x = -0.02, 
      color ="#904C2F"
    ) +
    xlim(-1.25, .5) + ylim(-.5, 1) +
    coord_fixed() + # fix aspect ratio to 1:1
    theme_minimal_grid(12)
  
  #------------------------------------------------------------------
  # plot eigen values 
  eigenvalues_plot <- pca_fit %>%
    tidy(matrix = "eigenvalues") %>%
    top_n(10, percent) %>%
    ggplot(aes(PC, percent)) +
    geom_col(fill = "#56B4E9", alpha = 0.8) +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(
      labels = scales::percent_format(),
      expand = expansion(mult = c(0, 0.01))
    ) +
    theme_minimal_hgrid(12)
  
  #------------------------------------------------------------------
  # visualize all together 
  ggarrange(pca_plot1,                                                 # First row with scatter plot
            ggarrange(rotation_matrix, eigenvalues_plot, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
            nrow = 2, 
            labels = "A"                                        # Labels of the scatter plot
  ) 
  
  
}

Data <- read_tsv(file = "data/02_data.tsv.gz") %>% drop_na()

PCA_plot <- PCA_analysis(Data)

ggsave(PCA_plot, filename="/cloud/project/results/PCA_plot.png", width = 16, height = 9, dpi = 72)


