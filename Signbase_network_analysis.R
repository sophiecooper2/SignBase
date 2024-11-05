library(magrittr)
library(tidyverse)
library(vegan)
library(phangorn)

my_data <- read_csv("signBase_Version1.0.csv")

my_data_matrix <- my_data %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum)) %>% 
  select(line:star) 

dm <- as.matrix(vegdist(my_data_matrix, "jaccard"))

nnet <- neighborNet(dm)
par("mar" = rep(1, 4))

neighbor_graph <- plot(nnet, show.tip.label = FALSE)
tiplabels(unique(my_data$site_name),
          bg = "white",
          frame = "none", 
          cex = 0.5)

