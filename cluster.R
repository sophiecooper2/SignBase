library(tidyverse)
library(magrittr)
library(vegan)
library(cluster)

signbase_data <- read_csv("data/signBase_Version1.0.csv") %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre") %>% 
  select(-other, -rectangle)

artifact_data <- signbase_data %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  column_to_rownames("site_name") %>% 
  select(line:star)


jac_dist <- vegdist(artifact_data, method="jaccard")

hire_clust <- hclust(d = jac_dist, method = "ward.D2")

library(factoextra)
fviz_dend(hire_clust, cex = 0.5)

cut_groups <- cutree(hire_clust, k = 10)
head(cut_groups, n = 10)
group_table <- 

  
  
  
  