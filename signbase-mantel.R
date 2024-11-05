library(ape)
library(tidyverse)
library(vegan)
library(ade4)
library(phangorn)
library(Lock5Data)
library(magrittr)

my_data <- read_csv("signBase_Version1.0.csv") %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum))
  
site_distances <- dist(cbind(my_data$longitude, my_data$latitude))

sign_similarity_data <- my_data %>% 
  select(line:star)

sign_similarity_jac <- vegdist(sign_similarity_data, method = "jaccard")


rtest <- mantel.rtest(site_distances, sign_similarity_jac, nrepet = 1000)

mantel <- mantel(site_distances,sign_similarity_jac), permutations = 1000)
print(rtest)
print(mantel)

plot(rtest)

bur_man_cor <- mantel.correlog(sign_similarity_jac, site_distances, n.class=50, break.pts=NULL, cutoff=FALSE, r.type="pearson", nperm=999, mult="holm", progressive=TRUE)

plot(bur_man_cor)



##Mantel_test

site_distances_mat <- as.matrix(site_distances)
sign_similarity_jac_mat <- as.matrix(sign_similarity_jac)

mantel_sim <- mantel.test(site_distances_mat,sign_similarity_jac_mat, nperm = 1000)
print(mantel_sim)

