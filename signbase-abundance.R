library(tidyverse)

library(magrittr)
signbase_full <- read_csv("signBase_Version1.0.csv") %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave") %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum))
  


site_distances <- dist(cbind(signbase_full$longitude, signbase_full$latitude))

artifact_data <- signbase_full %>% 
  select(line:star)

signbase_full$site_name <- factor(signbase_full$site_name)
row.names(artifact_data) <- (unique(signbase_full$site_name))


signbase_abundance <- signbase_full %>%
  pivot_longer(cols= line:other,
               names_to = "sign_type") %>% 
  filter(value != 0)


signbase_abundance_plot_data <- signbase_abundance %>% 
  select(site_name,
        value) %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum))

library(ggpubr)
ggplot(signbase_abundance_plot_data) +
  aes(reorder(site_name, value),
      y= value) +
  geom_col() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)


##test for correlation
library(vegan)
library(ecodist)
library(ade4)
library(ape)
sample_size_euc <- vegdist(artifact_data, method = "euclidean")
euc_matrix <- as.matrix(sample_size_euc)
sample_size_bray <-  vegdist(artifact_data, method = "bray")
bray_matrix <- as.matrix(sample_size_bray)
site_distances_matrix <- as.matrix(site_distances)

#euclidean method

euc_mantel_rtest <- mantel.rtest(sample_size_euc, jac)
euc_mantel <- vegan::mantel(sample_size_euc, jac)
euc_mantel_test <- mantel.test(euc_matrix, dm)

##curtis-bray method

bray_mantel_rtest <- mantel.rtest(sample_size_bray, jac)
bray_mantel <- vegan::mantel(sample_size_bray, jac)
bray_mantel_test <- mantel.test(bray_matrix, dm)


## test both methods against site distance matrix
euc_dist_rtest <- mantel.rtest(site_distances, sample_size_euc)
eud_dist_mantel <- vegan::mantel(site_distances, sample_size_euc)
euc_dist_test <- mantel.test(site_distances_matrix, euc_matrix)


bray_dist_rtest <- mantel.rtest(site_distances, sample_size_bray)
bray_dist_mantel <- vegan::mantel(site_distances, sample_size_bray)
bray_dist_test <- mantel.test(site_distances_matrix, bray_matrix)


mantel_table <- data_frame("Euclidean_vs_jac" = c(euc_mantel_rtest$pvalue, euc_mantel$signif, euc_mantel_test$p),
                           "Bray_vs_Jac" = c(bray_mantel_rtest$pvalue, bray_mantel$signif, bray_mantel_test$p),
                           "Euclidean_vs_Distance" = c(euc_dist_rtest$pvalue, eud_dist_mantel$signif, euc_dist_test$p),
                           "Bray_vs_Distance" = c(bray_dist_rtest$pvalue, bray_dist_mantel$signif, bray_dist_test$p))


row.names(mantel_table) <- c("Mantel.rtest(),", "Mantel()", "Mantel.test()")

mantel_table



