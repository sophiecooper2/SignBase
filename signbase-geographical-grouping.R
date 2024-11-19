library(tidyverse)
library(magrittr)


library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)




groups <- list("1" = c("La Viña", "El Castillo", 
                       "Hornos de la Pena", "El Rascaño", 
                       "Labeko Koba"),
               "2" = c("Gargas", "Gatzarria",
                       "Tuto de Camalhot", "Aurignac",
                       "Brassempouy", "Le Terme Pialat",
                       "Abri Pataud", "Abri de Laussel",
                       "Abri Lartet/Gorge d'Enfer",
                       "Abri du Poisson", "Castanet",
                       "La Souquette", "Blanchard",
                       "Cellier", "Les Rois",
                       "La Ferrassie"),
               "3" = c("Les Cottés","Grotte du Renne", 
                       "Grotte de la Verpillière I", "Solutré"),
               "4" = c("Menton/Grottes du Grimaldi", "Riparo Bombrini"),
               "5" = c("Fumane","Šandalja II"),
               "6" = c("Vogelherd", "Bockstein-Törle",
                       "Hohlenstein-Stadel", "Sirgenstein Cave", 
                       "Hohle Fels", "Göpfelsteinhöhle",
                       "Geissenklösterle"),
               "7" = c("Trou Magrite", "Le Trou Du Renard",
                       "Spy", "Grotte De La Princesse Pauline",
                       "Maisières-Canal",  "d'Engihoul",
                       "Grottes de Fonds-de-Forêt", "Trou al'Wesse",
                       "Grotte de Goyet"),
               "8" = c("Wildscheuer", "Breitenbach"),
               "9" = c("Vindija Cave"),
               "10" = c("Pod Hradem", "Otaslavice",
                        "Mladeč", "Slatinice",
                        "Lhotka", "Kvasice",
                        "Žlutava", "Nová Dědina",
                        "Shelter Birów IV"),
               "11" = c("Peskő Cave", "Istállóskő Cave"))
                     
                
df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

lat_long_df <- signbase_full_clean %>% 
  select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

signbase_geo_groups <- signbase_full_clean %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(df)

signbase_geo_groups <- signbase_geo_groups %>% 
  left_join(lat_long_df)

jaccard_geo_group_data <- signbase_geo_groups %>%
  column_to_rownames(var = "site_name") %>% 
  select(line:star)
  
jaccard_geo <- vegdist(jaccard_geo_group_data, method = "jaccard")


perman <- adonis2(jaccard_geo~as.factor(signbase_geo_groups$group), 
                  method = "jaccard",
                  sqrt.dist = TRUE)
print(perman)

## Analysis of multivariate homogeneity of group dispersions (variances)
dispersion <- betadisper(jaccard_geo, group = signbase_geo_groups$group)
permutest(dispersion)
plot(dispersion, hull = FALSE, ellipse=TRUE)

##Modularity
jaccard_geo_matrix <- as.matrix(jaccard_geo)
artifact_sim <- graph_from_adjacency_matrix(1- jaccard_geo_matrix)
mem <- as_membership(as.numeric(signbase_unique_groups$group))
as_group <- modularity(artifact_sim, membership = mem)

as_group


## Seriation


library(ggpubr)
artifact_matrix <- as.matrix(jaccard_geo_group_data %>% 
                               mutate(across(everything(), ~replace(., . > 1, 1))))
rownames(artifact_matrix) <- rownames(jaccard_geo_group_data)


geo_graph_data <- artifact_matrix %>% 
  data.frame() %>% 
  rownames_to_column("site_name") %>% 
  left_join(df)

sign_type_data <- geo_graph_data %>% 
  pivot_longer(cols= line:star,
               names_to = "sign_type",
               values_to = "count") %>% 
  dplyr::filter(count != 0)

library(ggpubr)
ggplot(sign_type_data) +
  aes(x = sign_type) +
  geom_bar() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL) +
  facet_wrap(~group, nrow = 7)



```