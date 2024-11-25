library(tidyverse)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)
<<<<<<< HEAD

library(terra)
=======
>>>>>>> 10e315864f4ebc299ae7f8782b878ec646be4d3f

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
               "4" = c("Menton/Grottes du Grimaldi", "Riparo Bombrini",
                       "Fumane","Šandalja II"),
               "5" = c("Vogelherd", "Bockstein-Törle",
                       "Hohlenstein-Stadel", "Sirgenstein Cave", 
                       "Hohle Fels", "Göpfelsteinhöhle",
                       "Geissenklösterle"),
               "6" = c("Trou Magrite", "Le Trou Du Renard",
                       "Spy", "Grotte De La Princesse Pauline",
                       "Maisières-Canal",  "d'Engihoul",
                       "Grottes de Fonds-de-Forêt", "Trou al'Wesse",
                       "Grotte de Goyet"),
               "7" = c("Wildscheuer", "Breitenbach"),
               "8" = c("Vindija Cave","Pod Hradem", "Otaslavice",
                       "Mladeč", "Slatinice",
                       "Lhotka", "Kvasice",
                        "Žlutava", "Nová Dědina",
                        "Shelter Birów IV",
                       "Peskő Cave", "Istállóskő Cave"))
                     
                
df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

lat_long_df <- signbase_full_clean %>% 
  select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

abundance_data <- signbase_full_clean %>% 
  pivot_longer(cols= line:star,
               names_to = "sign_type") %>% 
  filter(value != 0) %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(df) %>% 
  left_join(lat_long_df) %>% 
  mutate(sign_total = value)

## calculate diversity
artifact_data <- signbase_full_clean %>%
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>%
  column_to_rownames(var = "site_name") %>% 
  select(line:star)

artifact_div <- vegan::diversity(artifact_data, index = "shannon")

abundance_data$diversity <- artifact_div
##plot diversity by geographical groups

ggplot(abundance_data) +
  aes(x = site_name, y = diversity) +
  geom_col() +
  facet_wrap(~group) +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

##map diversity
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)
signbase_sf <- 
  st_as_sf(abundance_data, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)


world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf,
          aes(colour = diversity,
              size = sign_total)) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  geom_text_repel(data = signbase_sf,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 100,
                  size = 2) +
  theme_minimal() +
  facet_wrap(~group) +
  labs(color = "Diversity",
       size = "Sign Count")



##Permanova test for data
jaccard_geo <- vegdist(artifact_data, method = "jaccard")

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


## Sign type makeup of groups
library(ggpubr)
artifact_matrix <- as.matrix(artifact_data %>% 
                               mutate(across(everything(), ~replace(., . > 1, 1))))
rownames(artifact_matrix) <- rownames(artifact_data)


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