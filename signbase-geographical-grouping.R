library(tidyverse)
library(magrittr)
signbase_full <- read_csv("signBase_Version1.0.csv")

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

signbase_sf <- 
  st_as_sf(signbase_full, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1,
          aes(colour = country)) +
  geom_text_repel(data = signbase_sf %>% 
                    distinct(site_name, .keep_all = TRUE),
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 100,
                  size = 2) +
  coord_sf(xlim = c(-7,70), 
           ylim = c(35,52), 
           expand = FALSE) +
  theme_minimal()


groups <- list("1" = c("La Viña", "El Salitre",
                       "El Castillo", "Hornos de la Pena",
                       "El Rascaño", "Labeko Koba",
                       "Gargas", "Gatzarria",
                       "Tuto de Camalhot", "Aurignac",
                       "Brassempouy", "Les Cottés",
                       "Le Terme Pialat", "Abri Pataud",
                       "Abri du Poisson", "La Ferrassie",
                       "Abri Lartet/Gorge d'Enfer", "Abri de Laussel",
                       "Cellier", "Castanet",
                       "La Souquette", "Les Rois",
                       "Blanchard"),
              "2" = c("Grotte du Renne", "Grotte de la Verpillière I",
                      "Solutré", "Menton/Grottes du Grimaldi", 
                      "Riparo Bombrini", "Fumane", 
                      "Šandalja II"),
              "3" = c("Trou Magrite", "Le Trou Du Renard",
                      "Spy", "Grotte De La Princesse Pauline",
                      "Maisières-Canal",  "d'Engihoul",
                      "Grottes de Fonds-de-Forêt", "Trou al'Wesse",
                      "Grotte de Goyet", "Wildscheuer",
                      "Vogelherd", "Bockstein-Törle",
                      "Hohlenstein-Stadel", "Sirgenstein Cave",
                      "Hohle Fels", "Göpfelsteinhöhle",
                      "Geissenklösterle"),
              "4" = c("Breitenbach", "Willendorf",
                      "Pod Hradem", "Vindija Cave",
                      "Kvasice", "Nová Dědina",
                      "Peskő Cave", "Istállóskő Cave",
                      "Žlutava", "Lhotka",
                      "Slatinice", "Otaslavice",
                      "Mladeč", "Shelter Birów IV"),
              "Outliers" = c("Riparo di Fontana Nuova","Muralovka", 
                             "Shanidar Cave","Hayonim Cave"))
                
df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

# assign groups to signbase data
signbase_unique_groups <- signbase_full %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum)) %>%
  left_join(df) %>% 
  filter(group != "Outliers")

jaccard_geo_group_data <- signbase_unique_groups %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum)) %>% 
  select(line:star)
  
jaccard_geo <- vegdist(jaccard_geo_group_data, method = "jaccard")


perman <- adonis2(jaccard_geo~as.factor(signbase_unique_groups$group), 
                  method = "jaccard",
                  sqrt.dist = TRUE)
print(perman)

## Analysis of multivariate homogeneity of group dispersions (variances)
dispersion <- betadisper(jaccard_geo, group = signbase_unique_groups$group)
permutest(dispersion)
plot(dispersion, hull = FALSE, ellipse=TRUE)

##Modularity
jaccard_geo_matrix <- as.matrix(jaccard_geo)
artifact_sim <- graph_from_adjacency_matrix(1- jaccard_geo_matrix)
mem <- as_membership(as.numeric(signbase_unique_groups$group))
as_group <- modularity(artifact_sim, membership = mem)

as_group
```