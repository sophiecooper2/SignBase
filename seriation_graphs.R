library(tidyverse)
library(magrittr)
signbase_full <- read_csv("signBase_Version1.0.csv")
signbase_full <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave")
groups <- list("1" = c("Abri Pataud", "El Rascaño", 
                       "Fumane", "La Viña",
                       "Lhotka", "Pod Hradem",
                       "Riparo Bombrini", "Sirgenstein Cave",
                       "Šandalja II"),
               "2" = c("Aurignac", "Gargas",
                       "Grotte de la Verpillière I", "Hohlenstein-Stadel",
                       "Istállóskő Cave", "Mladeč",
                       "d'Engihoul"), 
               "3" = c("Grottes de Fonds-de-Forêt", "Kvasice",
                       "Labeko Koba", "Les Rois",
                       "Breitenbach", "Peskő Cave",
                       "Vindija Cave", "Wildscheuer"),
               "4" = c("Le Trou Du Renard", "Trou Magrite", 
                       "Gatzarria", "Bockstein-Törle",
                       "Grotte De La Princesse Pauline", "Abri Lartet/Gorge d'Enfer",
                       "Tuto de Camalhot", "Slatinice"),
               "5" = c("Spy", "Solutré",
                       "Nová Dědina", "Grotte de Goyet",
                       "Brassempouy",  "El Castillo",
                       "Menton/Grottes du Grimaldi", "Geissenklösterle",
                       "El Salitre", "Göpfelsteinhöhle",
                       "Vogelherd"),
               "6" = c("Castanet", "Maisières-Canal",
                       "Grotte du Renne", "Hohle Fels",
                       "Žlutava", "Cellier",
                       "Blanchard", "La Ferrassie"),
               "7" = c("La Souquette", "Shelter Birów IV",
                       "Hornos de la Pena", "Trou al'Wesse",
                       "Les Cottés", "Le Terme Pialat",
                       "Abri de Laussel", "Abri du Poisson",
                       "Otaslavice"))

df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)


library(vegan)


graph_data <- artifact_matrix %>% 
  concentrate() %>% 
  data.frame() %>% 
  rownames_to_column("site_name") %>% 
  left_join(df)

sign_type_data <- graph_data %>% 
  pivot_longer(cols= notch:hashtag,
               names_to = "sign_type",
               values_to = "count") %>% 
  dplyr::filter(count != 0)

x_order <- graph_data %>% 
  select(-site_name,
         -group) %>% 
  colnames() 

library(ggpubr)
ggplot(sign_type_data) +
  aes(x = sign_type) +
  geom_bar() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL) +
  labs(x = x_order) +
  scale_x_discrete(limits = c(x_order)) +
  facet_wrap(~group, nrow = 11)



library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

signbase_sf <- 
  st_as_sf(signbase_full, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326) %>% 
  left_join(df, join_by(site_name))


world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1,
          aes(colour = gro:up)) +
  geom_text_repel(data = signbase_sf %>% 
                    distinct(site_name, .keep_all = TRUE),
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  size = 2) +
  coord_sf(xlim = c(-10,40), 
           ylim = c(35,55), 
           expand = FALSE) +
  theme_minimal() +
  facet_wrap(~group) #

  
