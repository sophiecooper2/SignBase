## most abundant signs
library(tidyverse)
library(magrittr)
library(rcarbon)
library(readxl)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II",
         site_name!= "Le Terme Pialat",
         site_name != "Les Cottés",
         site_name != "Otaslavice",
         site_name != "Trou al'Wesse",
         site_name != "Hornos de la Pena",
         site_name != "Göpfelsteinhöhle") %>% 
  dplyr::select(-other, -rectangle)


sign_long <- signbase_full_clean %>% 
  pivot_longer(cols = line:star) %>% 
  dplyr::select(name, value) %>% 
  filter(value!= 0) %>% 
  group_by(name) %>% 
  summarise(total = sum(value)) %>% 
  dplyr::arrange(desc(total)) %>% 
  dplyr::slice(1:10)
  
abundant_sign_names <- c(sign_long$name)

signbase_full_clean <- signbase_full_clean %>% 
  dplyr::select(object_id, site_name, longitude, latitude,
                all_of(abundant_sign_names), date_bp_max_min, country)

lat_long_df <- signbase_full_clean %>% 
  dplyr::select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

signbase_unique_groups <- signbase_full_clean %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df)

artifact_data <- signbase_unique_groups %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(notch:obnotch)


groups <- list("1" = c("Abri Pataud", "El Rascaño", 
                       "Fumane", "La Viña",
                       "Lhotka", "Pod Hradem",
                       "Riparo Bombrini", "Sirgenstein Cave",
                       "Grottes de Fonds-de-Forêt", "Maisières-Canal"),
               "2" = c("Aurignac", "d'Engihoul",
                       "Gargas", "Gatzarria",
                       "Grotte de la Verpillière I", "Hohlenstein-Stadel",
                       "Istállóskő Cave",  "Mladeč", 
                       "Cellier", "Brassempouy"),
               "3" = c("Kvasice", "Labeko Koba", 
                       "Les Rois"),
               "4" = c("Le Trou Du Renard", "Solutré"),
               "5" = c("Wildscheuer", "Breitenbach",
                       "Peskő Cave", "Vindija Cave",
                       "El Castillo"),
               "6" = c("Slatinice", "Grotte du Renne",
                      "Nová Dědina", "Menton/Grottes du Grimaldi"),
               "7" = c("Tuto de Camalhot","Abri Lartet/Gorge d'Enfer",
                       "Blanchard", "La Ferrassie", "Castanet"),
               "8" = c("Trou Magrite", "Geissenklösterle"),
               "9" = c("Spy", "Vogelherd",
                       "Hohle Fels", "Žlutava"),
               "10" = c("Grotte de Goyet", "Bockstein-Törle"),
               "11" = c("La Souquette", "Shelter Birów IV",
                        "Abri de Laussel", "Abri du Poisson"))
group_df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)

signbase_unique_groups <- signbase_unique_groups %>% 
  left_join(group_df)


##map out sites
signbase_sf_group <- 
  st_as_sf(signbase_unique_groups, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf_group,
          aes(colour = as.factor(group))) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  geom_text_repel(data = signbase_sf_group,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 100,
                  size = 2) +
  theme_minimal() +
  facet_wrap(~group) +
  labs(color = "Group Number")




## Most distinct signs

library(tidyverse)
library(magrittr)
library(rcarbon)
library(readxl)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II",
         site_name != "Blanchard") %>% 
  dplyr::select(-other, -rectangle) %>% 
  dplyr::select(site_name, country, longitude, latitude, date_bp_max_min, cross, paw, 
                maccaroni, star, zigzag, grid, zoomorph, anthropomorph, zigzagrow)

artifact_total <- signbase_full_clean %>%
  dplyr::select(-site_name, -country, -longitude, -latitude, -date_bp_max_min) %>% 
  mutate(row_sums = rowSums(.)) 


signbase_full_clean$row_sum <- artifact_total$row_sums


signbase_full_clean <- signbase_full_clean %>% 
  filter(row_sum >= 1)

lat_long_df <- signbase_full_clean %>% 
  dplyr::select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

signbase_unique_groups <- signbase_full_clean %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df)

artifact_data <- signbase_unique_groups %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(cross:zigzagrow)



