## Most distinct signs

library(tidyverse)
library(magrittr)
library(rcarbon)

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
         site_name != "Žlutava",
         site_name != "Trou Magrite",
         site_name != "Le Terme Pialat",
         site_name != "Le Trou Du Renard",
         site_name !=  "Otaslavice",
         site_name != "Brassempouy") %>% 
  dplyr::select(site_name, object_id, country, longitude, latitude, date_bp_max_min, 
                cross, v, star, zigzag, zigzagrow, grid, hashtag, hatching, 
                anthropomorph,zoomorph, vulva, rhombus, rectangle)


signbase_years <- signbase_full_clean %>% 
  drop_na(date_bp_max_min) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+\\/\\-", "±")) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+", "±")) %>% 
  separate(date_bp_max_min,
           sep = " - ",
           c("date_bp_max", "date_bp_min"),
           remove = FALSE) %>% 
  mutate(date_bp_max = ifelse(str_detect(date_bp_max, "\\/"),
                              str_extract(date_bp_max, ".*?(?=\\/)"),
                              date_bp_max)) %>% 
  mutate(date_bp_min = ifelse(str_detect(date_bp_min, "\\/"),
                              str_extract(date_bp_min, ".*?(?=\\/)"),
                              date_bp_min)) %>% 
  separate(date_bp_max,
           sep = "±",
           c("date_bp_max_age",
             "date_bp_max_error")) %>% 
  separate(date_bp_min,
           sep = "±",
           c("date_bp_min_age",
             "date_bp_min_error")) %>% 
  drop_na(date_bp_max_age,
          date_bp_max_error) %>% 
  mutate(date_bp_max_age = parse_number(date_bp_max_age),
         date_bp_max_error = parse_number(date_bp_max_error))
signbase_years_cal <- 
  rcarbon::calibrate(signbase_years$date_bp_max_age,
                     signbase_years$date_bp_max_error,
                     verbose = FALSE) %>% 
  summary() %>% 
  data_frame()


signbase_years$MedianBP <- signbase_years_cal$MedianBP




signbase_full_clean <- signbase_full_clean %>% 
  mutate(row_sum = rowSums(x = (signbase_full_clean %>% 
                                  dplyr::select(cross:rectangle)))) %>% 
  filter(row_sum >= 1) %>% 
  select(-row_sum) %>% 
  left_join(signbase_years %>% 
              dplyr::select(object_id, MedianBP))

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
  dplyr::select(cross:rectangle)


long_site_distinct_location <- signbase_unique_groups %>% 
  pivot_longer(cols = cross:rectangle) %>% 
  filter(value != 0) %>% 
  rename(sign_type = name)


signbase_sf <- 
  st_as_sf(long_site_distinct_location, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]


#| 
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

library(ggpubr)

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  geom_text_repel(data = signbase_sf,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 30,
                  size = 2) +
  theme_minimal() +
  facet_wrap(~sign_type)



##plot out signs by date:

long_distinct_dates <- signbase_full_clean %>% 
  pivot_longer(cols = cross:rectangle) %>% 
  select(site_name, name, value, MedianBP) %>% 
  filter(value != 0) %>% 
  drop_na()


ggplot(long_distinct_dates) +
  aes(x = MedianBP, fill = )+
  geom_bar(width = 150) +
  facet_wrap(~name)


##divide signs into groups


sim_df <- jac %>% 
  as.matrix() %>% 
  as.data.frame()

sim_df <- sim_df %>% 
  mutate(siteB = colnames(sim_df)) %>% 
  pivot_longer(cols = `Abri de Laussel`:Vogelherd)

poisson_sim <- sim_df %>% 
  filter(name == "Abri du Poisson") %>% 
  filter(value < 0.7)

groups <- list("1" = c("Geissenklösterle", "Solutré"),
               "2" = c("Vogelherd", "Spy","Hohle Fels", "Menton/Grottes du Grimaldi", "Castanet"),
               "3" = c("Göpfelsteinhöhle", "Slatinice"),
               "4" = c("Grotte de Goyet", "Grotte du Renne", "Nová Dědina"),
               "5" = c("Bockstein-Törle", "Maisières-Canal", "Hornos de la Pena"),
               "6" = c("La Ferrassie", "Blanchard",  "Cellier"),
               "7" = c("Abri du Poisson", "Abri de Laussel"))
group_df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)
     

signbase_unique_groups <- signbase_unique_groups %>% 
  left_join(group_df)


##map out groups

signbase_group_sf <- 
  st_as_sf(signbase_unique_groups, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_group_sf, mapping = aes(color = group)) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  geom_text_repel(data = signbase_group_sf,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 30,
                  size = 2) +
  theme_minimal()

