library(tidyverse)
library(magrittr)
library(rcarbon)
library(readxl)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>%  # 511 rows
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II") %>% 
  dplyr::select(-other, -rectangle) %>% 
  mutate(longitude = ifelse(site_name == "Riparo Bombrini", 7.437500, longitude)) %>% 
  mutate(latitude = ifelse(site_name == "Riparo Bombrini", 43.77083, latitude))

signbase_years <- signbase_full_clean %>%  # 446
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
  inner_join(signbase_years %>% 
               dplyr::select(object_id, MedianBP))

signbase_full_clean <- signbase_full_clean %>%  # 446
  mutate(time_period = case_when((MedianBP > 42999) ~ "transitional",
                                 (MedianBP > 39899) ~ "proto_aurignacian",
                                 (MedianBP > 36999) ~ "early_aurignacian",
                                 (MedianBP > 31999) ~ "evolved_aurignacian"))



ggplot(signbase_full_clean) +
  aes(x = MedianBP, fill = time_period) +
  geom_bar(width = 150)

lat_long_df <- signbase_full_clean %>% 
  dplyr::select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

trans_unique_data <- signbase_full_clean %>% 
  filter(time_period == "transitional") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "transitional")

proto_unique_data <- signbase_full_clean %>% 
  filter(time_period == "proto_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "proto_aurignacian")

early_unique_data <- signbase_full_clean %>% 
  filter(time_period == "early_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "early_aurignacian")

evolved_unique_data <- signbase_full_clean %>% 
  filter(time_period == "evolved_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "evolved_aurignacian")


trans_artifact_data <- trans_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)


proto_artifact_data <- proto_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

early_artifact_data <- early_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

evolved_artifact_data <- evolved_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)


##collective plots

library(cowplot)

seriation_trans <- produce_clusters(trans_artifact_data, trans_unique_data, method = "seriation")
seriation_proto <- produce_clusters(proto_artifact_data, proto_unique_data, method = "seriation")
seriation_early <- produce_clusters(early_artifact_data, early_unique_data, method = "seriation")
seriation_evolved <- produce_clusters(evolved_artifact_data, evolved_unique_data, method = "seriation")

plot_grid(seriation_trans,seriation_proto, seriation_early , seriation_evolved, 
          ncol = 2, align = "hv", 
          labels = c("Transitional", "Proto", "Early", "Evolved"), label_size = 10)

network_trans <- trans_network(trans_artifact_data)
network_proto <- produce_clusters(proto_artifact_data, proto_unique_data, method = "network")
network_early <- produce_clusters(early_artifact_data, early_unique_data, method = "network")
network_evolved <- produce_clusters(evolved_artifact_data, evolved_unique_data, method = "network")

plot_grid(network_trans, network_proto, network_early, network_evolved, 
          ncol = 2, align = "hv", labels = c("Transitional", "Proto", "Early", "Evolved"), 
          label_size = 7, label_colour = "black", label_y = 1.02, label_x = -0.03)

pcoa_trans <- produce_clusters(trans_artifact_data, trans_unique_data, method = "pcoa")
pcoa_proto <- produce_clusters(proto_artifact_data, proto_unique_data, method = "pcoa")
pcoa_early <- produce_clusters(early_artifact_data, early_unique_data, method = "pcoa")
pcoa_evolved <- produce_clusters(evolved_artifact_data, evolved_unique_data, method = "pcoa")

plot_grid(pcoa_trans, pcoa_proto, pcoa_early, pcoa_evolved, 
          align = "hv", labels = c("Transitional", "Proto", "Early", "Evolved"),
          label_size = 7, label_colour = "red", label_y = 1.02)


##groups within time periods 
trans_unique_data <- trans_unique_data %>% 
  mutate(group = case_when((site_name == "Pod Hradem") ~ "1",
                           (site_name == "El Castillo" | site_name == "Hohle Fels") ~ "2"))

proto_unique_data <- proto_unique_data %>% 
  mutate(group = case_when((site_name == "Labeko Koba" | 
                              site_name == "Gatzarria" | 
                              site_name == "Hohlenstein-Stadel" | 
                              site_name == "Mladeč" | 
                              site_name == "Abri Pataud" | 
                              site_name == "Fumane") ~ "1",
                           (site_name == "Geissenklösterle" |
                              site_name == "Grotte du Renne" |
                              site_name == "Vogelherd" |
                              site_name == "Spy" |
                              site_name == "La Ferrassie" |
                              site_name == "Les Cottés") ~ "2"))

early_unique_data <- early_unique_data %>% 
  mutate(group = case_when((site_name == "Grottes de Fonds-de-Forêt" |
                              site_name == "Riparo Bombrini"|
                              site_name == "Grotte de la Verpillière I"|
                              site_name == "Vindija Cave" |
                              site_name == "Trou al'Wesse") ~ "1",
                           (site_name == "Hohle Fels" | 
                              site_name == "Solutré" |
                              site_name == "Castanet"|
                              site_name == "Cellier" | 
                              site_name == "Blanchard") ~ "2"))


evolved_unique_data <- evolved_unique_data %>% 
  mutate(group = case_when((site_name == "Les Rois" |
                              site_name == "Gargas" |
                              site_name == "La Viña"|
                              site_name ==  "Sirgenstein Cave") ~ "1",
                           (site_name == "Bockstein-Törle"|
                              site_name == "Vogelherd" |
                              site_name == "Hohle Fels"|
                              site_name == "Trou Magrite") ~ "2"))


time_unique_data_full <-rbind(trans_unique_data, proto_unique_data, 
                              early_unique_data, evolved_unique_data)

time_unique_data_full$time_period <- factor(time_unique_data_full$time_period, 
                                            levels = c("transitional", "proto_aurignacian", 
                                                       "early_aurignacian", "evolved_aurignacian"))

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

signbase_sf <- time_unique_data_full  %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf, aes(color = group)) +
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
  facet_wrap(~time_period)



##testing overall dataset
time_unique_data_test <- time_unique_data_full %>% 
  mutate(group = time_period)

time_unique_data_artifact_test <- time_unique_data_full %>% 
  dplyr::select(line:star)

strength_function(time_unique_data_artifact_test, time_unique_data_test, type = "dispersion")

## test groupings in individual time period

trans_strength <- strength_function(trans_artifact_data, trans_unique_data, type = "table")
proto_strength <- strength_function(proto_artifact_data, proto_unique_data, type = "table")
early_strength <- strength_function(early_artifact_data, early_unique_data, type = "table")
evolved_strength <- strength_function(evolved_artifact_data, evolved_unique_data, type = "table")


total_table <- rbind(trans_strength, proto_strength, early_strength, evolved_strength) %>% 
  mutate(time_period = c("transitional", "proto", "early", "evolved"), .before = 1)



trans_strength_plot <- strength_function(trans_artifact_data, trans_unique_data, type = "dispersion")
proto_strength_plot <- strength_function(proto_artifact_data, proto_unique_data, type = "dispersion")
early_strength_plot <- strength_function(early_artifact_data, early_unique_data, type = "dispersion")
evolved_strength_plot <- strength_function(evolved_artifact_data, evolved_unique_data, type = "dispersion")

##look at individual sign distribution across periods

full_data_long <- time_unique_data_full %>% 
  pivot_longer(cols = line:star) %>% 
  filter(value != 0)

ggplot(full_data_long) +
  aes(x = name, fill = group) +
  geom_bar() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL) + 
  facet_wrap(~time_period, nrow = 4)




  
