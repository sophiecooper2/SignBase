library(tidyverse)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)
library(terra)
signbase_full <- read_csv("data/signBase_Version1.0.csv")
signbase_full_clean <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre") %>% 
  select(-other, -rectangle)

lat_long_df <- signbase_full_clean %>% 
  select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

signbase_unique_groups <- signbase_full_clean %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  column_to_rownames("site_name") 

artifact_div <- signbase_unique_groups %>% 
  select(line:star) %>% 
  vegan::diversity(index = "shannon") %>% 
  tibble() 

colnames(artifact_div) <- "diversity"

abundance_data <- signbase_full_clean %>% 
  pivot_longer(cols= line:star,
               values_to = "sign_total",
              names_to = "sign_type") %>% 
  filter(sign_total != 0) %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  select(site_name, sign_total,  longitude, latitude)

signbase_sf <- 
  st_as_sf(abundance_data, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)

signbase_sf <- signbase_sf %>% 
  add_column(artifact_div)

##Environmental Zones in Europe, 
##as categorized by the Environmental Stratification of Europe (EnS) dataset (European Environmental Agency)
##link to download : https://www.eea.europa.eu/en/datahub/datahubitem-view/c8c4144a-8c0e-4686-9422-80dbf86bc0cb?activeAccordion=1082728

eco_raster <- rast("data/eea_r_3035_1_km_env-zones_p_2018_v01_r00.tif")

signbase_sf$environmental_zone <- extract(eco_raster, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(environmental_zone = environmental_zone$eea_r_3035_1_km_env)
  
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf,
          aes(color = as.factor(environmental_zone),
              size = diversity)) +
  scale_size_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
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
  facet_wrap(~environmental_zone)

##looking at distribution of signs within environmental zones
signbase_unique_groups$environmental_zone<- signbase_sf$environmental_zone

abundance_data_environment <- signbase_unique_groups %>% 
  pivot_longer(cols= line:star,
               values_to = "sign_total",
               names_to = "sign_type") %>% 
  filter(sign_total != 0) 

library(ggpubr)
ggplot(abundance_data_environment) +
  aes(x = sign_type) +
  geom_bar() +
  facet_wrap(~environmental_zone) +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

## Biogeographical regions of Europe - (https://pmc.ncbi.nlm.nih.gov/articles/PMC7340631/#sec24)
#3 as determined by the European Environment Agency

bio_geo_raster <- rast("data/bdj-08-e53720-s002.tif")

signbase_sf$biogeographical_region <- extract(bio_geo_raster, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(biogeographical_region = biogeographical_region$bdj)

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf,
          aes(color = as.factor(biogeographical_region),
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
  facet_wrap(~biogeographical_region)

## Hydrobasin data
library(fs)
shapefiles <- "large-files/hybas_eu_lev01-12_v1c/" %>% 
  dir_ls(recurse = TRUE, regexp = 'shp$') 

basin_shapefiles <- shapefiles %>% 
  map(st_read) %>% 
  bind_rows()

basin_shapefiles <- st_as_sf(basin_shapefiles)

# BM seems too detailed, maybe ignore this
basin_shapefiles_simple <- st_simplify(st_make_valid(basin_shapefiles))

<<<<<<< HEAD
basin_shapefiles_simple_cropped <- 
  st_crop( basin_shapefiles_simple$geometry, 
           signbase_sf$geometry)

ggplot() +
  geom_sf(data = basin_shapefiles_simple_cropped)

v <- vect(basin_shapefiles)
r <- rast(v, resolution = 0.001)

basin_raster <- rasterize(v, r)

signbase_sf$basin <- extract(basin_raster, signbase_sf)
=======
signbase_sf$basins <- st_intersection(basin_shapefiles_simple, signbase_sf)

ggplot() +
  geom_sf(data = basin_shapefiles_simple)
>>>>>>> 0ffd10f34922182b52f6076f3648d2bebafe99e3
