library(magrittr)
library(tidyverse)

abundance_data <- signbase_full_clean %>% 
  pivot_longer(cols= line:star,
               values_to = "sign_total") %>% 
  filter(sign_total != 0) %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum))

country_data_df <- signbase_full_clean %>% 
  select(site_name, country) %>% 
  distinct(site_name, .keep_all = TRUE)


abundance_data <- abundance_data %>% 
  left_join(lat_long_df) %>% 
  left_join(group_df) %>% 
  left_join(country_data_df) %>% 
  select(site_name, sign_total, longitude, latitude, group, country)


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
          aes(colour = group,
              size = sign_total)) +
  geom_text_repel(data = signbase_sf,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 100,
                  size = 2) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  theme_minimal() +
  facet_wrap(~group)


## GIS with raster data
library(terra)
library(tidyterra)
list_of_rasters <- c("eudem/N2000000E4000000.tif",
                     "eudem/N2000000E5000000.tif",
                     "eudem/N2000000E3000000.tif",
                     "eudem/N3000000E4000000.tif")

list_of_rasters <- 
  terra::merge(sprc(list_of_rasters),
               gdal = c("BIGGTIFF = YES",
                        "NUM_THREADS = ALL_CPUS"))

ggplot() +
  geom_spatraster(data = list_of_rasters) +
  geom_sf(data = signbase_sf %>% 
          distinct(site_name, .keep_all = TRUE), 
          size = 1) 


  extract_res <- 
  extract(list_of_rasters, signbase_sf[,2]) 
  
  signbase_sf$elevation_from_raster <- extract_res$N2000000E4000000

aspect <- terrain(list_of_rasters, v = "aspect", unit = "degrees", neighbors=8)
slope <- terrain(list_of_rasters, v = "slope", unit = "degrees", neighbors=8)

signbase_sf$aspect<-
  extract(aspect, signbase_sf[,2])$aspect

signbase_sf$slope <-
  extract(slope, signbase_sf[,2])$slope 

ggplot() +
  geom_spatraster(data = aspect) +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1) 
ggplot() +
  geom_spatraster(data = slope) +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1) 


## Running tests

## Elevation vs abundance, point graph
ggplot(signbase_sf) +
  aes(x = sign_total,
      y= elevation_from_raster) + 
  geom_point()

## Elevation by group, boxplot
ggplot(signbase_sf) +
  aes(x = group,
      y = elevation_from_raster) +
  geom_boxplot()



                        

