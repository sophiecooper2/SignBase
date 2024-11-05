library(magrittr)
library(tidyverse)

my_data <- read_csv("signBase_Version1.0.csv")
my_data <- my_data %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave") %>% 
  select(-other)


ggplot(my_data) +
  aes(x = longitude,
      y = latitude) +
  geom_point() +
  theme_bw()

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

signbase_sf <- 
  st_as_sf(my_data, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326) %>% 
  filter(!country %in% c("Israel", "Iraq"))


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
                  size = 2) +
  coord_sf(xlim = c(-10,40), 
           ylim = c(35,55), 
           expand = FALSE) +
  theme_minimal()

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
                        

