glibrary(magrittr)
library(tidyverse)


abundance_data <- signbase_full_clean %>% 
  pivot_longer(cols= line:star,
               values_to = "sign_total") %>% 
  filter(sign_total != 0) %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum))


abundance_data <- abundance_data %>% 
  left_join(lat_long_df) %>% 
  left_join(group_df) %>% 
  select(site_name, sign_total, longitude, latitude, group)


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


## GIS with raster data - data from https://ec.europa.eu/eurostat/web/gisco/geodata/digital-elevation-model/eu-dem (DD dataset)
library(terra)
library(tidyterra)

raster_data <- rast("large-files/EU_DEM_mosaic_5deg/eudem_dem_4258_europe.tif")

signbase_sf$elevation <- extract(raster_data, signbase_sf)

raster_data <- crop(raster_data, signbase_sf)

aspect_data <- terrain(raster_data, v = "aspect", unit = "degrees", neighbors=8)
slope_data <- terrain(raster_data, v = "slope", unit = "degrees", neighbors=8)

signbase_sf$aspect <- extract(aspect_data, signbase_sf)
signbase_sf$slope <- extract(slope_data, signbase_sf)

ggplot() +
  geom_spatraster(data = aspect_data) +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1) 
ggplot() +
  geom_spatraster(data = slope_data) +
  geom_sf(data = signbase_sf %>% 
            distinct(site_name, .keep_all = TRUE), 
          size = 1) 





## Running tests 

## Elevation vs abundance, point graph
ggplot(signbase_sf) +
  aes(x = sign_total,
      y= elevation$eudem_dem_4258_europe) + 
  geom_point()

## Elevation by group, boxplot
elevation_boxplot <- ggplot(signbase_sf) +
  aes(x = group,
      y = elevation$eudem_dem_4258_europe) +
  geom_boxplot() +
  labs(y = "elevation")



## group by abundance boxplot
abundance_boxplot <- ggplot(signbase_sf) +
  aes(x = group,
      y = sign_total) +
  geom_boxplot() +
  labs(y = "sign count")

library(cowplot)
plot_grid(elevation_boxplot, abundance_boxplot)


##scatterplot of abundace vs elevation
signbase_sf <- signbase_sf %>% 
  filter(site_name != "Vogelherd") %>% 
  mutate("elevation" = elevation$eudem_dem_4258_europe) %>% 
  mutate(aspect = aspect$aspect) %>% 
  mutate(slope = slope$slope)

ggplot(signbase_sf) +
  aes(x = sign_total,
      y = elevation,
      color = group) +
  geom_point()

