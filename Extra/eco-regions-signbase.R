##Environmental Zones in Europe, 
##as categorized by the Environmental Stratification of Europe (EnS) dataset (European Environmental Agency)
##link to download : https://www.eea.europa.eu/en/datahub/datahubitem-view/c8c4144a-8c0e-4686-9422-80dbf86bc0cb?activeAccordion=1082728
library(raster)
library(terra)
eco_raster <- terra::rast("data/eea_r_3035_1_km_env-zones_p_2018_v01_r00.tif")

signbase_sf$environmental_zone <- extract(eco_raster, signbase_sf)

max_lat <- ceiling(max(signbase_sf$latitude))
min_lat <- floor(min(signbase_sf$latitude))
max_lon <- ceiling(max(signbase_sf$longitude))
min_lon <- floor(min(signbase_sf$longitude))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

eco_raster_crop = crop(x = eco_raster, y = geographic_extent)


signbase_sf <- signbase_sf %>% 
  mutate(environmental_zone = environmental_zone$eea_r_3035_1_km_env)

plot(eco_raster)
points(x = signbase_sf$longitude, 
       y = signbase_sf$latitude, add = TRUE)

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

# BM seems too detailed
basin_shapefiles_simple <- st_simplify(st_make_valid(basin_shapefiles))

signbase_sf$basins <- st_intersection(basin_shapefiles_simple, signbase_sf)

ggplot() +
  geom_sf(data = basin_shapefiles_simple)
