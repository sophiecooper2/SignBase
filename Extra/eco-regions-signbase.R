##Environmental Zones in Europe, 
##as categorized by the Environmental Stratification of Europe (EnS) dataset (European Environmental Agency)
##link to download : https://www.eea.europa.eu/en/datahub/datahubitem-view/c8c4144a-8c0e-4686-9422-80dbf86bc0cb?activeAccordion=1082728

library(terra)
library(tidyterra)
env_raster <- rast("large-files//eea_r_3035_1_km_env-zones_p_2018_v01_r00.tif")

env_raster_reproj <- terra::project(env_raster, "epsg:4326")

signbase_sf$environmental_zone <- terra::extract(env_raster_reproj, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(environmental_zone = environmental_zone$eea_r_3035_1_km_env)

##Plotting out distribution of signs over environmental zone raster
ggplot(Europe) + 
  geom_spatraster(data = env_raster_reproj) +
  scale_fill_grass_c(palette = "viridis") +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

## plot out env zone by group 
ggplot(signbase_sf) +
  aes(x = group, fill= as.factor(environmental_zone)) +
  geom_bar()

## Biogeographical regions of Europe - (https://pmc.ncbi.nlm.nih.gov/articles/PMC7340631/#sec24)
#3 as determined by the European Environment Agency

bio_geo_raster <- rast("data/bdj-08-e53720-s002.tif")

signbase_sf$biogeographical_region <- terra::extract(bio_geo_raster, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(biogeographical_region = biogeographical_region$bdj)

ggplot(Europe) + 
  geom_spatraster(data = bio_geo_raster) +
  scale_fill_grass_c(palette = "viridis") +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

## elevation data
library(geodata)
elev_rast <- elevation_global(res = 0.5, path = "/Users/sophiecooper/Independent Research Study/SignBase/large-files")


signbase_sf$elevation <- extract(elev_rast, signbase_sf)


aspect_data <- terrain(elev_rast, v = "aspect", unit = "degrees", neighbors=8)
slope_data <- terrain(elev_rast, v = "slope", unit = "degrees", neighbors=8)

signbase_sf$aspect <- extract(aspect_data, signbase_sf)
signbase_sf$slope <- extract(slope_data, signbase_sf)




##climate data

clim_files <- c("large-files/cclgmbi_2-5m/cclgmbi1.tif", "large-files/cclgmbi_2-5m/cclgmbi10.tif",
                "large-files/cclgmbi_2-5m/cclgmbi11.tif", "large-files/cclgmbi_2-5m/cclgmbi12.tif",
                "large-files/cclgmbi_2-5m/cclgmbi13.tif", "large-files/cclgmbi_2-5m/cclgmbi14.tif",
                "large-files/cclgmbi_2-5m/cclgmbi15.tif", "large-files/cclgmbi_2-5m/cclgmbi16.tif",
                "large-files/cclgmbi_2-5m/cclgmbi17.tif", "large-files/cclgmbi_2-5m/cclgmbi18.tif",
                "large-files/cclgmbi_2-5m/cclgmbi19.tif", "large-files/cclgmbi_2-5m/cclgmbi2.tif",
                "large-files/cclgmbi_2-5m/cclgmbi3.tif", "large-files/cclgmbi_2-5m/cclgmbi4.tif",
                "large-files/cclgmbi_2-5m/cclgmbi5.tif", "large-files/cclgmbi_2-5m/cclgmbi6.tif",
                "large-files/cclgmbi_2-5m/cclgmbi7.tif", "large-files/cclgmbi_2-5m/cclgmbi8.tif",
                "large-files/cclgmbi_2-5m/cclgmbi9.tif")

mean_temp_rast <- rast("large-files/cclgmbi_2-5m/cclgmbi1.tif")

clim_raster<- rast(clim_files)


signbase_sf$climate <- extract(clim_raster, signbase_sf)

mean_temp_rast <- rast("large-files/cclgmbi_2-5m/cclgmbi1.tif")

signbase_sf$mean_temperature <- extract(mean_temp_rast, signbase_sf)

ggplot(Europe) + 
  geom_spatraster(data = mean_temp_rast) +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

