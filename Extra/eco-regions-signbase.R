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

env_zone_data <- signbase_sf %>% 
  mutate(environmental_zone_clean = as.factor(environmental_zone))
  

ggplot(Europe) + 
  geom_spatraster(data = env_raster_reproj) +
  geom_sf(data = env_zone_data, mapping = aes(fill = as.factor(environmental_zone_clean))) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

## plot out env zone by group 
ggplot(signbase_sf) +
  aes(x = group, fill= as.factor(environmental_zone)) +
  geom_bar()

## diversity vs env zone

signbase_div <- vegan::diversity(artifact_data, index = "shannon")

signbase_sf$diversity <- signbase_div

ggplot(signbase_sf) +
  aes(x = environmental_zone, y = diversity, color = group) +
  geom_point()

ggplot(Europe) + 
  geom_spatraster(data = env_raster_reproj) +
  geom_sf(data = signbase_sf, 
          mapping = aes(size = diversity, )) +
  scale_size_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25)) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)



## Biogeographical regions of Europe - (https://pmc.ncbi.nlm.nih.gov/articles/PMC7340631/#sec24)
#3 as determined by the European Environment Agency

bio_geo_raster <- rast("data/bdj-08-e53720-s002.tif")

res(bio_geo_raster)

signbase_sf$biogeographical_region <- terra::extract(bio_geo_raster, signbase_sf)



signbase_sf <- signbase_sf %>% 
  mutate(biogeographical_region = biogeographical_region$bdj)

ggplot(Europe) + 
  geom_spatraster(data = bio_geo_raster) +
  scale_fill_grass_d(palette = "viridis") +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

## elevation data
library(geodata)
elev_rast <- elevation_global(res = 0.5, path = "/Users/sophiecooper/Independent Research Study/SignBase/large-files")


signbase_sf$elevation <- extract(elev_rast, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(elevation = elevation$wc2.1_30s_elev)

aspect_data <- terrain(elev_rast, v = "aspect", unit = "degrees", neighbors=8)
slope_data <- terrain(elev_rast, v = "slope", unit = "degrees", neighbors=8)

signbase_sf$aspect <- extract(aspect_data, signbase_sf)
signbase_sf$slope <- extract(slope_data, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(aspect = aspect$aspect)

signbase_sf <- signbase_sf %>% 
  mutate(slope = slope$slope)

ggplot(Europe) + 
  geom_spatraster(data = elev_rast) +
  geom_sf(data = signbase_sf, color = "red") +
  scale_fill_grass_d(palette = "viridis") +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

# sign count vs elevation

signbase_sf$sign_count <- abundance_data$sign_total

ggplot(signbase_sf) +
  aes(x = sign_count, y = elevation) +
  geom_point()

# diversity vs elevation

ggplot(signbase_sf) +
  aes(x = diversity, y = elevation) +
  geom_point()

# boxplot of elevation by group

ggplot(signbase_sf) +
  aes(x = group, y = elevation) +
  geom_boxplot()

##boxplot of elevation by sign

sign_long <- signbase_sf %>% 
  pivot_longer(cols = line:star)

sign_long <- sign_long %>% 
  select(site_name, group, environmental_zone, biogeographical_region, elevation, aspect, slope, mean_temperature, name, value) %>% 
  filter(value!= 0)

table(sign_long$name)

##climate data


clim_files <- list.files(path = "large-files/cclgmbi_2-5m", full.names = TRUE)

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

