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
          mapping = aes(size = diversity), 
          scale_size_manual(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2))) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)




## Biogeographical regions of Europe - (https://pmc.ncbi.nlm.nih.gov/articles/PMC7340631/#sec24)
#3 as determined by the European Environment Agency

bio_geo_raster <- rast("data/bdj-08-e53720-s002.tif")
bio_geo_raster_df <- 
  as.data.frame(bio_geo_raster, xy = TRUE) %>% 
  mutate(`bdj-08-e53720-s002` = as.factor(`bdj-08-e53720-s002`))

res(bio_geo_raster)

signbase_sf$biogeographical_region <- terra::extract(bio_geo_raster, signbase_sf)

signbase_sf <- signbase_sf %>% 
  mutate(biogeographical_region = biogeographical_region$bdj)

ggplot(Europe) + 
  geom_raster(data = bio_geo_raster_df,
                  aes(x = x,
                      y = y,
                      fill = `bdj-08-e53720-s002`)) +
  #scale_fill_discrete() +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)

## elevation data
library(geodata)
elev_rast <- elevation_global(res = 0.5, path = "/Users/sophiecooper/Independent Research Study/SignBase/large-files") %>% 
  crop(rast_ext)


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

ggplot(signbase_sf %>% 
         filter(sign_count >= 5)) +
  aes(x = sign_count, y = elevation) +
  geom_point()

# diversity vs elevation

ggplot(signbase_sf) +
  aes(x = diversity, y = elevation) +
  geom_point()

# boxplot of elevation by group

ggplot(signbase_sf) +
  aes(x = reorder(group, elevation),
      y = elevation) +
  geom_boxplot()

##boxplot of elevation by signtype

sign_long <- signbase_sf %>% 
  pivot_longer(cols = line:star)

sign_long <- sign_long %>% 
  select(site_name, group, environmental_zone, biogeographical_region, elevation, aspect, slope, mean_temperature, name, value) %>% 
  filter(value!= 0)

table(sign_long$name)

sign_long <- sign_long %>% 
  filter(name == "notch" | name == "line"| name =="obline" |
           name == "dot" | name == "hatching" | name == "cross" |
           name == "circumnotch" | name == "vulva" | name == "grid" |
           name == "zigzag")

ggplot(sign_long) +
  aes(x = name, y = elevation) + 
  geom_boxplot()

##climate data

## creation of Koggen Geiger climate zones
library(climetrics)
rast_ext <- ext(-10, 30, 35, 53)

month_avg_min_tmp_files <- list.files(path = "large-files/cclgmtn_2-5m/", full.names = TRUE)

month_avg_min_tmp <- rast(month_avg_min_tmp_files) %>% 
  crop(rast_ext)

month_avy_max_tmp_files <- list.files(path = "large-files/cclgmtx_2-5m/", full.names = TRUE)

month_avg_max_tmp <- rast(month_avy_max_tmp_files) %>% 
  crop(rast_ext)

month_precip_files <- list.files(path = "large-files/cclgmpr_2-5m/", full.names = TRUE)

month_precip <- rast(month_precip_files) %>% 
  crop(rast_ext)

mean_month_temp_rast <- ((month_avg_min_tmp + month_avg_max_tmp)/2)

koppen_geiger_zones <- kgc(p = month_precip, tmin = month_avg_min_tmp, tmax = month_avg_max_tmp, tmean = mean_month_temp_rast)

koppen_geiger_zones_df <- as.data.frame(koppen_geiger_zones, xy = TRUE)

koppen_geiger_zones_df <- koppen_geiger_zones_df %>% 
  mutate(mean = as.factor(mean))

ggplot(Europe) + 
  geom_raster(data = koppen_geiger_zones_df,
              aes(x = x,
                  y = y,
                  fill = mean)) +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30),
           ylim = c(35,53), 
           expand = FALSE) +
  facet_wrap(~group)


##pca on climate data


clim_isodata_df <- as.data.frame(month_avg_min_tmp, xy= TRUE) %>% 
  st_as_sf(coords = c("x", "y"),
           remove = FALSE,
           crs = 4326)

clim_isodata_df$elevation <- extract(elev_rast, clim_isodata_df)

clim_isodata_df$max_monthly_temp <- extract(month_avg_max_tmp, clim_isodata_df)

clim_isodata_df$slope <- extract(slope_data, clim_isodata_df)


clim_isodata_df$precipitation <- extract(month_precip, clim_isodata_df)

clim_isodata_df <- clim_isodata_df %>% 
  unnest(c(max_monthly_temp, precipitation, elevation, slope), names_repair = "universal") %>% 
  drop_na() %>%
  st_drop_geometry()

clim_xy <- clim_isodata_df %>% 
  select(x, y)

clim_xy$rn <- paste((clim_xy$x, clim_xy$y, sep = ","))

clim_isodata_df <- clim_isodata_df %>% 
  select(-x)

colnames(clim_isodata_df) <-  c("northing", "min_temp_jan", "min_temp_oct", "min_temp_nov", 
                               "min_temp_dec", "min_temp_feb", "min_temp_mar", "min_temp_apr",
                               "min_temp_may", "min_temp_june", "min_temp_jul", "min_temp_aug",
                               "min_temp_sep", "elevID", "elevation", "maxID", "max_temp_jan", "max_temp_oct", 
                               "max_temp_nov","max_temp_dec", "max_temp_feb", "max_temp_mar",
                               "max_temp_apr","max_temp_may","max_temp_june","max_temp_jul", 
                               "max_temp_aug", "max_temp_sep", "slopeID", "slope", "precipID", "precip_jan",
                               "precip_oct", "precip_nov", "precip_dec", "precip_feb", "precip_mar",
                               "precip_apr", "precip_may", "precip_june", "precip_jul", "precip_aug",
                               "precip_sep")

clim_isodata_df <- clim_isodata_df %>% 
  select(-maxID, -precipID, -elevID, -slopeID)

rownames(clim_isodata_df) <- clim_xy$rn

library(factoextra)
library(FactoMineR)
hire_data <- clim_isodata_df

clim_pca <- PCA(clim_isodata_df, graph = FALSE)
clim_hcpc <- HCPC(clim_pca, graph = FALSE)
fviz_cluster(clim_hcpc,
             repel = TRUE,
             show.clust.cent = FALSE, 
             color_labels_by_k = FALSE,
             ellipse.alpha = 0,
             ggtheme = theme_bw(),
             main = "Factor map")

hcpc_dataframe <- clim_hcpc$data.clust
