## elevation data
library(terra)
library(tidyterra)
library(geodata)

rast_ext <- ext(-10, 30, 35, 53)

elev_rast <- elevation_global(res = 0.5, path = "/Users/sophiecooper/Independent Research Study/SignBase/large-files") %>% 
  crop(rast_ext)

slope_data <- terrain(elev_rast, v = "slope", unit = "degrees", neighbors=8)

month_avg_min_tmp_files <- list.files(path = "large-files/cclgmtn_2-5m/", full.names = TRUE)

month_avg_min_tmp <- rast(month_avg_min_tmp_files) %>% 
  crop(rast_ext)

month_avg_max_tmp_files <- list.files(path = "large-files/cclgmtx_2-5m/", full.names = TRUE)

month_avg_max_tmp <- rast(month_avg_max_tmp_files) %>% 
  crop(rast_ext)

month_precip_files <- list.files(path = "large-files/cclgmpr_2-5m/", full.names = TRUE)

month_precip <- rast(month_precip_files) %>% 
  crop(rast_ext)

clim_df <- as.data.frame(month_avg_min_tmp, xy= TRUE) %>% 
  st_as_sf(coords = c("x", "y"),
           remove = FALSE,
           crs = 4326)

clim_df$elevation <- extract(elev_rast, clim_df)

clim_df$max_monthly_temp <- extract(month_avg_max_tmp, clim_df)

clim_df$slope <- extract(slope_data, clim_df)

clim_df$precipitation <- extract(month_precip, clim_df)

clim_df <- clim_df %>% 
  st_drop_geometry() %>% 
  drop_na() %>% 
  unnest(c(max_monthly_temp, precipitation, elevation, slope), names_repair = "universal") %>% 
  mutate(northing = y)


colnames(clim_df) <-  c("x", "y", "min_temp_jan", "min_temp_oct", "min_temp_nov", 
                               "min_temp_dec", "min_temp_feb", "min_temp_mar", "min_temp_apr",
                               "min_temp_may", "min_temp_june", "min_temp_jul", "min_temp_aug",
                               "min_temp_sep", "elevID", "elevation", "maxID", "max_temp_jan", "max_temp_oct", 
                               "max_temp_nov","max_temp_dec", "max_temp_feb", "max_temp_mar",
                               "max_temp_apr","max_temp_may","max_temp_june","max_temp_jul", 
                               "max_temp_aug", "max_temp_sep", "slopeID", "slope", "precipID", "precip_jan",
                               "precip_oct", "precip_nov", "precip_dec", "precip_feb", "precip_mar",
                               "precip_apr", "precip_may", "precip_june", "precip_jul", "precip_aug",
                               "precip_sep", "northing")

clim_df <- clim_df %>% 
  dplyr::select(-maxID, -precipID, -elevID, -slopeID)


clim_rast <- as_spatraster(clim_df,
                   xycols = 1:2,
                   crs = "EPSG:4326")

library(ENMTools)
clim_pca <- raster.pca(clim_rast, n = 1)

clim_pca_rast <- clim_pca$rasters


ggplot(Europe) +
  geom_spatraster(data = clim_pca_rast)+
  scale_fill_grass_b(breaks = c(-17, -16, -15, -14, -13, -12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) +
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
  facet_wrap(~time_period) +
  theme()


signbase_sf$PCA <- extract(clim_pca_rast, signbase_sf) 

signbase_sf <- signbase_sf %>% 
  mutate(PCA = PCA$PC1)

library(ggbeeswarm)
ggplot(signbase_sf) +
  aes(x = time_period, 
      y = PCA,
      fill = group,
      colour = group) +
  geom_boxplot(outliers = FALSE,
               alpha = 0.1) +
  geom_quasirandom(size = 4,
                   dodge.width = 0.5,
                   alpha = 0.3) +
  theme_minimal()


## Permanova test for environmental zones - how much is the impact on group?

signbase_sf$clim <- terra::extract(clim_rast, signbase_sf)

signbase_sf <- signbase_sf %>% 
  unnest(clim)

min_temp_df <- signbase_sf %>% 
  dplyr::select(min_temp_jan:min_temp_sep) %>% 
  st_drop_geometry() %>% 
  mutate(avg = rowSums(.)/12)

signbase_sf$min_temp_avg <- min_temp_df$avg

max_temp_df <- signbase_sf %>% 
  dplyr::select(max_temp_jan:max_temp_sep) %>% 
  st_drop_geometry() %>% 
  mutate(avg = rowSums(.)/12)

signbase_sf$max_temp_avg <- max_temp_df$avg

precip_df <- signbase_sf %>% 
  dplyr::select(precip_jan:precip_sep) %>% 
  st_drop_geometry() %>% 
  mutate(avg = rowSums(.)/12)

signbase_sf$precip_avg <- precip_df$avg



proto_mantel <- perm_function(period = "proto_aurignacian", variable = min_temp_jan:northing, method = "mantel")






clim_pca_df<- as.data.frame(clim_pca_rast, xy = TRUE) %>% 
  mutate(PCA = PC1)

clim_pca_fill <- clim_pca$pca.object$x %>% 
  as_data_frame()

clim_pca_df$PC2 <- clim_pca_fill$PC2

signbase_sf <- signbase_sf %>% 
  left_join(clim_pca_df)

ggplot(signbase_sf) +
  aes(x = PCA, y = PC2,  color = group) +
  geom_point() +
  geom_text_repel(aes(label = site_name)) +
  facet_wrap(~time_period)



                            