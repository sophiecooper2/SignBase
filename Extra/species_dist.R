library(geodata)
library(predicts)
library(dismo)
list_of_rasters <- c("data/cclgmbi_2-5m/cclgmbi1.tif", "data/cclgmbi_2-5m/cclgmbi10.tif", 
                     "data/cclgmbi_2-5m/cclgmbi11.tif", "data/cclgmbi_2-5m/cclgmbi12.tif",
                     "data/cclgmbi_2-5m/cclgmbi13.tif", "data/cclgmbi_2-5m/cclgmbi14.tif",
                     "data/cclgmbi_2-5m/cclgmbi15.tif", "data/cclgmbi_2-5m/cclgmbi16.tif",
                     "data/cclgmbi_2-5m/cclgmbi17.tif", "data/cclgmbi_2-5m/cclgmbi18.tif",
                     "data/cclgmbi_2-5m/cclgmbi19.tif", "data/cclgmbi_2-5m/cclgmbi2.tif",
                     "data/cclgmbi_2-5m/cclgmbi3.tif", "data/cclgmbi_2-5m/cclgmbi4.tif",
                     "data/cclgmbi_2-5m/cclgmbi5.tif", "data/cclgmbi_2-5m/cclgmbi6.tif",
                     "data/cclgmbi_2-5m/cclgmbi7.tif", "data/cclgmbi_2-5m/cclgmbi8.tif",
                     "data/cclgmbi_2-5m/cclgmbi9.tif")
clim_data <- raster::stack(list_of_rasters)
clim_data <- rast(clim_data)

max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))


sample_extent <- geographic_extent * 1.25

clim_data <- terra::crop(x = clim_data, y = sample_extent)

obs_data <- st_as_sf(signbase_unique_groups, 
           coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)

obs_data$climate <- extract(clim_data, obs_data)

obs_data1 <- obs_data %>% 
  filter(group == 1) %>% 
  dplyr::select(latitude, longitude)

clim_data_1 <- obs_data %>% 
  filter(group == 1) %>% 
  dplyr::select(climate, latitude, longitude)

maxent(clim_data, p, a=NULL, factors=NULL, removeDuplicates=TRUE, nbg=10000)

obs_data1 <- obs_data
  filter(group == 1) %>% 
  dplyr::select(latitude, longitude) %>% 
  st_as_sf()


clim

max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))


sample_extent <- geographic_extent * 1.25

clim_data <- terra::crop(x = clim_data, y = sample_extent)

set.seed(20210707)

background <- spatSample(x = bioclim_data,
                         size = 1000,
                         values = FALSE,
                         na.rm = TRUE,
                         xy = TRUE)
presence <- obs_data1[, c("longitude", "latitude")]
presence$pa <- 1
absence <- as.data.frame(background)
colnames(absence) <- c("longitude", "latitude")
absence$pa <- 0
all_points <- rbind(presence, absence)
bioclim_extract <- extract(x = bioclim_data,
                           y = all_points[, c("longitude", "latitude")],
                           ID = FALSE)

points_climate <- cbind(all_points, bioclim_extract)


library(dismo)
maxent(clim_data)



# Identify columns that are latitude & longitude
drop_cols <- which(colnames(points_climate) %in% c("longitude", "latitude"))

# Remove the geographic coordinates from the data frame
points_climate <- points_climate[, -drop_cols]

# Create vector indicating fold to separate training and testing data
fold <- folds(x = points_climate,
              k = 5,
              by = points_climate$pa)

# Separate data into training and testing sets
testing <- points_climate[fold == 1, ]
training <- points_climate[fold != 1, ]

# Build a model using training data
glm_model <- glm(pa~., data = training, family = binomial())

# Get predicted values from the model
glm_predict <- predict(bioclim_data, glm_model, type = "response")

# Use testing data for model evaluation
glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ],
                        a = testing[testing$pa == 0, ],
                        model = glm_model,
                        type = "response")

# Determine minimum threshold for "presence"
glm_threshold <- glm_eval@thresholds$max_spec_sens

# Plot the results
# Plot base map
plot(my_map, 
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(glm_predict > glm_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab")) # only color for second element ("present")

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

# Redraw those country borders
plot(my_map, add = TRUE, border = "grey5")
