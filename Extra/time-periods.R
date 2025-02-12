library(tidyverse)
library(magrittr)
library(rcarbon)
library(readxl)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>%  # 511 rows
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II") %>% 
  dplyr::select(-other, -rectangle)

signbase_years <- signbase_full_clean %>%  # 446
  drop_na(date_bp_max_min) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+\\/\\-", "±")) %>% 
  mutate(date_bp_max_min = str_replace_all(date_bp_max_min, "\\+", "±")) %>% 
  separate(date_bp_max_min,
           sep = " - ",
           c("date_bp_max", "date_bp_min"),
           remove = FALSE) %>% 
  mutate(date_bp_max = ifelse(str_detect(date_bp_max, "\\/"),
                              str_extract(date_bp_max, ".*?(?=\\/)"),
                              date_bp_max)) %>% 
  mutate(date_bp_min = ifelse(str_detect(date_bp_min, "\\/"),
                              str_extract(date_bp_min, ".*?(?=\\/)"),
                              date_bp_min)) %>% 
  separate(date_bp_max,
           sep = "±",
           c("date_bp_max_age",
             "date_bp_max_error")) %>% 
  separate(date_bp_min,
           sep = "±",
           c("date_bp_min_age",
             "date_bp_min_error")) %>% 
  drop_na(date_bp_max_age,
          date_bp_max_error) %>% 
  mutate(date_bp_max_age = parse_number(date_bp_max_age),
         date_bp_max_error = parse_number(date_bp_max_error))

signbase_years_cal <- 
  rcarbon::calibrate(signbase_years$date_bp_max_age,
                     signbase_years$date_bp_max_error,
                     verbose = FALSE) %>% 
  summary() %>% 
  data_frame()

signbase_years$MedianBP <- signbase_years_cal$MedianBP


signbase_full_clean <- signbase_full_clean %>% 
  inner_join(signbase_years %>% 
              select(object_id, MedianBP))

signbase_full_clean <- signbase_full_clean %>%  # 446
  mutate(time_period = case_when((MedianBP > 42999) ~ "transitional",
                                 (MedianBP > 39899) ~ "proto_aurignacian",
                                 (MedianBP > 36999) ~ "early_aurignacian",
                                 (MedianBP > 31999) ~ "evolved_aurignacian"))


ggplot(signbase_full_clean) +
  aes(x = MedianBP, fill = time_period) +
  geom_bar(width = 150)

lat_long_df <- signbase_full_clean %>% 
  dplyr::select(site_name, longitude, latitude) %>% 
  distinct(site_name, .keep_all = TRUE)

trans_unique_data <- signbase_full_clean %>% 
  filter(time_period == "transitional") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "transitional")

proto_unique_data <- signbase_full_clean %>% 
  filter(time_period == "proto_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "proto_aurignacian")

early_unique_data <- signbase_full_clean %>% 
  filter(time_period == "early_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "early_aurignacian")

evolved_unique_data <- signbase_full_clean %>% 
  filter(time_period == "evolved_aurignacian") %>% 
  mutate(longitude = as.character(longitude),
         latitude = as.character(latitude)) %>% 
  group_by(site_name) %>%
  summarize(across(where(is.numeric), sum)) %>% 
  left_join(lat_long_df) %>% 
  mutate(time_period = "evolved_aurignacian")


library(rnaturalearthdata)
library(ggrepel)
library(sf)

library(ggpubr)
time_unique_data_full <-rbind(trans_unique_data, proto_unique_data, early_unique_data, evolved_unique_data)

signbase_sf <- time_unique_data_full  %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           remove = FALSE,
           crs = 4326)
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]

ggplot(Europe) +
  geom_sf() +
  geom_sf(data = signbase_sf) +
  coord_sf(xlim = c(-10,30), 
           ylim = c(35,53), 
           expand = FALSE) +
  geom_text_repel(data = signbase_sf,
                  aes(x = longitude ,
                      y = latitude,
                      label = site_name),
                  max.overlaps = 30,
                  size = 2) +
  theme_minimal() +
  facet_wrap(~time_period)

trans_artifact_data <- trans_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

proto_artifact_data <- proto_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

early_artifact_data <- early_unique_data %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

evolved_artifact_data <- evolved_unique_data%>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:star)

library(ggpubr)
artifact_matrix <- as.matrix(ser_data_evolved %>% 
                               mutate(across(everything(), ~replace(., . > 1, 1))))
rownames(artifact_matrix) <- rownames(ser_data_evolved)

# Define the mplot function
mplot <- function(x, ..., fill_colour = "black", title = NULL, row_labels = NULL, col_labels = NULL, country_colors = NULL) {
  # Create the data frame
  d <- data.frame(
    x = rep(1:ncol(x), each = nrow(x)),
    y = rep(nrow(x):1, ncol(x)),
    z = as.integer(x)
  )
  
  # Add country colors to the data frame if provided
  if (!is.null(country_colors)) {
    d$country_color <- rep(country_colors, ncol(x))
  }
  
  # Use custom row/column labels if provided, otherwise use defaults
  if (is.null(col_labels)) col_labels <- colnames(x)
  if (is.null(row_labels)) row_labels <- rev(rownames(x))
  
  # Plot
  p <- ggplot(d, aes(x, y)) +
    geom_tile(col = "grey50", linewidth = 0.5, aes(fill = ifelse(z == 1, country_color, "white"))) +
    scale_fill_identity() +  # Use the fill values directly
    coord_equal(expand = FALSE) +
    scale_x_continuous(
      breaks = 1:ncol(x),
      labels = col_labels,
      position = "top"
    ) +
    scale_y_continuous(
      breaks = 1:nrow(x),
      labels = row_labels
    ) +
    theme(
      axis.text = element_text(size = rel(0.5)),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.25)
    ) +
    labs(x = "Sign Type", y = "Site Name", fill = "Count", title = title) +
    rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)
  
  p
}

# Define the increase_focus function
increase_focus <- function(x, country = NULL) {
  # Reorder rows by the mean position of 1s in each row
  mcp <- apply(
    x, 
    MARGIN = 1, 
    FUN = \(z) { mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  row_order <- order(mcp, na.last = TRUE)
  x <- x[row_order, ]
  
  # Reorder the country vector if provided
  if (!is.null(country)) {
    country <- country[row_order]
  }
  
  # Reorder columns by the mean position of 1s in each column
  mrp <- apply(
    x, 
    MARGIN = 2, 
    FUN = \(z) { mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  x <- x[, order(mrp, na.last = TRUE)]
  
  # Return the reordered matrix and country vector
  list(matrix = x, country = country)
}

# Define the concentrate function
concentrate <- function(x, country = NULL, max_iter = 100) {
  old <- x
  not_identical <- TRUE
  iter <- 0
  
  while (not_identical && iter < max_iter) {
    result <- increase_focus(old, country)
    new <- result$matrix
    country <- result$country
    not_identical <- !identical(old, new)
    old <- new
    iter <- iter + 1
  }
  
  if (iter == max_iter) {
    warning("Reached maximum iterations without convergence.")
  }
  
  list(matrix = new, country = country)
}


# get country names, for example
x_df <- 
  evolved_unique_data%>% 
  left_join(signbase_full_clean %>% 
              select(site_name,
                     country),
            by = "site_name")

countries <- x_df$country

# Generate a named vector of colors, one for each unique country
unique_countries <- unique(countries)
color_palette <-  RColorBrewer::brewer.pal(length(unique_countries), "Set3")   # Generate distinct colors
row_colors <- setNames(color_palette, unique_countries) 
countries_colors <- row_colors[countries]

# Concentrate the matrix and reorder the country vector
result <- concentrate(artifact_matrix, country = countries)
concentrated_matrix <- result$matrix
reordered_countries <- result$country

# Map countries to colors
country_colors <- row_colors[reordered_countries]

# Plot the concentrated matrix with country colors
mplot(
  concentrated_matrix,
  country_colors = country_colors
)



to_investigate<- signbase_full %>% 
  filter(is.na(date_bp_max_min))





