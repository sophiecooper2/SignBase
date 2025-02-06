## Test with reduced timeframe
library(tidyverse)
library(magrittr)
library(rcarbon)
signbase_full <- read_csv("data/signBase_Version1.0.csv")

signbase_full_clean <- signbase_full %>% 
  filter(site_name != "Willendorf",
         site_name != "Riparo di Fontana Nuova",
         site_name != "Muralovka",
         site_name != "Shanidar Cave",
         site_name != "Hayonim Cave",
         site_name != "El Salitre",
         site_name != "Grotte De La Princesse Pauline",
         site_name != "Šandalja II",
         site_name != "Les Cottés",
         site_name != "Trou al'Wesse") %>% 
  dplyr::select(-other, -rectangle, -circumline, -pinleft, -pinright, -star, -circumspiral, -circumnotch, -maccaroni)

signbase_years <- signbase_full_clean %>% 
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

signbase_years_cal$object_id <- signbase_years$object_id

signbase_full_clean <- inner_join(signbase_full_clean, signbase_years_cal)

signbase_full_clean <- signbase_full_clean %>% 
  filter(MedianBP > 37500) %>% 
  filter(MedianBP < 42500)


artifact_data <- signbase_full_clean %>% 
  column_to_rownames("object_id") %>% 
  dplyr::select(line:concenline)

artifact_data <- artifact_data %>% 
  mutate(row_sums = rowSums(.)) %>% 
  filter(row_sums >= 2) %>% 
  dplyr::select(-row_sums)

# seriation

library(ggpubr)
artifact_matrix <- as.matrix(artifact_data %>% 
                               mutate(across(everything(), ~replace(., . > 1, 1))))
rownames(artifact_matrix) <- rownames(artifact_data)
mplot <- function(x,...){
  d <- data.frame(x = rep(1:ncol(x), each = nrow(x)),
                  y = rep(nrow(x):1, ncol(x)),
                  z = as.integer(x))
  ggplot(d, aes(x, y, fill = factor(z))) +
    geom_tile(col = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = c("0" = "white", "1" = "black")) +
    coord_equal(expand = FALSE) +
    scale_x_continuous(breaks = 1:ncol(x), 
                       labels = colnames(x),
                       position = "top") +
    scale_y_continuous(breaks = 1:nrow(x), 
                       labels = rev(rownames(x))) +
    theme(axis.text = element_text(size = rel(0.5)),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.25)) +
    labs(x = "Sign Type",
         y = "Site Name",
         fill = "Count") +
    rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)}
increase_focus <- function(x){
  mcp <- apply(
    x, 
    MARGIN = 1, 
    FUN = \(z){ mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  x <- x[order(mcp), ]
  mrp <- apply(
    x, 
    MARGIN = 2, 
    FUN = \(z){ mean(which(z == 1)) }, 
    simplify = FALSE) |> unlist()
  x[, order(mrp)]}
concentrate <- function(x){
  old <- x
  not_identical <- TRUE
  while(not_identical){
    new <- increase_focus(old)
    not_identical <- !identical(old, new)
    old <- new}
  new}
artifact_matrix |> concentrate() |> mplot()

# map of sites 

library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(sf)

library(ggpubr)


signbase_sf <- 
  st_as_sf(signbase_unique_groups, 
           coords = c("longitude", "latitude"),
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
  theme_minimal()



