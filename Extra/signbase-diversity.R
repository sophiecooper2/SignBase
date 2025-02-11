library(vegan)

artifact_div <- vegan::diversity(artifact_data, index = "shannon")

abundance_data$diversity <- artifact_div

##plot diversity against abundance

ggplot(abundance_data) +
  aes(x = sign_total,
      y = diversity) +
  geom_point() +
  geom_smooth(se = FALSE)

## plot diversity by site
library(ggpubr)
ggplot(abundance_data) +
  aes(x = site_name, y = sign_total, fill = diversity) +
  geom_col() +
  rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)

## map diversity by site
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
          aes(size = diversity)) +
  scale_size_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)) +
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
  facet_wrap(~group) +
  labs(color = "Diversity",
       size = "Sign Count")


## Calculate the diversity of groups
div_groups <- abundance_data %>% pull(group)
artifact_div_groups <- diversity(artifact_data, index = "shannon", groups = div_groups)
group_diversity <- data_frame(artifact_div_groups) %>% 
  rownames_to_column("group")



#---------------------------------------------
# BM from Aleah and Eva

library(tabula)
library(tidyverse)
library(rnaturalearth)
library(ggrepel)
library(dplyr)
library(cowplot)

signbase <- read_csv("data/signBase_Version1.0.csv")

# Create a new dataframe with site_name as rows and object_type as the different columns
signbase_transformed <- signbase %>%
  group_by(site_name, object_type) %>% 
  summarise(count = n(), .groups = "drop") %>%
  filter(count >= 5) %>% 
  pivot_wider(names_from = object_type, 
              values_from = count, 
              values_fill = list(count = 0))

# View the transformed dataframe
print(signbase_transformed)

# BM added this plot
signbase_transformed %>% 
  pivot_longer(-site_name) %>% 
  ggplot() +
  aes(site_name, 
      value,
      fill = name) +
  geom_col() +
  coord_flip()

## Measure diversity by comparing to simulated assemblages
set.seed(12345)

signbase_transformed|>
  heterogeneity(method = "shannon") |>
  simulate() |>
  plot()

signbase_transformed |>
  richness(method = "observed") |>
  simulate() |>
  plot()

#---------------------------------------
# diversity of signs at each site 

# need to create these plotting functions because the 
# tabula package has changed recently 

# functions from https://github.com/tesselle/tabula/blob/b831dc45a11c3a4af7b5bee16d8c1dc9482f5fa5/R/plot_diversity.R

as_factor <- function(x, reverse = FALSE) {
  lvl <- unique(x)
  if (reverse) {
    lvl <- rev(lvl)
  }
  factor(x, levels = lvl)
}

to_long <- function(from, factor = FALSE, reverse = FALSE) {
  x <- data.frame(
    row = as.vector(row(from, as.factor = factor)),
    column = as.vector(col(from, as.factor = factor)),
    value = as.vector(from),
    stringsAsFactors = FALSE
  )
  if (factor) {
    x$row <- as_factor(x$row, reverse = reverse)
    x$column <- as_factor(x$column, reverse = reverse)
  }
  return(x)
}

autoplot.DiversityIndex <- function(object, ...) {
  ## Prepare data
  count <- cbind.data.frame(
    label = object@labels,
    x = object@size,
    y = object@.Data
  )
  
  ## Simulated assemblages
  gg_sim <- NULL
  if (length(object@simulation) != 0) {
    # Build a long table for ggplot2
    refined <- object@simulation
    
    sim_stacked <- to_long(refined[, -c(1)], factor = TRUE)
    sim <- cbind.data.frame(
      size = refined[, 1],
      sim_stacked,
      Estimate = ifelse(sim_stacked[["column"]] == "mean", "mean", "conf. int.")
    )
    gg_sim <- ggplot2::geom_path(
      mapping = ggplot2::aes(
        x = .data$size,
        y = .data$value,
        colour = .data$Estimate,
        group = .data$column
      ),
      data = sim,
      na.rm = TRUE,
      inherit.aes = FALSE
    )
  }
  
  y_lab <- switch (
    class(object),
    HeterogeneityIndex = "Heterogeneity",
    EvennessIndex = "Evenness",
    RichnessIndex = "Richness",
    "Diversity"
  )
  
  ## ggplot
  ggplot2::ggplot(data = count) +
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label) +
    ggplot2::geom_point() +
    gg_sim +
    ggplot2::scale_x_log10(name = "Sample size") +
    ggplot2::scale_y_continuous(
      name = sprintf("%s (%s)", y_lab, object@method)
    )
}


# RarefactionIndex =============================================================

autoplot.RarefactionIndex <- function(object, ...) {
  ## Prepare data
  count <- to_long(object)
  count$Entity <- rownames(object)
  count$size <- rep(object@size, each = nrow(object))
  
  ## ggplot
  ggplot2::ggplot(data = count) +
    ggplot2::aes(x = .data$size, 
                 y = .data$value, 
                 colour = .data$Entity,
                 group = .data$Entity, 
                 label = .data$Entity) +
    ggplot2::geom_line(na.rm = TRUE)
}

#------------------------------------------------------------
# BM: my code to compute and plot diversity and richness

signs <-
  signbase %>% 
  select(site_name,
        # c(line:star)
         cross, v, star, zigzag, zigzagrow, grid, hashtag, hatching, anthropomorph,zoomorph, vulva) %>% 
  group_by(site_name) %>% 
  summarise(across(where(is.numeric), ~ sum(.))) %>% 
  column_to_rownames("site_name")

diversity_index <- 
  heterogeneity(signs, method = "shannon")

diversity_sim <- simulate(diversity_index)

autoplot.DiversityIndex(diversity_sim) +
  geom_text_repel(aes(label = diversity_sim@labels),
                  size = 3) +
  guides(colour = "none") +
  theme_minimal()

# richness at each site 

richness <- richness(signs, method = "count")
richness_sim <- simulate(richness)

autoplot(richness_sim) +
  geom_text_repel(aes(label = richness_sim@labels)) +
  guides(colour = "none") +
  theme_minimal()





