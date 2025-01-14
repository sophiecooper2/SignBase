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
