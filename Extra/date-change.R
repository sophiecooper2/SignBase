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
  dplyr::select(-other, -rectangle, -circumline, -pinleft, -pinright, -star, -circumspiral)

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

# network analysis

library(phangorn)
library(ggraph)
library(statnet)
library(vegan)
jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
dm <- as.matrix(jac)
disim <- 1 - dm
disim[disim<0.35] <- 0
disim_net <- network(disim,
                     directed=F,
                     ignore.eval=F,
                     names.eval='weight')
disim_net %v% 'vertex.names' <- 
  row.names(artifact_data)

set.seed(500)

ggraph(disim_net, layout="fr") +
  geom_edge_link(aes(width = weight, 
                     alpha=weight), 
                 edge_colour = "black", 
                 show.legend=T) +
  scale_edge_width(range=c(0.5,1.5)) +
  scale_edge_colour_gradient(low = "#CCCCCC",
                             high = "#000000") +
  geom_node_point(alpha=1, size=5) +
  geom_node_text(aes(label=row.names(artifact_data)), 
                 size=6, 
                 family= "Arial", 
                 repel=T) +
  theme(text= element_text(size = 15), 
        panel.background = element_rect(fill = 'white'))


#pcoa
library(ecodist)
library(ape)
pcoa <- pcoa(jac)
pcoa_df <- data.frame(pcoa1 = pcoa$vectors[,1], 
                      pcoa2 = pcoa$vectors[,2])
pcoa_df <- pcoa_df %>% 
  mutate(site_name = rownames(artifact_data))
library(ggrepel)
ggplot(pcoa_df) +
  aes(x=pcoa1, 
      y=pcoa2) +
  geom_point() +
  labs(x = "PC1",
       y = "PC2",) +
  theme(title = element_text(size = 10)) +
  geom_text_repel(aes(label = site_name), 
                  size = 2,
                  max.overlaps = 20)

#create groups

groups <- list("1" = c("Abri Pataud", "Riparo Bombrini", "Grottes de Fonds-de-Forêt"), 
               "2" = c("Gatzarria", "Grotte de la Verpillière I", "Hohlenstein-Stadel", "Mladeč"),
               "3" = c("Labeko Koba", "Vindija Cave"),
               "4" = c( "Geissenklösterle", "Hohle Fels", "Vogelherd", "Spy", "Solutré","Grotte du Renne"),
               "5" = c("Castanet",  "Blanchard", "La Ferrassie", "Cellier"))

group_df <- 
  enframe(groups, 
          name = "group", 
          value = "site_name") %>%
  unnest(cols = site_name)


jac_df <- jac %>% 
  as.matrix() %>% 
  as.data.frame()

signbase_unique_groups <- signbase_unique_groups %>% 
  left_join(group_df)

artifact_data <- signbase_unique_groups %>% 
  column_to_rownames("site_name") %>% 
  dplyr::select(line:concenline)               


# modularity test for groupings
library(igraph)
set.seed(500)
modul_matrix <-
  graph_from_adjacency_matrix(1 - dm, mode = "undirected")
mem <- as_membership(parse_number(signbase_unique_groups$group))
as_group <- modularity(modul_matrix, membership = mem)

# permanova test for groupings
perman <- adonis2(jac~as.factor(signbase_unique_groups$group), 
                  method = "jaccard",
                  sqrt.dist = TRUE)
perman <- perman %>% 
  data_frame() %>% 
  slice_head()
perman_r <- perman$R2
perman_p <- perman$`Pr(>F)`


dispersion <- betadisper(jac, group = signbase_unique_groups$group)
plot(dispersion, hull = FALSE, ellipse=TRUE)


## mantel test

site_distances <- dist(cbind(signbase_unique_groups$longitude, signbase_unique_groups$latitude))
mantel_distance <- vegan::mantel(jac, site_distances, permutations = 1000)
p_value_distance_correlation <- round(mantel_distance$signif, 3)
r_value_distance_correlation <- round(mantel_distance$statistic, 3)


signbase_full_clean <- signbase_full_clean %>% 
  left_join(group_df)



ggplot(signbase_full_clean) +
  aes(x = MedianBP, fill = group) +
  geom_bar(width = 150) +
  labs(x = "Median calibrated BP")


