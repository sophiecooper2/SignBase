#| label: fig-neighbor-net
#| fig-cap: "Neighbor-Net analysis of the data"
##network analysis
library(vegan)
library(phangorn)

## neighborNet
jac <- vegdist(artifact_data, "jaccard", binary = TRUE)

dm <- as.matrix(jac)
nnet <- neighborNet(dm)

par("mar" = rep(1, 4))

# Reorder the tips according to the seriation grouping
nnet$tip.label <- nnet$tip.label[match(group_df$site_name, nnet$tip.label)]  

# Plot the network without tip labels initially
plot(nnet, 
     show.tip.label = TRUE,
     cex = 0.5)


## try new neighbor net
library(ggraph)
library(tidygraph)
library(igraph)
library(statnet)
library(intergraph)
library(FastKNN)
library(ggpubr)
library(reshape2)
library(patchwork)
library(ggmap)
library(seriation)
library(purrr)
library(tnet)
library(argosfilter)

disim <- 1 - dm
disim[disim<0.5] <- 0
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
  theme(text= element_text(size = 20), 
        panel.background = element_rect(fill = 'white'))

# BM try yet another

# Ensure all edge lengths are positive
nnet$edge.length <- pmax(nnet$edge.length, 1e-6)

# Create the graph object
g <- graph_from_edgelist(nnet$edge, directed = FALSE)
E(g)$weight <- nnet$edge.length

# Create a layout for the nodes
layout <- layout_with_fr(g)  # Fruchterman-Reingold layout

# Create a data frame for nodes (coordinates and labels)
nodes <- data.frame(
  x = layout[, 1],  # x-coordinates
  y = layout[, 2],  # y-coordinates
  label = c(nnet$tip.label, rep("internal", nnet$Nnode))
)

# Create a data frame for edges (start and end coordinates)
edges_df <- data.frame(
  from = nnet$edge[, 1],  # Start node of each edge
  to = nnet$edge[, 2],    # End node of each edge
  weight = nnet$edge.length  # Corresponding edge weights
)

# Create a data frame for the edge coordinates
edges_df$x_start <- nodes$x[edges_df$from]  # x-coordinate of start node
edges_df$y_start <- nodes$y[edges_df$from]  # y-coordinate of start node
edges_df$x_end <- nodes$x[edges_df$to]     # x-coordinate of end node
edges_df$y_end <- nodes$y[edges_df$to]     # y-coordinate of end node

# Plot the network
library(ggrepel)
ggplot() +
  geom_segment(data = edges_df, 
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               color = "grey", size = 1) +
  geom_point(data = nodes %>% 
               filter(label != "internal"),
             aes(x = x, y = y), color = "black", size = 3) +
  geom_text_repel(data = nodes %>% 
                    filter(label != "internal"), 
                  aes(x = x, y = y, label = label), 
                  size = 3, vjust = -1) +
  theme_void() +
  theme(legend.position = "none")

