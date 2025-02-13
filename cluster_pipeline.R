library(ggpubr)
library(phangorn)
library(ggraph)
library(statnet)
library(ecodist)
library(ape)
library(ggrepel)
library(vegan)
library(cowplot)
library(igraph)

mplot <- function(x, ..., fill_colour = "black", title = NULL, row_labels = NULL, col_labels = NULL, country_colors = NULL) {
  d <- data.frame(
    x = rep(1:ncol(x), each = nrow(x)),
    y = rep(nrow(x):1, ncol(x)),
    z = as.integer(x)
  )
  
  if (!is.null(country_colors)) {
    d$country_color <- rep(country_colors, ncol(x))
  }
  
  if (is.null(col_labels)) col_labels <- colnames(x)
  if (is.null(row_labels)) row_labels <- rev(rownames(x))
  
  p <- ggplot(d, aes(x, y)) +
    geom_tile(col = "grey50", linewidth = 0.5, aes(fill = ifelse(z == 1, country_color, "white"))) +
    scale_fill_identity() +  
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
    labs(x = "", y = "") +
    rotate_x_text(angle = 90, hjust = NULL, vjust = NULL)
  
  p
}

increase_focus <- function(x, country = NULL) {
  mcp <- apply(
    x, 
    MARGIN = 1, 
    FUN = \(z) { mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  row_order <- order(mcp, na.last = TRUE)
  x <- x[row_order, ]
  
  if (!is.null(country)) {
    country <- country[row_order]
  }
  
  mrp <- apply(
    x, 
    MARGIN = 2, 
    FUN = \(z) { mean(which(z == 1)) }, 
    simplify = FALSE
  ) |> unlist()
  x <- x[, order(mrp, na.last = TRUE)]
  
  list(matrix = x, country = country)
}

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


produce_clusters <- function(artifact_data, artifact_data_unique, method = "seriation"){
  artifact_data <- artifact_data %>% 
    select_if(~ !is.numeric(.) || sum(.) != 0)
  jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
  dm <- as.matrix(jac)
  
  if(method == "seriation"){
  artifact_matrix <- as.matrix(artifact_data %>% 
                                 mutate(across(everything(), ~replace(., . > 1, 1))))
  rownames(artifact_matrix) <- rownames(artifact_data)
  
  x_df <- artifact_data_unique %>% 
    left_join(signbase_full_clean,
              by = "site_name")
  countries <- x_df$country
  unique_countries <- unique(countries)
  color_palette <-  RColorBrewer::brewer.pal(length(unique_countries), "Set3")
  row_colors <- setNames(color_palette, unique_countries)
  result <- concentrate(artifact_matrix, country = countries)
  concentrated_matrix <- result$matrix
  reordered_countries <- result$country
  countries_colors <- row_colors[countries]
  country_colors <- row_colors[reordered_countries]

  final_plot <- mplot(concentrated_matrix,
    country_colors = country_colors)}
  
  if(method == "network"){
  disim <- 1 - dm
  disim[disim<0.5] <- 0
  disim_net <- network(disim,
                       directed=F,
                       ignore.eval=F,
                       names.eval='weight')
  disim_net %v% 'vertex.names' <- row.names(artifact_data)
  set.seed(500)
  
  final_plot <- ggraph(disim_net, layout="fr") +
      geom_edge_link(aes(width = weight, 
                         alpha=weight), 
                     edge_colour = "black", 
                     show.legend= F,
                    ) +
      scale_edge_width(range=c(0.5,1.5)) +
      scale_edge_colour_gradient(low = "#CCCCCC",
                                 high = "#000000") +
      geom_node_point(alpha=1, size=5) +
      geom_node_text(aes(label=row.names(artifact_data)), 
                     size=3, 
                     family= "Arial", 
                     repel=T, max.overlaps = 100) +
      theme(text= element_text(size = 15), 
            panel.background = element_rect(fill = 'white')) +
    border(color = "black", size = 0.5)
    
  }
  
  
  if(method == "pcoa"){
  pcoa <- pcoa(jac)
  pcoa_df <- data.frame(pcoa1 = pcoa$vectors[,1], 
                      pcoa2 = pcoa$vectors[,2])
  pcoa_df <- pcoa_df %>% 
  mutate(site_name = rownames(artifact_data))
  
  final_plot <- ggplot(pcoa_df) +
    aes(x=pcoa1, 
        y=pcoa2) +
    geom_point() +
    labs(x = "PC1",
         y = "PC2",) +
    theme(title = element_text(size = 5)) +
    geom_text_repel(aes(label = site_name), 
                    size = 2,
                    max.overlaps = 20)}
  
  return(final_plot)
}





strength_function <- function(artifact_data, group_data, type = "table"){
  artifact_data <- artifact_data %>% 
    select_if(~ !is.numeric(.) || sum(.) != 0)
  
  set.seed(500)
  jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
  dm <- as.matrix(jac)
  
  if(type == "table"){
  modul_matrix <-
    graph_from_adjacency_matrix(1 - dm, mode = "undirected")
  mem <- as_membership(parse_number(group_data$group))
  as_group <- modularity(modul_matrix, membership = mem)
  
  perman <- adonis2(jac~as.factor(group_data$group), 
                    method = "jaccard",
                    sqrt.dist = TRUE)
  perman <- perman %>% 
    data_frame() %>% 
    slice_head()
  perman_r <- perman$R2
  perman_p <- perman$`Pr(>F)`
  
  site_distances <- dist(cbind(group_data$longitude, group_data$latitude))
  mantel_distance <- vegan::mantel(jac, site_distances, permutations = 1000)
  p_value_distance_correlation <- round(mantel_distance$signif, 3)
  r_value_distance_correlation <- round(mantel_distance$statistic, 3)
  
  final_table <- data_frame("modularity" = as_group, 
                            "permanova_r" = perman_r,
                            "permanova_p" = perman_p,
                            "distance_mantel_r" = r_value_distance_correlation, 
                            "distance_mantel_p" = p_value_distance_correlation)

  return(final_table)}
  
  if(type == "dispersion"){
    set.seed(500)
    dispersion <- betadisper(jac, group = group_data$group)
    disp_plot<- plot(dispersion, hull = FALSE, ellipse=TRUE)
    return(disp_plot)}
}




trans_network <- function(artifact_data){
  artifact_data <- artifact_data %>% 
    select_if(~ !is.numeric(.) || sum(.) != 0)
    jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
    dm <- as.matrix(jac)
    disim <- 1 - dm
    disim[disim<0.1] <- 0
    disim_net <- network(disim,
                         directed=F,
                         ignore.eval=F,
                         names.eval='weight')
    disim_net %v% 'vertex.names' <- row.names(artifact_data)
    set.seed(500)
    final_plot <- ggraph(disim_net, layout="fr") +
      geom_edge_link(aes(width = weight, 
                         alpha=weight), 
                       edge_colour = "black", 
                       show.legend=F) +
      scale_edge_width(range=c(0.5,1.5)) +
      scale_edge_colour_gradient(low = "#CCCCCC",
                                   high = "#000000") +
      geom_node_point(alpha=1, size=5) +
      geom_node_text(aes(label=row.names(artifact_data)), 
                     size=3, 
                     family= "Arial", 
                     repel=T, max.overlaps = 100) +
      theme(text= element_text(size = 15), 
            panel.background = element_rect(fill = 'white')) +
      border(color = "black", size = 0.5)
    return(final_plot)
    }




