# functions.R
# Single source of truth for all functions used by paper.qmd and the supplements.
# Sourced from both documents; do not duplicate function definitions elsewhere.

# ── Package loading ──────────────────────────────────────────────────────────
# All packages used by any function below. Load order matters where names
# collide (e.g. igraph/statnet/sna); the network functions are fully
# igraph::-namespaced so load order is irrelevant for them.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(stringr)
  library(purrr)
  library(magrittr)
  library(vegan)
  library(igraph)
  library(statnet)
  library(sna)
  library(ggraph)
  library(cowplot)
  library(ggpubr)
  library(sf)
  library(tabula)
  library(ggplot2)
  library(ggrepel)
  library(knitr)
  library(readxl)
})

# ── Data preparation ──────────────────────────────────────────────────────────
# Build per-phase site-level data from the object-level table.
# `signbase_full_clean` and `lat_long_df` are expected to exist in the calling
# environment (created in the document setup chunks).
make_phase_data <- function(phase_name, signbase_full_clean, lat_long_df) {
  df <- signbase_full_clean %>%
    filter(time_period == phase_name) %>%
    mutate(longitude = as.character(longitude),
           latitude  = as.character(latitude))
  obj_num <- df %>% group_by(site_name) %>% summarise(nobjects = n())
  df %>%
    group_by(site_name) %>%
    summarize(across(where(is.numeric), sum)) %>%
    left_join(lat_long_df) %>%
    left_join(obj_num) %>%
    mutate(time_period = phase_name)
}

# Extract the sign-type presence columns from a site-level data frame, keeping
# only sign types that occur at least once.
extract_artifact <- function(df) {
  df %>%
    column_to_rownames("site_name") %>%
    dplyr::select(line:star) %>%
    dplyr::select(where(~ is.numeric(.) && sum(.) != 0))
}

# ── Automated group detection ─────────────────────────────────────────────────
# Louvain community detection on a site × sign binary matrix.
# Returns a named integer vector (community ID per site).
get_louvain_groups <- function(artifact_data, threshold = 0.2, metric = "jaccard") {
  sign_names <- c("line","dashline","obline","radline","circumline","notch","obnotch",
                  "radnotch","circumnotch","dot","cupule","cross","rhombus","grid",
                  "hatching","zigzag","zigzagrow","rectangle","hashtag","maccaroni",
                  "v","circumspiral","vulva","anthropomorph","zoomorph","paw",
                  "concenline","pinleft","pinright","star")
  present <- intersect(sign_names, colnames(artifact_data))
  mat <- as.data.frame(artifact_data[, present, drop = FALSE])
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  mat <- as.data.frame(lapply(mat, function(x) as.numeric(x > 0)))
  rownames(mat) <- rownames(artifact_data)
  if (metric == "jaccard") {
    jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  } else if (metric == "sorensen") {
    jac <- as.matrix(vegan::vegdist(mat, "bray", binary = TRUE))
  } else if (metric == "simpson") {
    jac <- as.matrix(vegan::designdist(mat, "pmin(A-J, B-J) / pmin(A, B)",
                                       terms = "binary"))
  } else {
    jac <- as.matrix(vegan::vegdist(mat, method = metric, binary = TRUE))
  }
  adj <- 1 - jac
  adj[adj < threshold] <- 0
  diag(adj) <- 0
  set.seed(42)
  ig <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected",
                                            weighted = TRUE, diag = FALSE)
  comm <- igraph::cluster_louvain(ig)
  mem <- as.integer(igraph::membership(comm))
  names(mem) <- igraph::V(ig)$name
  mem
}

# Format large numbers with thousands separators for inline text.
pretty_print_dates <- function(x) {
  formatC(x,
          decimal.mark = ".",
          big.mark = ",",
          digits = 0,
          format = "f")
}

# ── Seriation ────────────────────────────────────────────────────────────────
# Tile-plot a presence/absence matrix; rows are sites, columns are sign types.
mplot <- function(x, ..., fill_colour = "black", title = NULL,
                  row_labels = NULL, col_labels = NULL, palette_colors = NULL) {
  d <- data.frame(
    x = rep(1:ncol(x), each = nrow(x)),
    y = rep(nrow(x):1, ncol(x)),
    z = as.integer(x)
  )

  if (!is.null(palette_colors)) {
    d$palette_colors <- rep(palette_colors, ncol(x))
  }

  if (is.null(col_labels)) col_labels <- colnames(x)
  if (is.null(row_labels)) row_labels <- rev(rownames(x))

  p <- ggplot(d, aes(x, y)) +
    geom_tile(col = "grey50",
              linewidth = 0.5,
              aes(fill = ifelse(z == 1, palette_colors, "white"))) +
    scale_fill_identity() +
    coord_equal(expand = FALSE) +
    scale_x_continuous(
      breaks = 1:ncol(x),
      labels = col_labels,
      position = "top",
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = 1:nrow(x),
      labels = row_labels,
      expand = c(0, 0)
    ) +
    theme(
      axis.text = element_text(size = rel(1)),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 1,
                                margin = margin(b = 2)),
      plot.margin = margin(t = 0, r = 4, b = 0, l = 4)
    ) +
    labs(x = "", y = "") +
    rotate_x_text(angle = 90, hjust = 0, vjust = 0.5)

  p
}

# One pass of the Brower-Kile style ordering: sort rows by mean column index of
# presences, then columns by mean row index of presences.
increase_focus <- function(x, group = NULL) {
  mcp <- apply(
    x,
    MARGIN = 1,
    FUN = \(z) { mean(which(z == 1)) },
    simplify = FALSE
  ) |> unlist()
  row_order <- order(mcp, na.last = TRUE)
  x <- x[row_order, ]

  if (!is.null(group)) {
    group <- group[row_order]
  }

  mrp <- apply(
    x,
    MARGIN = 2,
    FUN = \(z) { mean(which(z == 1)) },
    simplify = FALSE
  ) |> unlist()
  x <- x[, order(mrp, na.last = TRUE)]

  list(matrix = x, group = group)
}

# Iterate increase_focus until the matrix stops changing.
concentrate <- function(x, group = NULL, max_iter = 100) {
  old <- x
  not_identical <- TRUE
  iter <- 0

  while (not_identical && iter < max_iter) {
    result <- increase_focus(old, group)
    new <- result$matrix
    group <- result$group
    not_identical <- !identical(old, new)
    old <- new
    iter <- iter + 1
  }

  if (iter == max_iter) {
    warning("Reached maximum iterations without convergence.")
  }

  list(matrix = new, group = group)
}

# Produce a seriation or network plot for one phase.
produce_clusters <- function(artifact_data, artifact_data_unique,
                             method = "seriation",
                             color_palette = c("#E41A1C", "#377EB8"),
                             threshold = 0.2, metric = "jaccard") {

  artifact_data <- artifact_data %>%
    dplyr::select(where(~ is.numeric(.) && sum(.) != 0)) %>%
    mutate(across(everything(), ~ as.numeric(. > 0)))

  if (metric == "jaccard") {
    jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
  } else if (metric %in% c("sorensen", "bray")) {
    jac <- vegdist(artifact_data, "bray", binary = TRUE)
  } else if (metric == "simpson") {
    jac <- vegan::designdist(artifact_data, "pmin(A-J, B-J) / pmin(A, B)", terms = "binary")
  } else {
    jac <- vegdist(artifact_data, method = metric, binary = TRUE)
  }
  dm <- as.matrix(jac)
  group_sizes <- NULL

  if (method == "seriation") {
    artifact_matrix <- as.matrix(artifact_data)
    rownames(artifact_matrix) <- rownames(artifact_data)

    group <- artifact_data_unique$group
    unique_group <- sort(unique(group))
    row_colors <- setNames(color_palette, unique_group)
    result <- concentrate(artifact_matrix, group = group)
    concentrated_matrix <- result$matrix
    reordered_group <- result$group
    group_colors <- row_colors[reordered_group]

    # Calculate group sizes for reporting
    group_sizes <- table(reordered_group)

    final_plot <- mplot(concentrated_matrix,
                        palette_colors = group_colors)
  }

  if (method == "network") {
    disim <- 1 - dm
    disim[disim < threshold] <- 0
    disim_net <- network::network(disim,
                                  directed = FALSE,
                                  ignore.eval = FALSE,
                                  names.eval = "weight")
    disim_net %v% "vertex.names" <- row.names(artifact_data)
    set.seed(500)

    final_plot <- ggraph(disim_net, layout = "fr") +
      geom_edge_link(aes(width = weight,
                         alpha = weight),
                     edge_colour = "black",
                     show.legend = FALSE) +
      scale_edge_width(range = c(0.5, 1.5)) +
      scale_edge_colour_gradient(low = "#CCCCCC",
                                 high = "#000000") +
      geom_node_point(alpha = 1, size = 5) +
      geom_node_text(aes(label = row.names(artifact_data)),
                     size = 3,
                     family = "Arial",
                     repel = TRUE, max.overlaps = 100,
                     point.padding = 0.6,
                     box.padding = 0.5,
                     force = 2,
                     bg.r = 0.25,
                     bg.colour = "white") +
      theme(text = element_text(size = 15),
            panel.background = element_rect(fill = "white")) +
      ggpubr::border(color = "black", size = 0.5)
  }
  return(list(plot = final_plot, group_sizes = group_sizes))
}

# ── Network statistics & tests ────────────────────────────────────────────────
# Parameterised network-stats function (threshold & metric exposed).
# NOTE: igraph:: is used explicitly throughout to avoid sna/statnet masking.
network_stats <- function(artifact_data, threshold = 0.2, metric = "jaccard") {
  # Select only sign-type columns that have at least one nonzero value
  sign_names <- c("line","dashline","obline","radline","circumline","notch","obnotch",
                  "radnotch","circumnotch","dot","cupule","cross","rhombus","grid",
                  "hatching","zigzag","zigzagrow","rectangle","hashtag","maccaroni",
                  "v","circumspiral","vulva","anthropomorph","zoomorph","paw",
                  "concenline","pinleft","pinright","star")
  present <- intersect(sign_names, colnames(artifact_data))
  mat <- as.data.frame(artifact_data[, present, drop = FALSE])
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  mat <- as.data.frame(lapply(mat, function(x) as.numeric(x > 0)))
  if (metric == "jaccard") {
    jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  } else if (metric %in% c("sorensen", "bray")) {
    jac <- as.matrix(vegan::vegdist(mat, "bray", binary = TRUE))
  } else if (metric == "simpson") {
    jac <- as.matrix(vegan::designdist(mat, "pmin(A-J, B-J) / pmin(A, B)", terms = "binary"))
  } else {
    jac <- as.matrix(vegan::vegdist(mat, method = metric, binary = TRUE))
  }
  adj  <- 1 - jac
  adj[adj < threshold] <- 0
  diag(adj) <- 0
  ig   <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected",
                                              weighted = TRUE, diag = FALSE)
  comp      <- igraph::components(ig)
  mean_dist <- tryCatch(igraph::mean_distance(ig), error = function(e) NA)
set.seed(42)
  comm      <- igraph::cluster_louvain(ig)
  data.frame(
    n_sites       = igraph::vcount(ig),
    n_edges       = igraph::ecount(ig),
    density       = round(igraph::edge_density(ig), 3),
    n_components  = comp$no,
    n_isolates    = sum(comp$csize == 1),
    mean_degree   = round(mean(igraph::degree(ig)), 2),
    mean_strength = round(mean(igraph::strength(ig)), 2),
    mean_between  = round(mean(igraph::betweenness(ig, weights = NA)), 2),
    transitivity  = round(igraph::transitivity(ig), 3),
    mean_path     = round(mean_dist, 2),
    n_communities = length(unique(igraph::membership(comm))),
    modularity    = round(igraph::modularity(comm), 3),
    row.names = NULL
  )
}

# Compute per-site network centrality metrics for a binary artifact matrix
# Returns a data.frame with site_name, degree, strength, betweenness, eigenvector
node_centrality <- function(artifact_data, threshold = 0.2) {
  sign_names <- c("line","dashline","obline","radline","circumline","notch","obnotch",
                  "radnotch","circumnotch","dot","cupule","cross","rhombus","grid",
                  "hatching","zigzag","zigzagrow","rectangle","hashtag","maccaroni",
                  "v","circumspiral","vulva","anthropomorph","zoomorph","paw",
                  "concenline","pinleft","pinright","star")
  present <- intersect(sign_names, colnames(artifact_data))
  mat <- as.data.frame(artifact_data[, present, drop = FALSE])
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  mat <- as.data.frame(lapply(mat, function(x) as.numeric(x > 0)))
  jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  adj  <- 1 - jac
  adj[adj < threshold] <- 0
  diag(adj) <- 0
  ig   <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected",
                                              weighted = TRUE, diag = FALSE)
  eig <- as.numeric(igraph::eigen_centrality(ig)$vector)
  eig <- replace(eig, is.nan(eig), 0)
  data.frame(
    site_name = igraph::V(ig)$name,
    degree    = igraph::degree(ig),
    strength  = igraph::strength(ig),
    betweenness = igraph::betweenness(ig, weights = NA),
    eigenvector = eig,
    stringsAsFactors = FALSE
  )
}

# Compute per-site network centrality from a raw site-level dataframe
# (with site_name and sign columns line:star)
centrality_from_signs <- function(df, threshold = 0.2) {
  signs <- df %>%
    dplyr::select(site_name, line:star) %>%
    group_by(site_name) %>%
    summarise(across(where(is.numeric), ~ sum(.))) %>%
    column_to_rownames("site_name") %>%
    mutate(across(everything(), ~ as.numeric(. > 0)))
  jac <- as.matrix(vegan::vegdist(signs, "jaccard", binary = TRUE))
  adj  <- 1 - jac
  adj[adj < threshold] <- 0
  diag(adj) <- 0
  ig   <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected",
                                              weighted = TRUE, diag = FALSE)
  eig <- as.numeric(igraph::eigen_centrality(ig)$vector)
  eig <- replace(eig, is.nan(eig), 0)
  data.frame(
    site_name = igraph::V(ig)$name,
    degree    = igraph::degree(ig),
    strength  = igraph::strength(ig),
    betweenness = igraph::betweenness(ig, weights = NA),
    eigenvector = eig,
    stringsAsFactors = FALSE
  )
}

# PerMANOVA of sign distribution against group membership, plus a Mantel test
# of sign similarity vs. geographic distance, for one phase.
strength_function <- function(artifact_data, group_data) {

  set.seed(500)
  jac <- vegan::vegdist(artifact_data %>% mutate(across(everything(), ~ as.numeric(. > 0))),
                        "jaccard", binary = TRUE)
  perman <- vegan::adonis2(jac ~ as.factor(group_data$group),
                           method = "jaccard",
                           sqrt.dist = TRUE)
  perman <- perman %>%
    tibble::as_tibble() %>%
    slice_head()
  perman_r <- round(perman$R2, 3)
  perman_p <- round(perman$`Pr(>F)`, 3)
  perman_f <- round(perman$F, 3)

  site_sf <- sf::st_as_sf(group_data, coords = c("longitude", "latitude"), crs = 4326)
  site_distances <- as.dist(sf::st_distance(site_sf) / 1000)
  mantel_distance <- vegan::mantel(site_distances, jac, permutations = 1000)
  p_value_distance_correlation <- round(mantel_distance$signif, 3)
  r_value_distance_correlation <- round(mantel_distance$statistic, 3)

  final_table <- data.frame("PerMANOVA R2" = perman_r,
                            "PerMANOVA F" = perman_f,
                            "PerMANOVA p" = perman_p,
                            "Mantel R" = r_value_distance_correlation,
                            "Mantel p" = p_value_distance_correlation,
                            check.names = FALSE)

  return(final_table)
}

# ── Permutation tests for network statistics ─────────────────────────────────
# Build a single network statistic from a padded site x sign matrix.
build_stat <- function(stat) {
  force(stat)
  function(mat) {
    jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
    adj <- 1 - jac; adj[adj < 0.2] <- 0; diag(adj) <- 0  # main-analysis threshold
    ig <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    switch(stat,
      density     = if (igraph::ecount(ig) > 0) igraph::edge_density(ig) else 0,
      modularity  = tryCatch({
        igraph::modularity(igraph::cluster_louvain(ig))
      }, error = function(e) NA),
      betweenness = if (igraph::ecount(ig) > 0) mean(igraph::betweenness(ig, weights = NA)) else 0,
      components  = if (igraph::ecount(ig) > 0) igraph::components(ig)$no else nrow(mat))
  }
}

# Pairwise permutation test on the difference of a statistic between two phases.
pair_diff <- function(matA, matB, statfun, nperm = 5000) {
  d_obs <- statfun(matA) - statfun(matB)
  pooled <- rbind(matA, matB)
  nA <- nrow(matA)
  null <- replicate(nperm, {
    idx <- sample(nrow(pooled), nA)                 # phase sizes preserved
    statfun(pooled[idx, , drop = FALSE]) - statfun(pooled[-idx, , drop = FALSE])
  })
  null <- null[!is.na(null)]
  p <- mean(abs(null) >= abs(d_obs))                # two-sided
  c(diff = d_obs, null_mean = mean(null),
    ci_lo = unname(quantile(null, 0.025)), ci_hi = unname(quantile(null, 0.975)),
    p = p)
}

# Compute a matrix of 4 network statistics for each of several groups of sites,
# used for the shared-null permutation test.
stat_phase <- function(site_vectors, group_sizes) {
  splits <- cumsum(group_sizes)
  idx <- split(seq_len(nrow(site_vectors)),
               cut(seq_len(nrow(site_vectors)), c(0, splits)))
  t(sapply(idx, function(ii) {
    mat <- site_vectors[ii, , drop = FALSE]
    jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
    adj <- 1 - jac; adj[adj < 0.2] <- 0; diag(adj) <- 0  # main-analysis threshold
    ig <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE)
    dens  <- if (igraph::ecount(ig) > 0) igraph::edge_density(ig) else 0
    mod   <- tryCatch(igraph::modularity(igraph::cluster_louvain(ig)), error = function(e) NA)
    bet   <- if (igraph::ecount(ig) > 0) mean(igraph::betweenness(ig, weights = NA)) else 0
    ncomp <- if (igraph::ecount(ig) > 0) igraph::components(ig)$no else nrow(mat)
    c(density = dens, modularity = mod, betweenness = bet, components = ncomp)
  }))
}

# Pad a phase matrix to the full set of sign-type columns (0-fill missing).
pad <- function(m, all_cols) {
  miss <- setdiff(all_cols, colnames(m))
  for (c in miss) m[[c]] <- 0
  m[, all_cols]   # pad matrix to the same column layout
}

# ── Network plotting ──────────────────────────────────────────────────────────
# Parameterised network-plot function (threshold & metric exposed).
network_plot <- function(artifact_data, threshold = 0.2, metric = "jaccard", title = NULL) {
  mat <- artifact_data %>%
    dplyr::select(where(~ is.numeric(.) && sum(.) != 0)) %>%
    mutate(across(everything(), ~ as.numeric(. > 0)))
  if (metric == "jaccard") {
    jac <- vegan::vegdist(mat, "jaccard", binary = TRUE)
  } else if (metric %in% c("sorensen", "bray")) {
    jac <- vegan::vegdist(mat, "bray", binary = TRUE)
  } else if (metric == "simpson") {
    jac <- vegan::designdist(mat, "pmin(A-J, B-J) / pmin(A, B)", terms = "binary")
  } else {
    jac <- vegan::vegdist(mat, method = metric, binary = TRUE)
  }
  dm    <- as.matrix(jac)
  disim <- 1 - dm
  disim[disim < threshold] <- 0
  net <- network::network(disim, directed = FALSE,
                          ignore.eval = FALSE, names.eval = "weight")
  net %v% "vertex.names" <- row.names(mat)
  set.seed(500)
  ggraph(net, layout = "fr") +
    geom_edge_link(aes(width = weight, alpha = weight),
                   edge_colour = "black", show.legend = FALSE) +
    scale_edge_width(range = c(0.5, 1.5)) +
    geom_node_point(alpha = 1, size = 4) +
    geom_node_text(aes(label = row.names(mat)),
                   size = 2.5, repel = TRUE,
                   max.overlaps = 100,
                   point.padding = 0.5,
                   box.padding   = 0.4,
                   force = 2,
                   bg.r = 0.2, bg.colour = "white") +
    labs(title = title) +
    theme(text = element_text(size = 12),
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(size = 9, hjust = 0.5)) +
    ggpubr::border(color = "black", size = 0.5)
}

# ── Diversity plotting helpers (tabula) ───────────────────────────────────────
# Recast a vector to a factor preserving level order (optionally reversed).
as_factor <- function(x, reverse = FALSE) {
  lvl <- unique(x)
  if (reverse) {
    lvl <- rev(lvl)
  }
  factor(x, levels = lvl)
}

# Widen a matrix into a long data.frame of row/column/value triples.
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

# ggplot2 autoplot method for tabula diversity-index objects.
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

  y_lab <- switch(
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
    ggplot2::scale_y_continuous(name = "Diversity")
}

# Plot expected-vs-observed Shannon diversity per site for one phase.
plot_diversity_fn <- function(input_df) {

  signs <-
    input_df %>%
    dplyr::select(site_name,
                  c(line:star)) %>%
    group_by(site_name) %>%
    summarise(across(where(is.numeric), ~ sum(.))) %>%
    column_to_rownames("site_name")

  diversity_index <-
    tabula::heterogeneity(signs, method = "shannon")

  set.seed(42)
  diversity_sim <- tabula::simulate(diversity_index,
                                    level = 0.95)

  autoplot.DiversityIndex(diversity_sim) +
    ggrepel::geom_text_repel(aes(label = diversity_sim@labels),
                             size = 3,
                             max.overlaps = 15,
                             bg.color = "white",
                             bg.r = 0.1) +
    guides(colour = "none") +
    theme_minimal(base_size = 8) +
    ggpubr::border(color = "black", size = 0.5)
}

# Compute per-site Shannon diversity excess (observed - expected at sample size)
# Returns a data.frame with site_name, nobjects, H_obs, H_exp, H_excess
diversity_excess <- function(df) {
  signs <- df %>%
    dplyr::select(site_name, line:star) %>%
    group_by(site_name) %>%
    summarise(across(where(is.numeric), ~ sum(.))) %>%
    column_to_rownames("site_name")
  n <- rowSums(signs)
  H_obs <- apply(signs, 1, function(r) { p <- r[r > 0] / sum(r); -sum(p * log(p)) })
  di <- tabula::heterogeneity(signs, method = "shannon")
  set.seed(42)
  simdf <- as.data.frame(tabula::simulate(di, nsim = 1000, level = 0.95)@simulation)
  exp_fun <- approxfun(simdf$size, simdf$mean)
  data.frame(
    site_name = rownames(signs),
    nobjects  = n,
    H_obs     = H_obs,
    H_exp     = exp_fun(n),
    H_excess  = H_obs - exp_fun(n),
    stringsAsFactors = FALSE
  )
}

# Phase-level summary of diversity excess
# Returns a data.frame with sign_pool, mean_rel_div, n_above_mean
summarise_diversity <- function(df) {
  de <- diversity_excess(df)
  signs <- df %>%
    dplyr::select(site_name, line:star) %>%
    group_by(site_name) %>%
    summarise(across(where(is.numeric), ~ sum(.))) %>%
    column_to_rownames("site_name")
  data.frame(
    sign_pool = sum(colSums(signs) > 0),
    mean_rel_div = mean(de$H_excess),
    n_above_mean = sum(de$H_excess > 0),
    stringsAsFactors = FALSE
  )
}

# ── Mantel comparison (Euclidean vs. geodesic distance) ──────────────────────
run_mantel_comp <- function(df) {
  art <- df %>%
    column_to_rownames("site_name") %>%
    dplyr::select(line:star) %>%
    dplyr::select(where(~ is.numeric(.) && sum(.) != 0)) %>%
    mutate(across(everything(), ~ as.numeric(. > 0)))
  jac <- vegan::vegdist(art, "jaccard", binary = TRUE)

  # Flat coordinate Euclidean distance
  d_euc <- dist(cbind(df$longitude, df$latitude))
  set.seed(42)
  m_euc <- vegan::mantel(d_euc, jac, permutations = 1000)

  # True Geodesic distance in km
  sf_obj <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  d_geo  <- as.dist(sf::st_distance(sf_obj) / 1000)
  set.seed(42)
  m_geo  <- vegan::mantel(d_geo, jac, permutations = 1000)

  data.frame(
    Euclidean_R = round(m_euc$statistic, 3),
    Euclidean_p = round(m_euc$signif, 3),
    Geodesic_R  = round(m_geo$statistic, 3),
    Geodesic_p  = round(m_geo$signif, 3)
  )
}

# ── betadisper ────────────────────────────────────────────────────────────────
# Test for homogeneity of multivariate dispersions (betadisper) across groups.
# Returns group medians, distances to median, and ANOVA test for dispersion
# differences. Used to validate perMANOVA results.
#
# Args:
#   phase_df: Data frame for a single phase with columns site_name, longitude,
#             latitude, plus sign columns (line, notch, etc.)
#   groups:   Numeric vector of group labels (1 = restricted, 2 = broad)
#
# Returns: List with betadisper object, ANOVA results, and summary table
run_betadisper <- function(phase_df, groups) {
  art <- phase_df %>%
    column_to_rownames("site_name") %>%
    dplyr::select(line:star) %>%
    dplyr::select(where(~ is.numeric(.) && sum(.) != 0)) %>%
    mutate(across(everything(), ~ as.numeric(. > 0)))
  jac <- vegan::vegdist(art, "jaccard", binary = TRUE)

  bd <- vegan::betadisper(jac, as.factor(groups))
  bd_anova <- anova(bd)

  list(
    betadisper = bd,
    anova = bd_anova,
    medians = bd$medians,
    p_value = round(bd_anova$`Pr(>F)`[1], 3)
  )
}

# ── S3 registration ───────────────────────────────────────────────────────────
# Ensure ggplot2::autoplot() dispatches to autoplot.DiversityIndex even when the
# function is defined in a sourced script rather than the global environment.
registerS3method("autoplot", "DiversityIndex", autoplot.DiversityIndex)

# ── Paper-specific helpers ──────────────────────────────────────────────────────
# Build per-phase (Aur-P1/Aur-P2) site-level data from the object-level table.
# Uses the `phase2` column created during data cleaning.
# `signbase_full_clean` and `lat_long_df` are expected to exist in the calling
# environment (created in the document setup chunks).
make_phase2_data <- function(phase2_val, signbase_full_clean, lat_long_df) {
  df <- signbase_full_clean %>%
    filter(phase2 == phase2_val) %>%
    mutate(longitude = as.character(longitude),
           latitude  = as.character(latitude))
  obj_num <- df %>%
    group_by(site_name) %>%
    summarise(nobjects = n())
  df %>%
    group_by(site_name) %>%
    summarize(across(where(is.numeric), sum)) %>%
    left_join(lat_long_df) %>%
    left_join(obj_num) %>%
    mutate(time_period = phase2_val)
}

# ── Canonical manual group assignment (restricted = 1, broad = 2) ────────────────
# Single source of truth for paper.qmd, S1 (S5/S6), and S2.
manual_groups <- list(
  "Aur-P1" = c("Abri Pataud" = 1, "Fumane" = 1, "Pod Hradem" = 1,
               "Riparo Bombrini" = 1, "Grottes de Fonds-de-Forêt" = 1,
               "Labeko Koba" = 1, "Hohlenstein-Stadel" = 1, "Gatzarria" = 1,
               "Grotte de la Verpillière I" = 1,
               "Geissenklösterle" = 2, "Hohle Fels" = 2, "Vogelherd" = 2,
               "Cellier" = 2, "Blanchard" = 2, "La Ferrassie" = 2,
               "Castanet" = 2, "Solutré" = 2, "Grotte du Renne" = 2,
               "Les Cottés" = 2, "Trou al'Wesse" = 2),
  "Aur-P2" = c("Les Rois" = 1, "Sirgenstein Cave" = 1, "La Viña" = 1,
               "Mladeč" = 1, "Gargas" = 1, "Vindija Cave" = 1,
               "Trou Magrite" = 2, "Bockstein-Törle" = 2, "Hohle Fels" = 2))

# Attach the manual restricted(1)/broad(2) group to a phase data frame by site_name.
add_manual_group <- function(df, phase) {
  g <- manual_groups[[phase]]
  df %>% mutate(group = as.character(g[df$site_name]))
}

# ── Bootstrap consensus co-clustering (S1 S6.2) ──────────────────────────────────
# Bootstrap consensus co-clustering into k blocks, from a site x sign matrix.
s6_boot_consensus <- function(mat, n_boot = 1000, k = 2) {
  x <- as.matrix(mat); x[x > 0] <- 1
  sites <- rownames(x); n <- nrow(x)
  co <- matrix(0, n, n, dimnames = list(sites, sites))
  cnt <- matrix(0, n, n, dimnames = list(sites, sites))
  assign <- matrix(0L, n, n_boot, dimnames = list(sites, NULL))
  for (bk in seq_len(n_boot)) {
    set.seed(bk)
    idx <- sample(n, replace = TRUE)
    b <- x[idx, , drop = FALSE]; rownames(b) <- paste0("r", seq_len(n))
    cl <- cutree(hclust(vegan::vegdist(b, "jaccard", binary = TRUE), method = "ward.D2"), k = k)
    for (r in seq_len(n)) assign[idx[r], bk] <- cl[r]
  }
  present <- assign > 0
  for (i in seq_len(n)) for (j in seq_len(n)) if (i < j) {
    v <- present[i, ] & present[j, ]
    cnt[i, j] <- cnt[j, i] <- sum(v)
    if (cnt[i, j] > 0) co[i, j] <- co[j, i] <- sum(assign[i, v] == assign[j, v]) / cnt[i, j]
  }
  # Reference partition obtained by clustering the label-independent consensus
  # matrix; each resample's cluster labels are realigned to this reference to
  # undo the arbitrary label permutation introduced by hierarchical clustering.
  ref <- cutree(hclust(as.dist(1 - co), method = "ward.D2"), k = k)
  names(ref) <- sites
  for (bk in seq_len(n_boot)) {
    idx <- which(present[, bk]); a <- assign[idx, bk]
    agree_id <- sum((a == 1) == (ref[idx] == 1))
    agree_sw <- sum((a == 1) == (ref[idx] == 2))
    if (agree_sw > agree_id) a <- ifelse(a == 1, 2, 1)
    assign[idx, bk] <- a
  }
  modal <- apply(assign, 1, function(v) { t <- table(v[v > 0]); as.integer(names(t)[which.max(t)]) })
  consistency <- vapply(seq_len(n), function(i) {
    v <- assign[i, ]; v <- v[v > 0]
    if (!length(v)) NA_real_ else mean(v == modal[i])
  }, numeric(1))
  names(consistency) <- sites
  list(consensus = co, partition = ref, modal = modal, consistency = consistency)
}

# Compute bootstrap consensus co-clustering summary for both phases.
# Returns a data frame with Within_restricted, Within_broad, Overall_within, Across per phase.
s6_bootstrap_summary <- function(art_list, groups_list, n_boot = 1000) {
  boot <- lapply(art_list, s6_boot_consensus, n_boot = n_boot)
  tri  <- function(m) m[upper.tri(m)]
  map_dfr(names(boot), function(ph) {
    cs  <- boot[[ph]]$consensus
    sit <- rownames(cs); g <- groups_list[[ph]][sit]
    wr  <- mean(tri(cs[sit[g == 1], sit[g == 1]]), na.rm = TRUE)
    wb  <- mean(tri(cs[sit[g == 2], sit[g == 2]]), na.rm = TRUE)
    xr  <- mean(cs[sit[g == 1], sit[g == 2]], na.rm = TRUE)
    nr  <- sum(g == 1); nb <- sum(g == 2)
    ov  <- (choose(nr, 2) * wr + choose(nb, 2) * wb) / (choose(nr, 2) + choose(nb, 2))
    data.frame(Phase = ph, Restricted_n = nr, Broad_n = nb,
               Within_restricted = round(wr, 2), Within_broad = round(wb, 2),
               Overall_within = round(ov, 2), Across = round(xr, 2),
               stringsAsFactors = FALSE)
  })
}

# Mantel test for sign similarity vs geographic distance for a subset of sites.
# `sites`: character vector of site names (row names in Smat and geom_full).
# `Smat`: full site × sign matrix (binary or count).
# `geom_full`: sf object with geometry column matching Smat row order.
# Returns Mantel R statistic.
mantel_R_phase <- function(sites, Smat, geom_full) {
  i <- match(sites, rownames(Smat))
  jac <- vegan::vegdist(Smat[i, , drop = FALSE], "jaccard", binary = TRUE)
  gd  <- as.dist(sf::st_distance(geom_full[i, ]) / 1000)
  vegan::mantel(gd, jac, permutations = 0)$statistic
}

# Format a number as scientific-notation markdown with a superscript exponent
# (e.g. 10000 -> "10^4^" -> 10⁴), bypassing knitr's numeric inline hook that
# otherwise leaks a literal "10^{4}" into the docx.
sci_md <- function(x, digits = 2) {
  x <- as.numeric(x)
  if (is.na(x)) return(NA_character_)
  if (x == 0) return("0")
  s <- formatC(x, format = "e", digits = digits)
  s <- sub("e\\+", "e", s)
  parts <- strsplit(s, "e")[[1]]
  mantissa <- parts[1]
  exponent <- as.character(as.numeric(parts[2]))
  if (mantissa == paste0("1.", paste(rep("0", digits), collapse = ""))) {
    paste0("10^", exponent, "^")
  } else {
    paste0(mantissa, " \u00d7 10^", exponent, "^")
  }
}
