# functions.R
# Single source of truth for all functions used by paper.qmd and supplement.qmd.
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
    select_if(~ !is.numeric(.) || sum(.) != 0)
}

# ── Automated group detection ─────────────────────────────────────────────────
# Louvain community detection on a site × sign binary matrix.
# Returns a named integer vector (community ID per site).
get_louvain_groups <- function(artifact_data, threshold = 0.2, metric = "jaccard") {
  mat <- artifact_data %>%
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
      position = "top"
    ) +
    scale_y_continuous(
      breaks = 1:nrow(x),
      labels = row_labels
    ) +
    theme(
      axis.text = element_text(size = rel(1)),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 1,
                                margin = margin(b = 2)),
      plot.margin = margin(t = 0, r = 4, b = 2, l = 4)
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
                             color_palette = c("#8DD3C7", "#FFFFB3"),
                             threshold = 0.2, metric = "jaccard") {

  artifact_data <- artifact_data %>%
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))

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

  if (method == "seriation") {
    artifact_matrix <- as.matrix(artifact_data)
    rownames(artifact_matrix) <- rownames(artifact_data)

    group <- artifact_data_unique$group
    unique_group <- unique(group)
    row_colors <- setNames(color_palette, unique_group)
    result <- concentrate(artifact_matrix, group = group)
    concentrated_matrix <- result$matrix
    reordered_group <- result$group
    group_colors <- row_colors[reordered_group]

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
  return(final_plot)
}

# ── Network statistics & tests ────────────────────────────────────────────────
# Parameterised network-stats function (threshold & metric exposed).
# NOTE: igraph:: is used explicitly throughout to avoid sna/statnet masking.
network_stats <- function(artifact_data, threshold = 0.2, metric = "jaccard") {
  mat <- artifact_data %>%
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
    mean_between  = round(mean(igraph::betweenness(ig)), 2),
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
  mat <- artifact_data %>%
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
    betweenness = igraph::betweenness(ig),
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
    mutate_all(~ as.numeric(. > 0))
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
    betweenness = igraph::betweenness(ig),
    eigenvector = eig,
    stringsAsFactors = FALSE
  )
}

# PerMANOVA of sign distribution against group membership, plus a Mantel test
# of sign similarity vs. geographic distance, for one phase.
strength_function <- function(artifact_data, group_data) {

  set.seed(500)
  jac <- vegan::vegdist(artifact_data %>% mutate_all(~ as.numeric(. > 0)),
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
        set.seed(42)
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
    set.seed(42)
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
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
    select_if(~ !is.numeric(.) || sum(.) != 0) %>%
    mutate_all(~ as.numeric(. > 0))
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
