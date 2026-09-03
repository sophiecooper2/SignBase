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
  library(ape)
})

# Canonical 29 sign-type columns (matches get_louvain_groups / network_stats).
SIGN_COLS <- c("line","dashline","obline","radline","circumline","notch","obnotch",
               "radnotch","circumnotch","dot","cupule","cross","rhombus","grid",
               "hatching","zigzag","zigzagrow","rectangle","hashtag","maccaroni",
               "v","circumspiral","vulva","anthropomorph","zoomorph","paw",
               "concenline","pinleft","pinright","star")

# ── Canonical data-cleaning pipeline (shared by paper.qmd and S1) ───────────────
# `signbase_full` is the raw SignBase CSV tibble (read by the calling document).
# Returns the cleaned object-level table with calibrated `MedianBP` (variant
# selected by `date_variant`: "older", "midpoint", or "younger"), a
# technology-based three-phase `time_period` and two-phase `phase2` (matching the
# main-text assignment), a MedianBP-only `time_period_date` (used by S1 sensitivity
# analyses that revert technology overrides), and a positive `sign_total`.
# Non-radiocarbon ages (OSL) bypass calibration; radiocarbon ages (AMS, C14) are
# calibrated with IntCal20. Both ends of ranges are parsed; the chosen variant
# determines which calibrated median is used for phase assignment.
clean_signbase <- function(signbase_full, date_variant = c("midpoint", "older", "younger")) {
  date_variant <- match.arg(date_variant)
  
  signbase_full_clean <- signbase_full %>%
    filter(site_name != "Willendorf",
           site_name != "Riparo di Fontana Nuova",
           site_name != "Muralovka",
           site_name != "Shanidar Cave",
           site_name != "Hayonim Cave",
           site_name != "El Salitre",
           site_name != "Grotte De La Princesse Pauline",
           site_name != "Šandalja II") %>%
    dplyr::select(-other, -rectangle) %>%
    mutate(longitude = ifelse(site_name == "Riparo Bombrini", 7.437500, longitude)) %>%
    mutate(latitude  = ifelse(site_name == "Riparo Bombrini", 43.77083,  latitude))

  # Parse date strings: handle ranges, asymmetric errors, and different dating methods
  signbase_dates <- signbase_full_clean %>%
    drop_na(date_bp_max_min) %>%
    mutate(
      # Normalise separators
      date_str = str_replace_all(date_bp_max_min, "\\+\\/\\-", "±"),
      date_str = str_replace_all(date_str, "\\+", "±"),
      # Split range into older (max) and younger (min) ends
      parts = str_split(date_str, " - "),
      date_max = map_chr(parts, 1),
      date_min = map_chr(parts, ~ ifelse(length(.x) > 1, .x[2], .x[1])),
      # Extract age and error from each end (allow space after ±, e.g. "33000± 400")
      max_age = str_extract(date_max, "^[0-9.]+"),
      max_err = str_extract(date_max, "±\\s*([0-9.]+)"),
      max_err = str_remove(max_err, "±"),
      max_err = str_trim(max_err),
      min_age = str_extract(date_min, "^[0-9.]+"),
      min_err = str_extract(date_min, "±\\s*([0-9.]+)"),
      min_err = str_remove(min_err, "±"),
      min_err = str_trim(min_err)
    ) %>%
    mutate(
      max_age = parse_number(max_age),
      max_err = parse_number(max_err),
      min_age = parse_number(min_age),
      min_err = parse_number(min_err),
      # Determine dating method
      is_osl = dating_method == "OSL",
      is_c14 = dating_method %in% c("AMS", "C14", "C14; AMS")
    )

  # Helper to calibrate a single age/error pair (returns NA if not applicable)
  calibrate_one <- function(age, error, is_osl, is_c14) {
    if (is_osl) return(age)
    if (is_c14 && !is.na(age) && !is.na(error)) {
      cal <- rcarbon::calibrate(age, error, calCurves = "intcal20", verbose = FALSE)
      return(summary(cal)$MedianBP)
    }
    return(NA_real_)
  }

  # Calibrate radiocarbon dates; pass OSL dates through as calendar years
  # Process row by row since rcarbon::calibrate is not vectorized
  signbase_calibrated <- signbase_dates %>%
    rowwise() %>%
    mutate(
      MedianBP_older = calibrate_one(max_age, max_err, is_osl, is_c14),
      MedianBP_younger = calibrate_one(min_age, min_err, is_osl, is_c14),
      MedianBP_midpoint = (MedianBP_older + MedianBP_younger) / 2
    ) %>%
    ungroup() %>%
    dplyr::select(object_id, MedianBP_older, MedianBP_midpoint, MedianBP_younger)

  # Select the requested variant
  variant_col <- paste0("MedianBP_", date_variant)
  signbase_dated <- signbase_calibrated %>%
    dplyr::select(object_id, MedianBP = all_of(variant_col)) %>%
    drop_na(MedianBP)

  signbase_full_clean <- signbase_full_clean %>%
    inner_join(signbase_dated, by = "object_id")

  signbase_full_clean <- signbase_full_clean %>%
    mutate(time_period = case_when(
      site_name == "Spy"         ~ NA_character_,
      site_name == "El Castillo" ~ NA_character_,
      site_name == "Hohle Fels" & layer %in% c("Va", "Vaa", "Vab", "Vb") ~ "early_aurignacian",
      site_name == "Hohle Fels" & layer %in% c("IV", "IIIa", "IIIb", "Iid", "Iida", "Iie") ~ "evolved_aurignacian",
      site_name == "Hohle Fels" & layer == "IIIa-V" ~ NA_character_,
      site_name == "Geissenklösterle"  ~ "early_aurignacian",
      site_name == "Vogelherd"         ~ "early_aurignacian",
      site_name == "La Ferrassie"      ~ "early_aurignacian",
      site_name == "Abri Pataud"       ~ "early_aurignacian",
      site_name == "Mladeč"            ~ "evolved_aurignacian",
      site_name == "Gatzarria"         ~ "proto_aurignacian",
      (MedianBP > 39799) ~ "proto_aurignacian",
      (MedianBP > 37799) ~ "early_aurignacian",
      (MedianBP > 31999) ~ "evolved_aurignacian"
    )) %>%
    filter(!is.na(time_period)) %>%
    mutate(phase2 = ifelse(time_period %in% c("proto_aurignacian", "early_aurignacian"),
                           "Aur-P1", "Aur-P2")) %>%
    mutate(sign_total = rowSums(dplyr::select(., line:star))) %>%
    filter(sign_total > 0)

  signbase_full_clean <- signbase_full_clean %>%
    mutate(time_period_date = case_when(
      (MedianBP > 39799) ~ "proto_aurignacian",
      (MedianBP > 37799) ~ "early_aurignacian",
      (MedianBP > 31999) ~ "evolved_aurignacian"
    ))

  signbase_full_clean
}

# Distinct site-level longitude/latitude lookup used by make_phase_data / make_phase2_data.
site_latlong <- function(df) {
  df %>%
    dplyr::select(site_name, longitude, latitude) %>%
    distinct(site_name, .keep_all = TRUE)
}

# Site-level time-lag analysis (S5.12 / paper.qmd temporal-Mantel summary).
# Aggregates to site level, computes pairwise Jaccard dissimilarity and |ΔMedianBP|
# lag (ka), and returns trend statistics plus a binned summary table. Single source
# of truth so the supplement and main text report identical numbers.
time_lag_pairs <- function(df = signbase_full_clean) {
  site_sign <- df %>%
    group_by(site_name) %>%
    summarise(across(line:star, ~ as.numeric(sum(.) > 0)),
              MedianBP = median(MedianBP),
              phase2   = first(phase2),
              .groups = "drop")

  n_sites_ts <- nrow(site_sign)
  n_p1_ts    <- sum(site_sign$phase2 == "Aur-P1")
  n_p2_ts    <- sum(site_sign$phase2 == "Aur-P2")

  Y_dist   <- vegan::vegdist(site_sign %>% dplyr::select(line:star),
                             method = "jaccard", binary = TRUE)
  D_jac    <- as.matrix(Y_dist)
  D_lag    <- as.matrix(dist(site_sign$MedianBP))
  ut       <- upper.tri(D_jac)

  pairs_ts <- data.frame(
    lag_year   = D_lag[ut],
    lag_ka     = D_lag[ut] / 1000,
    jac        = D_jac[ut],
    same_phase = (outer(site_sign$phase2, site_sign$phase2, `==`))[ut]
  )
  n_pairs_ts <- nrow(pairs_ts)

  ts_all    <- cor.test(pairs_ts$lag_ka, pairs_ts$jac, method = "spearman")
  ts_within <- cor.test(pairs_ts$lag_ka[pairs_ts$same_phase],
                        pairs_ts$jac[pairs_ts$same_phase], method = "spearman")
  rho_all    <- round(unname(ts_all$estimate), 3)
  p_all      <- round(ts_all$p.value, 3)
  rho_within <- round(unname(ts_within$estimate), 3)
  p_within   <- round(ts_within$p.value, 3)

  mean_jac_within   <- round(mean(pairs_ts$jac[pairs_ts$same_phase]), 3)
  mean_jac_between  <- round(mean(pairs_ts$jac[!pairs_ts$same_phase]), 3)
  mean_lag_within   <- round(mean(pairs_ts$lag_ka[pairs_ts$same_phase]), 2)
  mean_lag_between  <- round(mean(pairs_ts$lag_ka[!pairs_ts$same_phase]), 2)

  pairs_ts$lag_bin <- cut(pairs_ts$lag_ka,
                          breaks = c(0, 2, 4, 6, 8, 10, 12, 15, 20, 50),
                          include.lowest = TRUE)
  ts_bin_tab <- pairs_ts %>%
    group_by(lag_bin) %>%
    summarise(`Pairs` = n(),
              `Mean Jaccard dissimilarity` = round(mean(jac), 3),
              `SD` = round(sd(jac), 3)) %>%
    ungroup()

  ts_jac_short <- round(ts_bin_tab$`Mean Jaccard dissimilarity`[1], 3)
  ts_last_bin  <- as.character(ts_bin_tab$lag_bin[nrow(ts_bin_tab)])
  ts_last_n    <- ts_bin_tab$Pairs[nrow(ts_bin_tab)]

  list(
    n_sites = n_sites_ts, n_p1 = n_p1_ts, n_p2 = n_p2_ts, n_pairs = n_pairs_ts,
    rho_all = rho_all, p_all = p_all, rho_within = rho_within, p_within = p_within,
    mean_jac_within = mean_jac_within, mean_jac_between = mean_jac_between,
    mean_lag_within = mean_lag_within, mean_lag_between = mean_lag_between,
    bin_tab = ts_bin_tab, jac_short = ts_jac_short, last_bin = ts_last_bin, last_n = ts_last_n,
    pairs = pairs_ts
  )
}

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
  sign_names <- SIGN_COLS
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
  sign_names <- SIGN_COLS
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
  sign_names <- SIGN_COLS
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

# ── Circularity-breaking validation for the restricted/broad PERMANOVA ─────────
# These functions answer the editor's concern (response-plan line 17) that the
# restricted/broad groups were defined from the same sign data tested in the
# PERMANOVA, so its p-values are not independent confirmation. Each method below
# either (a) accounts for the grouping having been optimised, (b) validates the
# groups on data withheld from their definition, or (c) drops discrete groups
# entirely in favour of a continuous gradient test.

# Direct PERMANOVA pseudo-F from a squared dissimilarity matrix.
# Matches vegan::adonis2(..., sqrt.dist = TRUE) on a jaccard distance object,
# because adonis2 with sqrt.dist applies sqrt to the distances and the Gower
# matrix is then built from their squares (i.e. the original jaccard distances).
# D2:    n x n matrix of squared dissimilarities (e.g. as.matrix(vegdist(mat, "jaccard", binary = TRUE)))
# group: factor/character vector of length n (group labels)
permanova_F <- function(D2, group) {
  stopifnot(is.matrix(D2), nrow(D2) == ncol(D2))
  # Square the distances so the Gower matrix matches vegan::adonis2(...,
  # sqrt.dist = TRUE) on a jaccard distance object (adonis2 squares the
  # effectively-sqrt distances back to the original jaccard distances).
  D2 <- as.matrix(D2)^2
  n <- nrow(D2)
  group <- as.factor(group)
  k <- nlevels(group)
  if (k < 2 || k >= n) return(NA_real_)
  rm <- rowMeans(D2); cm <- colMeans(D2); gm <- mean(D2)
  G  <- -0.5 * (D2 - outer(rm, rep(1, n)) - outer(rep(1, n), cm) + gm)
  SST <- sum(diag(G))                       # total SS = trace of Gower matrix
  SSB <- 0
  for (g in levels(group)) {
    idx <- which(group == g); ng <- length(idx)
    if (ng > 1) SSB <- SSB + sum(G[idx, idx]) / ng
  }
  SSW <- SST - SSB
  (SSB / (k - 1)) / (SSW / (n - k))
}

# Best 2-group split along an ordering, maximising pseudo-F.
# Searches all contiguous cuts of the ordered sites; returns the maximal F and
# the cut position (number of sites in the first group). min_size guards against
# degenerate 1-vs-rest partitions.
best_split_F <- function(D2, order = NULL, min_size = 2) {
  n <- nrow(D2)
  if (is.null(order)) order <- 1:n
  Do <- D2[order, order, drop = FALSE]
  best <- -Inf; best_k <- NA_integer_
  for (kk in min_size:(n - min_size)) {
    grp <- c(rep(1, kk), rep(2, n - kk))
    f <- permanova_F(Do, grp)
    if (is.finite(f) && f > best) { best <- f; best_k <- kk }
  }
  list(F = best, k = best_k)
}

# Permutation test of the best achievable 2-group separation.
# Addresses the editor's concern (response-plan line 17) that the
# restricted/broad groups were defined by optimising a
# split on the sign data, so the PERMANOVA p-value is circular. Here the null is
# generated by randomising the data under a structure-destroying model while
# preserving specified margins, re-deriving the best 2-group split from that
# randomised data in every permutation, and recording the maximal pseudo-F. The
# observed maximal pseudo-F is then compared to this null. A significant result
# means the data contain a two-group separation stronger than randomised data
# would yield even after the split is optimised in both observed and null, i.e.
# the grouping is not an artefact of having hunted for the best partition.
#
# mat:           site x sign binary matrix (rownames = sites). The distance
#                matrix is computed internally so the raw data can be permuted.
# group:         optional observed labels; if supplied, the manuscript PERMANOVA
#                F for those exact labels is also returned (manual_F) for
#                reference.
# null_type:     "curveball" (default, fixed-fixed, preserves row/col sums),
#                "column" (permutes each column independently),
#                "object_count" (Kintigh: resamples objects from phase pool with
#                observed per-site counts; requires object_df, site_sign_cols)
# object_df:     object-level data frame (required for null_type="object_count")
# site_sign_cols: sign column names (required for null_type="object_count")
best_split_permtest <- function(mat, group = NULL, B = 999, seed = 42,
                                min_size = 2,
                                null_type = c("curveball", "column", "object_count"),
                                object_df = NULL, site_sign_cols = NULL) {
  null_type <- match.arg(null_type)
  mat <- as.matrix(mat)
  n   <- nrow(mat)
  D2  <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  # Order sites by sign-repertoire richness: this mirrors how the restricted/
  # broad groups were actually defined (restricted = few sign types, broad =
  # many), so the maximally-selected split is a faithful re-derivation of the
  # grouping procedure rather than an arbitrary 1-D cut.
  ord_obs <- order(rowSums(mat, na.rm = TRUE))
  obs <- best_split_F(D2, ord_obs, min_size = min_size)
  obs_F_manual <- if (!is.null(group)) permanova_F(D2, group) else NA_real_
  null <- numeric(B)
  set.seed(seed)

  if (null_type == "curveball") {
    # Fixed-fixed null: preserves both row sums (site richness) and column sums
    # (sign-type frequencies) using the Curveball algorithm via vegan::nullmodel
    null_mod <- vegan::nullmodel(mat, "curveball")
    for (b in seq_len(B)) {
      sim <- simulate(null_mod, nsim = 1)
      mp <- sim[,,1]  # 3D array: sites x signs x nsim
      Dp   <- as.matrix(vegan::vegdist(mp, "jaccard", binary = TRUE))
      orp  <- order(rowSums(mp, na.rm = TRUE))
      null[b] <- best_split_F(Dp, orp, min_size = min_size)$F
    }
  } else if (null_type == "column") {
    # Column permutation: permutes each column independently, destroying
    # compositional structure while preserving column totals (sign-type freq)
    for (b in seq_len(B)) {
      mp <- apply(mat, 2, sample)
      Dp   <- as.matrix(vegan::vegdist(mp, "jaccard", binary = TRUE))
      orp  <- order(rowSums(mp, na.rm = TRUE))
      null[b] <- best_split_F(Dp, orp, min_size = min_size)$F
    }
  } else {  # "object_count" - Kintigh-style object-count null
    if (is.null(object_df) || is.null(site_sign_cols)) {
      stop("null_type = 'object_count' requires object_df and site_sign_cols")
    }
    cols <- intersect(site_sign_cols, names(object_df))
    pooled_objects <- object_df[, cols, drop = FALSE]
    pooled_objects <- pooled_objects[rowSums(pooled_objects) > 0, , drop = FALSE]
    n_obs <- rowSums(mat > 0)  # per-site object counts from presence/absence

    for (b in seq_len(B)) {
      mp <- mat  # same dims
      for (i in seq_len(n)) {
        idx <- sample(nrow(pooled_objects), n_obs[i], replace = TRUE)
        mp[i, ] <- as.numeric(colSums(pooled_objects[idx, , drop = FALSE]) > 0)
      }
      Dp <- as.matrix(vegan::vegdist(mp, "jaccard", binary = TRUE))
      orp <- order(rowSums(mp, na.rm = TRUE))
      null[b] <- best_split_F(Dp, orp, min_size = min_size)$F
    }
  }

  p <- (1 + sum(null >= obs$F, na.rm = TRUE)) / (1 + B)
  list(observed_best_F = obs$F, observed_best_k = obs$k,
       manual_F = obs_F_manual,
       null_F = null, p_value = p, B = B)
}

# Leave-one-out cross-validated group recovery (editor option c, site level).
# Trains a k-NN classifier on all OTHER sites and predicts each held-out site's
# group from its sign composition. Because the held-out site never contributes
# to its own prediction, a correct prediction is independent confirmation that
# the groups are recoverable from data not used to define them. Reports accuracy,
# a permutation p-value (labels shuffled), and a binomial 95% CI on accuracy.
cv_group_accuracy <- function(mat, group, k = 1, B = 999, seed = 42) {
  mat   <- as.matrix(mat)
  group <- as.factor(group)
  n     <- nrow(mat)
  D     <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  pred  <- character(n)
  for (i in seq_len(n)) {
    others <- setdiff(seq_len(n), i)
    ord    <- order(D[i, others])[seq_len(min(k, length(others)))]
    votes  <- group[others][ord]
    pred[i] <- as.character(names(sort(table(votes), decreasing = TRUE))[1])
  }
  correct <- pred == as.character(group)
  acc <- mean(correct)
  set.seed(seed)
  null_acc <- numeric(B)
  for (b in seq_len(B)) {
    gperm <- sample(group)
    p2    <- character(n)
    for (i in seq_len(n)) {
      others <- setdiff(seq_len(n), i)
      ord    <- order(D[i, others])[seq_len(min(k, length(others)))]
      votes  <- gperm[others][ord]
      p2[i]  <- as.character(names(sort(table(votes), decreasing = TRUE))[1])
    }
    null_acc[b] <- mean(p2 == as.character(gperm))
  }
  p <- (1 + sum(null_acc >= acc)) / (1 + B)
  ci <- stats::binom.test(sum(correct), n)$conf.int
  list(accuracy = acc, n = n, k = k, p_value = p,
       perm_acc = null_acc,
       ci_lo = unname(ci[1]), ci_hi = unname(ci[2]),
       B = B, predictions = pred)
}

# Object-half holdout PERMANOVA (editor option c, object level).
# For each site with >= 2 objects, split its objects into two random halves.
# The 2-group rule (low/high sign-repertoire richness) is learned from half A
# only; the group labels are then tested, by a fast permutation of pseudo-F, on
# the half B site signatures. Because half A and half B are disjoint sets of
# objects, a significant held-out test is independent confirmation that the
# grouping is not an artefact of any particular object sample. Repeated over
# B_rep random object splits; reports the distribution of held-out p-values and a
# Fisher-combined p-value across splits. Sites with < 2 objects are excluded.
cv_permanova_objectsplit <- function(object_df, site_sign_cols,
                                     B_rep = 200, B_perm = 999,
                                     seed = 42, threshold_median = NULL) {
  set.seed(seed)
  cols   <- intersect(site_sign_cols, names(object_df))
  sites  <- unique(object_df$site_name)
  usable <- sites[sapply(sites, function(s) sum(object_df$site_name == s) >= 2)]
  if (length(usable) < 4)
    return(list(n_usable = length(usable), p_values = numeric(0)))
  pvals <- numeric(B_rep)
  for (r in seq_len(B_rep)) {
    richA <- numeric(length(usable)); sigB_list <- vector("list", length(usable))
    for (j in seq_along(usable)) {
      s  <- usable[j]
      oi <- which(object_df$site_name == s)
      half <- sample(oi, floor(length(oi) / 2))
      sigA <- as.numeric(colSums(object_df[half, cols, drop = FALSE]) > 0)
      sigB <- as.numeric(colSums(object_df[setdiff(oi, half), cols, drop = FALSE]) > 0)
      richA[j]   <- sum(sigA)
      sigB_list[[j]] <- sigB
    }
    cut <- if (is.null(threshold_median)) median(richA) else threshold_median
    gA  <- ifelse(richA <= cut, 1, 2)
    matB <- do.call(rbind, sigB_list)
    rownames(matB) <- usable
    D2B  <- as.matrix(vegan::vegdist(matB, "jaccard", binary = TRUE))
    f_obs <- permanova_F(D2B, gA)
    null_f <- replicate(B_perm, permanova_F(D2B, sample(gA)))
    pvals[r] <- (1 + sum(null_f >= f_obs, na.rm = TRUE)) / (1 + B_perm)
  }
  list(n_usable = length(usable), usable_sites = usable,
       p_values = pvals,
       prop_significant = mean(pvals < 0.05),
       mean_p = mean(pvals),
       combined_p = fisher_combine_p(pvals))
}

# Group-free gradient PERMANOVA (editor option b): regress sign-composition
# dissimilarity on a continuous score (e.g. the first PCoA axis or repertoire
# richness) instead of asserting discrete groups. Tests for a gradient rather
# than a partition. Returns R2, F, and p from vegan::adonis2.
gradient_permanova <- function(D, score, seed = 42) {
  set.seed(seed)
  df <- data.frame(score = as.numeric(score))
  fit <- vegan::adonis2(D ~ score, data = df, permutations = 999)
  f <- as.data.frame(fit)
  list(R2 = round(f$R2[1], 3), F = round(f$F[1], 3), p = round(f$`Pr(>F)`[1], 3))
}

# Fisher's method to combine independent p-values into a single test.
fisher_combine_p <- function(p) {
  p <- p[is.finite(p) & p > 0]
  if (!length(p)) return(NA_real_)
  stat <- -2 * sum(log(p))
  pchisq(stat, df = 2 * length(p), lower.tail = FALSE)
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
# manual_groups is frozen source of truth (S2 reports); date_variant sensitivity may
# leave sites without a group (NA). Filter to valid groups so variant runs don't error.
s6_bootstrap_summary <- function(art_list, groups_list, n_boot = 1000) {
  boot <- lapply(art_list, s6_boot_consensus, n_boot = n_boot)
  tri  <- function(m) m[upper.tri(m)]
  map_dfr(names(boot), function(ph) {
    cs  <- boot[[ph]]$consensus
    sit <- rownames(cs); g <- groups_list[[ph]][sit]
    valid <- !is.na(g) & g %in% c(1, 2)
    sit <- sit[valid]; g <- g[valid]
    # Guard: need >=2 sites per group to compute within-block means
    wr <- if (sum(g == 1) >= 2) mean(tri(cs[sit[g == 1], sit[g == 1], drop = FALSE]), na.rm = TRUE) else NA_real_
    wb <- if (sum(g == 2) >= 2) mean(tri(cs[sit[g == 2], sit[g == 2], drop = FALSE]), na.rm = TRUE) else NA_real_
    xr <- if (sum(g == 1) >= 1 && sum(g == 2) >= 1) mean(cs[sit[g == 1], sit[g == 2], drop = FALSE], na.rm = TRUE) else NA_real_
    nr  <- sum(g == 1, na.rm = TRUE); nb <- sum(g == 2, na.rm = TRUE)
    ov  <- if (!is.na(wr) && !is.na(wb) && (choose(nr, 2) + choose(nb, 2)) > 0)
             (choose(nr, 2) * wr + choose(nb, 2) * wb) / (choose(nr, 2) + choose(nb, 2)) else NA_real_
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

# ── SBM fitting (S1 S6.3) ────────────────────────────────────────────────────────
# Fit stochastic block models for K = 1:5 blocks on a binary site x sign matrix.
# Fits two model families:
#   1. Bernoulli SBM on binarized adjacency (edge if Jaccard similarity >= 0.2)
#   2. Gaussian SBM on unthresholded weighted similarities (1 - Jaccard)
# Both use "SBM_sym" for undirected networks.
# Returns a list with $bernoulli and $gaussian elements, each containing:
#   $icl (numeric vector length 5), $bestK (int), $Z (n_sites x bestK matrix of posteriors)
# Also returns $edge_count_bernoulli (number of edges in binarized graph).
# Saves results to file if `save_path` is provided (default NULL: no caching).
fit_sbm <- function(artifact_data, max_K = 5, save_path = NULL, seed = 1) {
  if (!is.null(seed)) set.seed(seed)
  sign_names <- SIGN_COLS
  present <- intersect(sign_names, colnames(artifact_data))
  mat <- as.data.frame(artifact_data[, present, drop = FALSE])
  mat <- mat[, colSums(mat) > 0, drop = FALSE]
  # Preserve row names when converting to binary
  rownames_mat <- rownames(mat)
  mat <- as.matrix(mat)
  mat <- matrix(as.numeric(mat > 0), nrow = nrow(mat), ncol = ncol(mat))
  rownames(mat) <- rownames_mat
  colnames(mat) <- colnames(artifact_data)[present][colSums(artifact_data[, present, drop = FALSE] > 0) > 0]
  mat <- as.data.frame(mat)
  jac <- as.matrix(vegan::vegdist(mat, "jaccard", binary = TRUE))
  
  # Binarized adjacency for Bernoulli SBM (edge if similarity >= 0.2)
  adj_bin <- 1 - jac
  adj_bin[adj_bin < 0.2] <- 0
  adj_bin[adj_bin >= 0.2] <- 1
  diag(adj_bin) <- 0
  edge_count_bernoulli <- sum(adj_bin > 0) / 2
  
  # Unthresholded weighted adjacency for Gaussian SBM (similarities in [0, 1])
  adj_gauss <- 1 - jac
  diag(adj_gauss) <- 0
  
  # Suppress verbose output from blockmodels estimation
  sink(tempfile()); on.exit(sink())
  
  # Bernoulli SBM on binarized adjacency
  bm_bern <- blockmodels::BM_bernoulli("SBM_sym", adj_bin, verbosity = 0,
                                        explore_min = 1, explore_max = max_K)
  bm_bern$estimate()
  
  # Gaussian SBM on weighted adjacency
  bm_gauss <- blockmodels::BM_gaussian("SBM_sym", adj_gauss, verbosity = 0,
                                        explore_min = 1, explore_max = max_K)
  bm_gauss$estimate()
  
  sink()
  
  # Extract ICL for both models (length max_K)
  icl_bern <- bm_bern$ICL[1:max_K]
  icl_gauss <- bm_gauss$ICL[1:max_K]
  
  bestK_bern <- which.max(icl_bern)
  bestK_gauss <- which.max(icl_gauss)
  
  # Get posterior membership matrices Z for best K
  Z_bern <- bm_bern$memberships[[bestK_bern]]$Z
  Z_gauss <- bm_gauss$memberships[[bestK_gauss]]$Z
  
  # Preserve site names (rownames)
  rownames(Z_bern) <- rownames(adj_bin)
  colnames(Z_bern) <- paste0("Block", seq_len(bestK_bern))
  rownames(Z_gauss) <- rownames(adj_gauss)
  colnames(Z_gauss) <- paste0("Block", seq_len(bestK_gauss))
  
  res <- list(
    bernoulli = list(icl = icl_bern, bestK = bestK_bern, Z = Z_bern),
    gaussian  = list(icl = icl_gauss, bestK = bestK_gauss, Z = Z_gauss),
    edge_count_bernoulli = edge_count_bernoulli
  )
  if (!is.null(save_path)) {
    saveRDS(res, save_path)
  }
  res
}

# Fit SBM for both phases and optionally save combined results.
# `art_list`: named list of artifact_data per phase (e.g., list(Aur-P1 = ..., Aur-P2 = ...))
# `save_path`: path to write the RDS file (default NULL: no caching)
s6_fit_sbm_all <- function(art_list, max_K = 5, save_path = NULL) {
  out <- lapply(names(art_list), function(ph) {
    message("Fitting SBM for ", ph, " (K = 1:", max_K, ")...")
    fit_sbm(art_list[[ph]], max_K = max_K)
  })
  names(out) <- names(art_list)
  if (!is.null(save_path)) {
    saveRDS(out, save_path)
    message("Saved SBM results to ", save_path)
  }
  out
}

# ── SBM vs manual group mismatch (S1 S6.3) ──────────────────────────────────────
# Compute sites where SBM modal assignment diverges from manual restricted/broad
# groups after optimal block-to-group mapping.
# `sbm_list`: output of s6_fit_sbm_all (list per phase with $bernoulli and $gaussian)
# `boot_list`: output of s6_boot (list per phase with $consistency)
# `groups_list`: manual groups (manual_groups) per phase
# `model`: which SBM model to use ("bernoulli" or "gaussian")
# Returns character vector of site names that diverge.
s6_mismatch <- function(sbm_list, boot_list, groups_list, ph, model = "bernoulli") {
  if (!length(sbm_list) || !ph %in% names(sbm_list)) return(character())
  sit <- names(boot_list[[ph]]$consistency)
  m   <- groups_list[[ph]][sit]
  Z <- sbm_list[[ph]][[model]]$Z
  nm  <- apply(Z[sit, , drop = FALSE], 1, which.max)
  K <- max(nm); best_agree <- -1; best_map <- rep(1L, K)
  for (code in 0:(2^K - 1)) {
    mp <- ((code %/% (2^(0:(K-1)))) %% 2) + 1L
    agree <- sum(mp[nm] == as.integer(m))
    if (agree > best_agree) { best_agree <- agree; best_map <- mp }
  }
  names(m)[best_map[nm] != as.integer(m)]
}

# Compute SBM best K per phase for a given model
# `model`: which SBM model to use ("bernoulli" or "gaussian")
s6_bestK <- function(sbm_list, ph, model = "bernoulli") {
  if (ph %in% names(sbm_list) && model %in% names(sbm_list[[ph]]))
    sbm_list[[ph]][[model]]$bestK else NA_integer_
}

# ICL gap between best and second-best model for a given model
# `model`: which SBM model to use ("bernoulli" or "gaussian")
s6_gap <- function(sbm_list, ph, model = "bernoulli") {
  if (!length(sbm_list) || !ph %in% names(sbm_list) || !model %in% names(sbm_list[[ph]]))
    return(NA_real_)
  icl <- sbm_list[[ph]][[model]]$icl
  if (length(icl) < 2) return(NA_real_)
  sorted <- sort(icl, decreasing = TRUE)
  sorted[1] - sorted[2]
}

# Global minimum posterior probability across all phases/sites for a given model
# `model`: which SBM model to use ("bernoulli" or "gaussian")
s6_global_minPost <- function(sbm_list, model = "bernoulli") {
  if (!length(sbm_list)) return(NA_real_)
  min(unlist(lapply(sbm_list, function(x) {
    if (model %in% names(x) && !is.null(x[[model]]$Z)) min(x[[model]]$Z) else NA_real_
  })), na.rm = TRUE)
}

# ── Object-level downsampling & coverage-rarefaction helpers (S1 S9) ───────────

# 1) Per-site object counts for a phase (object-level rows of signbase_full_clean).
site_object_counts <- function(signbase, phase) {
  signbase %>% filter(phase2 == phase) %>%
    group_by(site_name) %>% summarise(nobjects = n(), .groups = "drop")
}

# 2) One replicate: site x sign presence matrix by sampling k objects per site.
downsample_site_matrix <- function(signbase, phase, k, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  df <- signbase %>% filter(phase2 == phase)
  sc <- intersect(SIGN_COLS, colnames(df))
  sites <- unique(df$site_name)
  rows <- list()
  for (s in sites) {
    objs <- df %>% filter(site_name == s)
    n <- nrow(objs)
    if (n < k) next
    samp <- objs[sample.int(n, k), , drop = FALSE]
    pv <- vapply(sc, function(cn) as.integer(any(as.numeric(samp[[cn]]) > 0)),
                 integer(1))
    rows[[s]] <- pv
  }
  if (!length(rows)) return(NULL)
  mat <- do.call(rbind, rows)
  rownames(mat) <- names(rows)
  mat
}

# Internal: map an arbitrary Louvain partition onto the 2-level manual groups by
# assigning each Louvain community to the majority manual group among its sites.
map_to_manual <- function(louvain_vec, manual_vec) {
  lv <- louvain_vec; mg <- manual_vec
  common <- intersect(names(lv), names(mg))
  lv <- lv[common]; mg <- mg[common]
  comms <- split(common, lv)
  out <- mg
  for (cs in comms) {
    tab <- table(mg[cs]); best <- names(tab)[which.max(tab)]
    out[cs] <- best
  }
  out
}

# 3) Object-level downsampling sensitivity: re-run network/Louvain across R
#    replicates at common object count k; report per-site group retention and
#    community-structure stability (mean ARI vs manual, modularity, n communities).
run_downsample_sensitivity <- function(signbase, phase, k, R = 1000,
                                       threshold = 0.2, seed = 123) {
  set.seed(seed)
  manual <- manual_groups[[phase]]
  df <- signbase %>% filter(phase2 == phase)
  site_counts <- df %>% group_by(site_name) %>% summarise(n = n(), .groups = "drop")
  qual <- site_counts$site_name[site_counts$n >= k]
  nsites_qual <- length(qual)
  ret_acc <- setNames(rep(0, nsites_qual), qual)
  ari_vec <- numeric(0); mod_vec <- numeric(0); ncomm_vec <- numeric(0)
  for (r in seq_len(R)) {
    mat <- downsample_site_matrix(signbase, phase, k, seed = seed + r)
    if (is.null(mat) || nrow(mat) < 2) next
    lv <- get_louvain_groups(mat, threshold = threshold)
    lv <- lv[intersect(names(lv), names(manual))]
    if (length(lv) < 2) next
    derived <- map_to_manual(lv, manual[names(lv)])
    for (s in intersect(names(derived), qual)) {
      if (as.character(derived[s]) == as.character(manual[s])) ret_acc[s] <- ret_acc[s] + 1
    }
    if (length(unique(derived)) > 1 &&
        length(unique(manual[names(derived)])) > 1) {
      ari_vec <- c(ari_vec, mclust::adjustedRandIndex(
        as.integer(derived), as.integer(manual[names(derived)])))
    }
    ns <- network_stats(mat, threshold = threshold)
    mod_vec   <- c(mod_vec, ns$modularity)
    ncomm_vec <- c(ncomm_vec, ns$n_communities)
  }
  retention <- data.frame(
    site_name = qual,
    nobjects  = site_counts$n[match(qual, site_counts$site_name)],
    retention_rate = ret_acc / R,
    manual_group   = as.integer(manual[qual]),
    stringsAsFactors = FALSE)
  list(n_sites_qualified = nsites_qual, k = k, R = R, threshold = threshold,
       retention = retention,
        mean_ari = if (length(ari_vec)) mean(ari_vec, na.rm = TRUE) else NA_real_,
       sd_ari   = if (length(ari_vec)) sd(ari_vec) else NA_real_,
       mean_modularity = mean(mod_vec, na.rm = TRUE),
       mean_n_communities = mean(ncomm_vec, na.rm = TRUE))
}

# 4) Coverage-based rarefaction: for each site, accumulate objects in random
#    order; record sign-type richness and sample coverage (1 - f1/m) at each
#    sample size m; interpolate richness at a target coverage. Returns per-site
#    table, group means, and a Wilcoxon test of richness-at-target by group.
coverage_rarefaction <- function(signbase, phase, target_coverage = 0.9,
                                 n_perm = 200, seed = 456) {
  set.seed(seed)
  df <- signbase %>% filter(phase2 == phase)
  sc <- intersect(SIGN_COLS, colnames(df))
  manual <- manual_groups[[phase]]
  out <- list()
  for (s in unique(df$site_name)) {
    objs <- df %>% filter(site_name == s)
    n <- nrow(objs)
    if (n < 1) next
    occ <- lapply(seq_len(n), function(i)
      which(vapply(sc, function(cn) as.numeric(objs[[cn]][i]) > 0, logical(1))))
    rich_curve <- numeric(n); cov_curve <- numeric(n)
    for (p in seq_len(n_perm)) {
      ord <- sample.int(n); cnt <- integer(length(SIGN_COLS))
      f1 <- 0; rich <- 0; rseq <- numeric(n); cseq <- numeric(n)
      for (m in seq_len(n)) {
        for (sg in occ[[ord[m]]]) {
          if (cnt[sg] == 0) { cnt[sg] <- 1; f1 <- f1 + 1; rich <- rich + 1 }
          else if (cnt[sg] == 1) { cnt[sg] <- 2; f1 <- f1 - 1 }
          else { cnt[sg] <- cnt[sg] + 1 }
        }
        rseq[m] <- rich
        cseq[m] <- if (m > 0) 1 - f1 / m else 0
      }
      rich_curve <- rich_curve + rseq
      cov_curve  <- cov_curve + cseq
    }
    rich_curve <- rich_curve / n_perm
    cov_curve  <- cov_curve / n_perm
    if (cov_curve[n] < target_coverage) rat <- NA_real_
    else {
      idx <- which(cov_curve >= target_coverage)[1]
      rat <- if (idx == 1) rich_curve[1] else
        rich_curve[idx-1] + (target_coverage - cov_curve[idx-1]) /
        (cov_curve[idx] - cov_curve[idx-1]) * (rich_curve[idx] - rich_curve[idx-1])
    }
    out[[s]] <- data.frame(site_name = s, nobjects = n,
                           richness_full = rich_curve[n],
                            coverage_full = max(0, min(1, cov_curve[n])),
                           richness_at_target = rat,
                           manual_group = as.integer(manual[s]),
                           stringsAsFactors = FALSE)
  }
  tab <- bind_rows(out)
  g1 <- na.omit(tab$richness_at_target[tab$manual_group == 1])
  g2 <- na.omit(tab$richness_at_target[tab$manual_group == 2])
  wt <- if (length(g1) >= 2 && length(g2) >= 2)
    wilcox.test(g1, g2) else list(p.value = NA_real_, statistic = NA_real_)
  list(table = tab,
       mean_restricted = mean(g1, na.rm = TRUE),
       mean_broad = mean(g2, na.rm = TRUE),
       wilcox_p = if (is.list(wt)) wt$p.value else NA_real_,
       target_coverage = target_coverage)
}

# 5) Negative-binomial model of sign-type richness with an object-count offset
#    (S1 S9.4). Tests whether the restricted/broad group effect on richness
#    survives after accounting for sampling effort (object count as exposure).
#    Defined once here and called by both paper.qmd and S1 S9.4 so the model is
#    fitted identically in the two documents and never duplicated.
# Args:
#   art_list:    named list of site x sign matrices, one per phase
#                (e.g. list("Aur-P1" = aurp1_artifact_data,
#                           "Aur-P2" = aurp2_artifact_data))
#   uniq_list:   named list of per-phase site-level data frames (must contain
#                $site_name and $nobjects), one per phase
#   groups_list: canonical manual groups (manual_groups) per phase
# Returns: list with $data (per-site richness/offset/group/phase), $fit
#          (glm.nb object), and scalar summaries $group_coef,
#          $group_rate_ratio, $group_p.
s9_offset_mixed_model <- function(art_list, uniq_list, groups_list) {
  richness_df <- bind_rows(lapply(names(art_list), function(ph) {
    art  <- art_list[[ph]]
    uniq <- uniq_list[[ph]]
    data.frame(
      site_name = rownames(art),
      phase     = ph,
      nobjects  = uniq$nobjects[match(rownames(art), uniq$site_name)],
      richness  = rowSums(art > 0)
    )
  }))
  grp_val <- mapply(function(s, p) groups_list[[p]][s],
                    richness_df$site_name, richness_df$phase)
  richness_df$group <- factor(
    ifelse(grp_val == 1, "restricted", "broad"),
    levels = c("restricted", "broad"))
  fit <- MASS::glm.nb(richness ~ group + phase + offset(log(nobjects)),
                      data = richness_df)
  co         <- coef(fit)
  group_coef <- co["groupbroad"]
  list(data = richness_df, fit = fit,
       group_coef = group_coef,
       group_rate_ratio = exp(group_coef),
       group_p = summary(fit)$coefficients["groupbroad", "Pr(>|z|)"])
}

# ── Shared sensitivity (S7 figurine-exclusion + S8 exclude-entire-Vogelherd) ──
# Single source of truth for the figurine/site-robustness statistics so that the
# main text (paper.qmd) and the S1/S8 supplements cannot report divergent numbers.
sensitivity_summary <- function(signbase_full_clean,
                                 aurp1_artifact_data, aurp2_artifact_data) {
  figurine_types <- c("figurine zoomorph", "figurine anthropomorph",
                      "figurine undet.", "possible figurine")

  figurine_summary <- signbase_full_clean %>%
    filter(object_type %in% figurine_types) %>%
    count(phase2, site_name, object_type) %>%
    pivot_wider(names_from = object_type, values_from = n, values_fill = 0) %>%
    mutate(Total = rowSums(across(where(is.numeric)))) %>%
    arrange(phase2, desc(Total))

  s7_baseline <- bind_rows(
    network_stats(aurp1_artifact_data) %>% mutate(Phase = "Aur-P1"),
    network_stats(aurp2_artifact_data) %>% mutate(Phase = "Aur-P2")
  ) %>% dplyr::select(Phase, n_sites, n_edges, density, mean_degree,
                      transitivity, modularity) %>%
    mutate(Condition = "Baseline (all objects)")

  # S7: rebuild each phase after excluding the filtered rows, then network_stats
  s7_phase_stats <- function(df, condition) {
    purrr::map_dfr(c("Aur-P1", "Aur-P2"), function(ph) {
      ph_df <- df %>% filter(phase2 == ph)
      if (n_distinct(ph_df$site_name) < 2) return(NULL)
      lat_long <- df %>% dplyr::select(site_name, longitude, latitude) %>%
        distinct(site_name, .keep_all = TRUE)
      ph_obj <- ph_df %>% group_by(site_name) %>% summarise(nobjects = n())
      ph_unique <- ph_df %>%
        group_by(site_name) %>%
        mutate(longitude = as.character(longitude), latitude = as.character(latitude)) %>%
        summarize(across(where(is.numeric), sum)) %>%
        left_join(lat_long) %>% left_join(ph_obj) %>%
        mutate(time_period = ph)
      ph_art <- extract_artifact(ph_unique)
      ns <- network_stats(ph_art, threshold = 0.2)
      data.frame(Phase = ph, n_sites = ns$n_sites, n_edges = ns$n_edges,
                 density = ns$density, mean_degree = ns$mean_degree,
                 transitivity = ns$transitivity, modularity = ns$modularity,
                 Condition = condition, stringsAsFactors = FALSE)
    })
  }

  # S7 Analysis: exclude all figurines (global)
  signbase_no_fig <- signbase_full_clean %>% filter(!object_type %in% figurine_types)
  s7_no_fig <- s7_phase_stats(signbase_no_fig, "Exclude all figurines")

  # S7 Analysis: exclude only Vogelherd figurines
  signbase_no_vog_fig <- signbase_full_clean %>%
    filter(!(site_name == "Vogelherd" & object_type %in% figurine_types))
  s7_no_vog_fig <- s7_phase_stats(signbase_no_vog_fig, "Exclude Vogelherd figurines")

  s7_sites_before <- signbase_full_clean %>% count(phase2, site_name) %>%
    group_by(phase2) %>% summarise(sites = n(), .groups = "drop")
  s7_sites_after <- signbase_no_fig %>% count(phase2, site_name) %>%
    group_by(phase2) %>% summarise(sites = n(), .groups = "drop")
  s7_sites_lost <- s7_sites_before %>%
    left_join(s7_sites_after, by = "phase2", suffix = c("_before", "_after")) %>%
    mutate(lost = sites_before - sites_after) %>% rename(Phase = phase2)

  s7_vog_before <- signbase_full_clean %>%
    filter(site_name == "Vogelherd") %>% count(phase2, name = "objects_before")
  s7_vog_after <- signbase_no_vog_fig %>%
    filter(site_name == "Vogelherd") %>% count(phase2, name = "objects_after")
  s7_vog_summary <- s7_vog_before %>%
    left_join(s7_vog_after, by = "phase2") %>%
    mutate(removed = objects_before - objects_after) %>% rename(Phase = phase2)

  joint_comparison <- bind_rows(s7_baseline, s7_no_fig, s7_no_vog_fig) %>%
    dplyr::select(Phase, Condition, n_sites, n_edges, mean_degree,
                  transitivity, modularity)
  s7_baseline_md <- s7_baseline %>% dplyr::select(Phase, Baseline_md = mean_degree)
  joint_comparison <- joint_comparison %>%
    left_join(s7_baseline_md, by = "Phase") %>%
    mutate(pct_change_md = round((mean_degree - Baseline_md) / Baseline_md * 100, 1)) %>%
    dplyr::select(-Baseline_md)

  # S8 Analysis B: exclude the entire Vogelherd site
  signbase_novog <- signbase_full_clean %>% filter(site_name != "Vogelherd")
  lat_long_nv <- signbase_novog %>%
    dplyr::select(site_name, longitude, latitude) %>% distinct(site_name, .keep_all = TRUE)
  aurp1_nv <- make_phase2_data("Aur-P1", signbase_novog, lat_long_nv)
  aurp2_nv <- make_phase2_data("Aur-P2", signbase_novog, lat_long_nv)
  s8_novog <- bind_rows(
    network_stats(extract_artifact(aurp1_nv), threshold = 0.2) %>% mutate(Phase = "Aur-P1"),
    network_stats(extract_artifact(aurp2_nv), threshold = 0.2) %>% mutate(Phase = "Aur-P2")
  ) %>% dplyr::select(Phase, n_sites, n_edges, density, mean_degree,
                      transitivity, modularity) %>%
    mutate(Condition = "Exclude Vogelherd")

  # ── Date-based object counts (S8 Analysis A prose) ──
  signbase_date <- signbase_full_clean %>%
    mutate(phase2_date = ifelse(time_period_date %in%
                                  c("proto_aurignacian", "early_aurignacian"),
                                "Aur-P1", "Aur-P2"))
  hf_p1       <- nrow(signbase_date %>% filter(site_name == "Hohle Fels",  phase2_date == "Aur-P1"))
  hf_p2       <- nrow(signbase_date %>% filter(site_name == "Hohle Fels",  phase2_date == "Aur-P2"))
  vog_p1_date <- nrow(signbase_date %>% filter(site_name == "Vogelherd",  phase2_date == "Aur-P1"))
  vog_p2_date <- nrow(signbase_date %>% filter(site_name == "Vogelherd",  phase2_date == "Aur-P2"))
  young_total <- hf_p2 + vog_p2_date

  # ── Scalars for the main-text sensitivity sentence (paper.qmd) ──
  md_base_p1   <- s7_baseline$mean_degree[s7_baseline$Phase == "Aur-P1"]
  md_base_p2   <- s7_baseline$mean_degree[s7_baseline$Phase == "Aur-P2"]
  md_nofig_p1  <- s7_no_fig$mean_degree[s7_no_fig$Phase == "Aur-P1"]
  md_novog_p1  <- s8_novog$mean_degree[s8_novog$Phase == "Aur-P1"]
  base_edges_p1  <- s7_baseline$n_edges[s7_baseline$Phase == "Aur-P1"]
  novog_edges_p1 <- s8_novog$n_edges[s8_novog$Phase == "Aur-P1"]
  edge_pct <- round((base_edges_p1 - novog_edges_p1) / base_edges_p1 * 100, 0)
  md_pct   <- round((md_base_p1 - md_novog_p1) / md_base_p1 * 100, 1)

  list(
    figurine_types   = figurine_types,
    figurine_summary = figurine_summary,
    s7_baseline      = s7_baseline,
    s7_no_fig        = s7_no_fig,
    s7_no_vog_fig    = s7_no_vog_fig,
    s7_sites_lost    = s7_sites_lost,
    s7_vog_before    = s7_vog_before,
    s7_vog_summary   = s7_vog_summary,
    joint_comparison = joint_comparison,
    s8_novog         = s8_novog,
    hf_p1 = hf_p1, hf_p2 = hf_p2,
    vog_p1_date = vog_p1_date, vog_p2_date = vog_p2_date,
    young_total = young_total,
    md_base_p1 = md_base_p1, md_base_p2 = md_base_p2,
    md_nofig_p1 = md_nofig_p1, md_novog_p1 = md_novog_p1,
    edge_pct = edge_pct, md_pct = md_pct
  )
}
