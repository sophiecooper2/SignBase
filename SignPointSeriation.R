library(tidyverse)
library(magrittr)

signbase_full <- read_csv("signBase_Version1.0.csv")

artifact_matrix <- signbase_full %>% 
  group_by(site_name) %>% 
  summarize(across(where(is.numeric), sum)) %>% 
  select(line:star) %>% 
  mutate(across(everything(), ~replace(., . > 1, 1))) %>% 
  as.matrix()

row.names(artifact_matrix) <- unique(signbase_full$site_name)

artifact_matrix <- artifact_matrix[!rowSums(artifact_matrix) == 0,]

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
         fill = "Count")}


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


