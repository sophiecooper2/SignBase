library(robin)

artifact_data <- trans_artifact_data %>% 
  select_if(~ !is.numeric(.) || sum(.) != 0) %>% 
  mutate_all(~ as.numeric(. > 0))

jac <- vegdist(artifact_data, "jaccard", binary = TRUE)
dm <- as.matrix(jac)

disim <- 1 - dm
disim[disim<0.2] <- 0
disim_net <- network(disim,
                     directed=F,
                     ignore.eval=F,
                     names.eval='weight')

disim_net %v% 'vertex.names' <- row.names(artifact_data)

modul_matrix <-
  graph_from_adjacency_matrix(1 - dm, mode = "undirected")


mem <- as_membership(parse_number(trans_unique_data$group))
as_group <- modularity(modul_matrix, membership = mem)


library(intergraph)

ig <- asIgraph(disim_net)

as_group <- modularity(ig, membership = mem)


g <- graph.adjacency(
  as.matrix(as.dist(cor(base::t(artifact_data), method="pearson"))),
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)

g <- make_graph(edges = row.names(artifact_data), directed = FALSE)

g$weight <- jac


g <- graph.adjacency(
  as.matrix(vegdist(artifact_data, "jaccard", binary = TRUE)),
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)


g <- graph.adjacency(
  as.matrix(as.dist(cor(t(artifact_data), method="pearson"))),
  mode="undirected",
  weighted=TRUE,
  diag=FALSE
)
as_group <- modularity(g, membership = mem)

jac_df <- dm %>% 
  as.data.frame() %>% 
  rownames_to_column("edges") %>% 
  pivot_longer(cols = `El Castillo`: `Pod Hradem`)

g <- graph_from_data_frame(d = jac_df, directed = FALSE)

as_group <- modularity(g, membership = mem)
