
jac <- vegdist(trans_artifact_data %>% mutate_all(~ as.numeric(. > 0)), "jaccard", binary = TRUE)
dm <- as.matrix(jac)
modul_matrix <-
  graph_from_adjacency_matrix(1 - dm, mode = "undirected")
mem <- as_membership(parse_number(trans_unique_data$group))
gt <- modularity(modul_matrix, membership = mem)

modularity_function<- function(modul_matrix, mem){
  gt <- modularity(modul_matrix, membership = mem)
  N <- nrow(jac)
  nsteps <- 10000
  modul_table <- tibble("modularity" = c())
  for (i in 1:nsteps){
    mem_random <- as_membership(c(sample(1:N, N, replace = T)))
    modularity_permute <- modularity(modul_matrix, membership = mem_random)
    modul_row <- tibble("modularity" = modularity_permute)
    modul_table <- modul_table %>% rbind(modul_row)}
  num <- nrow(modul_table %>% 
                filter(modularity > gt))
  p_score <- num/10000
  return(p_score)
}


modularity_function(modul_matrix, mem)



