jac <- vegdist(trans_artifact_data %>%  mutate_all(~ as.numeric(. > 0)), "jaccard", binary = TRUE)
dm <- as.matrix(jac)
disim <- 1 - dm
disim[disim<0.1] <- 0
disim_net <- network(disim,
                     directed=F,
                     ignore.eval=F,
                     names.eval='weight')

phyl <- nj(dm)

mem <- as_membership((trans_unique_data$group))




library(geomorph)
res <- modularity.test(dm, partition.gp = mem, CI = FALSE)
plot(res)

data(pupfish) 
Y.gpa <- gpagen(pupfish$coords, print.progress = FALSE)   
#landmarks on the body and operculum
land.gps <- rep('a',56); land.gps[39:48] <- 'b'

MT <- modularity.test(Y.gpa$coords, land.gps, CI = FALSE)
summary(MT) # Test summary
plot(MT)



