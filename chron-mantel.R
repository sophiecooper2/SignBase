chron_df <- signbase_full_clean %>% 
  column_to_rownames("id") %>% 
  dplyr::select(MedianBP) %>% 
  dist()

chron_sign_df <- signbase_full_clean %>% 
  column_to_rownames("id") %>% 
  dplyr::select(line:star) %>% 
  vegdist("jaccard")

chron_mantel <- vegan::mantel(chron_df, chron_sign_df, permutations = 1000)
chron_mantel_plot <- mantel.correlog(chron_sign_df, chron_df, n.class=50, break.pts=NULL, cutoff=FALSE, r.type="pearson", nperm=999, mult="holm", progressive=TRUE)
plot(chron_mantel_plot)