# The stats from each cluster function 
# we need k (number of clusters) and a dyads_cluster df with one row per dyad and columns like cluster, duraction, pc1 and so on
# it computes the number of vessels, couples, dyads, average duration of dyads and average pc1 for each cluster and the total dataset
stats_clusters <- function(k, dyads_cluster){
  stats.clusters <-
    data.frame(
      Vessels = rep(NA, k + 1),
      Vessels.Perc = rep(NA, k + 1),
      Couples = rep(NA, k + 1),
      Dyads = rep(NA, k + 1),
      Dyads.Perc = rep(NA, k + 1),
      Duration = rep(NA, k + 1)#,
      # PC1 = rep(NA, k + 1)
    )
  rownames(stats.clusters) <- c("Total", paste0("Cluster-", 1:k))
  stats.clusters$Vessels[1] <-
    length(unique(c(
      dyads_cluster$id.1, dyads_cluster$id.2
    )))
  stats.clusters$Vessels.Perc[1] <-
    100.00 
  stats.clusters$Couples[1] <-
    length(unique(paste0(
      dyads_cluster$id.1, '-', dyads_cluster$id.2
    )))
  stats.clusters$Dyads[1] <- dim(dyads_cluster)[1]
  stats.clusters$Dyads.Perc[1] <-
    100.00 #round((dim(dyads_cluster)[1]/dim(dyads)[1])*100,2)
  stats.clusters$Duration[1] <-
    round(median(dyads_cluster$dur), 2)
  # stats.clusters$PC1[1] <- round(median(dyads_cluster$pca1), 2)
  
  rien <- sapply(1:k, function(x) {
    ind <- which(dyads_cluster$clust_new_order == x)
    stats.clusters$Vessels[x + 1] <<-
      length(unique(c(
        dyads_cluster$id.1[ind], dyads_cluster$id.2[ind]
      )))
    stats.clusters$Vessels.Perc[x + 1] <<-
      round(stats.clusters$Vessels[x + 1] / stats.clusters$Vessels[1] * 100, 2)
    stats.clusters$Couples[x + 1] <<-
      length(unique(paste0(
        dyads_cluster$id.1[ind], '-', dyads_cluster$id.2[ind]
      )))
    stats.clusters$Dyads[x + 1] <<- length(ind)
    stats.clusters$Dyads.Perc[x + 1] <<-
      round(stats.clusters$Dyads[x + 1] / dim(dyads_cluster)[1] * 100, 2)
    stats.clusters$Duration[x + 1] <<-
      round(median(dyads_cluster$dur[ind]), 2)
    
  })
  
  return(stats.clusters)
}
