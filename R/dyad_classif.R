# The prediction and stats-per-cluster function for each gear
# It takes the GMM model fitted with PTM data and predicts for the gear we want,
# it also produces graphs and stats tables (so it needs the function above)
# gear: LOTB, SOTB, OTM, APS-10 or thon
# variables: metrics
# dyads: the entire df
# BestModel: fitted with PTM data
# k: number of clusters
# order_clust: order of old labels of clusters
# cluster.file: name of the file where we are storing general information on the clusters
# weight_var: contribution of each metric to pc1. Not anymore.
# dossier.stats.outputs: path to where we are storing stat and plot files
# dossier.data.outputs: path to where we are storing some data outputs
# dta: PTM data frame with groups

dyad_classif <- function(gear, variables, dyads, BestModel, k, order_clust, cluster.file, dossier.stats.outputs, dossier.data.outputs, dta){
  # weight_var, load_pca){
  
  dyads %>%  filter(gear.1 == gear) %>% select(variables) -> dta_gear
  
  prediction <-
    mixmodPredict(data = dta_gear[, 1:3], classificationRule = BestModel["bestResult"])
  
  dta_gear %>% mutate(
    clust_prox = factor(prediction@partition),
    p_cluster = apply(prediction@proba, 1, max)
  ) -> dta_gear
  dta_gear$clust_prox <-
    as.numeric(as.character(dta_gear$clust_prox))
  
  # changing orders
  new_order_vector <- dta_gear$clust_prox
  # k <- length(unique(new_order_vector))
  for (cl in 1:k) {
    new_order_vector[which(dta_gear$clust_prox == order_clust[cl])] <-
      cl
  }
  dta_gear$clust_new_order <- new_order_vector
  dta_gear$group <- as.factor(dta_gear$clust_new_order)
  
  dta_gear %>% ggplot(aes(x = dta_gear[, 1], y = dta_gear[, 2], col = group)) + geom_point() + xlab('Prox') + ylab('DItheta') + scale_colour_viridis_d() + theme_bw()
  ggsave(paste0(
    dossier.stats.outputs,
    "clustFree_Prox_DItheta_PTM_",
    gear,
    ".pdf"
  ))
  ggsave(paste0(
    dossier.stats.outputs,
    "clustFree_Prox_DItheta_PTM_",
    gear,
    ".png"
  ))
  dta_gear %>% ggplot(aes(x = dta_gear[, 1], y = dta_gear[,3], col = group)) + geom_point() + xlab('Prox') + ylab('DId') + scale_colour_viridis_d() + theme_bw()
  ggsave(paste0(
    dossier.stats.outputs,
    "clustFree_Prox_DId_PTM_",
    gear,
    ".pdf"
  ))
  ggsave(paste0(
    dossier.stats.outputs,
    "clustFree_Prox_DId_PTM_",
    gear,
    ".png"
  ))
  
  # 
  perc_table_gear <-
    round(prop.table(table(dta_gear$group)) * 100, 1)
  
  print(perc_table_gear)
  
  sink(cluster.file, append = TRUE)
  cat(gear)
  cat(" - Percentage of dyads in each ordered cluster: ", "\n")
  print(perc_table_gear)
  cat("\n")
  cat("\n")
  sink()
  
  # Now plotting distributions of probabilities per cluster
   
  if (nlevels(dta_gear$group) == 2){
    dta_gear_mod <- dta_gear %>% select(p_cluster,group)
    dta_gear_mod$group <- as.numeric(as.character(dta_gear_mod$group))
    dta_gear_mod <- rbind.data.frame(c(NA,1),dta_gear_mod)
    dta_gear_mod$group <- as.factor(dta_gear_mod$group)
    ggplot(data = dta_gear_mod, aes (x = group, y = p_cluster, fill = group)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, begin = 0.5)  +
      theme_bw() +
      xlab('') + ylab("") +
      theme(legend.position = "none", text = element_text(size = 18))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_proba_PTM_",
      gear,
      "_ordered.pdf"
    ))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_proba_PTM_",
      gear,
      "_ordered.png"
    ))
    
  }else{
    
    ggplot(data = dta_gear, aes (x = group, y = p_cluster, fill = group)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE)  +
      theme_bw() +
      xlab('') + ylab("") +
      theme(legend.position = "none", text = element_text(size = 18))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_proba_PTM_",
      gear,
      "_ordered.pdf"
    ))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_proba_PTM_",
      gear,
      "_ordered.png"
    ))
  }
  
  
  dta_facto_gear  <- dta_gear[, c(1, 2, 3, 7)]
  
  dta_gather_gear <- dta_facto_gear %>%
    gather(key = metric, value = value, -group)
  dta_gather_gear$metric <-
    factor(dta_gather_gear$metric, levels = c("Prox", "DI.theta", "DI.d"))
  #names(weight_var)[order(weight_var,
  # decreasing = TRUE)])
  
  # histograms
  ggplot(data = dta_gather_gear, aes(x = value, color = group)) + 
    geom_histogram(fill = "white", position = "dodge") +
    # geom_density(alpha=0.6)+
    facet_wrap(facets = vars(metric), scales = "free") +
    scale_color_viridis(discrete = TRUE) +
    theme_bw() +
    xlab('') + ylab("") +
    theme(legend.position = "none", text = element_text(size = 16))
  ggsave(
    paste0(
      dossier.stats.outputs,
      "clustFree_MetricsHist_PTM_",
      gear,
      "_ordered.pdf"
    ), width = 8
  )
  ggsave(
    paste0(
      dossier.stats.outputs,
      "clustFree_MetricsHist_PTM_",
      gear,
      "_ordered.png"
    ), width = 8
  )
  
  # 
  ####### stats from clustering for tables #######
  # dyads_cluster <- dyads %>% filter(gear.1 == "PTM")
  dyads_cluster_gear <- dyads %>% filter(gear.1 == gear) 
  dyads_cluster_gear <-
    cbind.data.frame(dyads_cluster_gear, dta_gear[, c("p_cluster", "clust_new_order")])
  
  saveRDS(
    dyads_cluster_gear,
    file = paste0(
      dossier.data.outputs,
      'ClustPCAoutputs-PTM-',
      gear,
      '-',
      paste0(variables, collapse = '-'),
      '_ordered.rds'
    )
  )
  
  stats.clusters.gear <- stats_clusters(k, dyads_cluster_gear)
  
  save(
    stats.clusters.gear,
    file = paste0(
      dossier.data.outputs,
      "ClusterTable-",
      k,
      '-PTM-',
      gear,
      '-',
      paste(variables, collapse = '-'),
      'ordered.RData'
    )
  )
  
  print(
    xtable(t(stats.clusters.gear)),
    file = paste0(
      dossier.stats.outputs,
      "ClusterLatexTable-",
      k,
      '-PTM-',
      gear,
      '-',
      paste(variables, collapse = '-'),
      'ordered.txt'
    )
  )
  
  ptm_cluster_1 <- dta %>% 
    filter(group == 1)
  gear_cluster_1 <- dta_gear %>% 
    filter(group == 1)
  
  
  get_hist <- function(ptm_cluster_1, gear_cluster_1, metric_name, break_start, 
                       nbins){
    
    hist_res <- hist(ptm_cluster_1[,metric_name], breaks = seq(break_start,1,length.out = nbins+1), freq = TRUE)
    
    marca_clase <- hist_res$breaks[1:(length(hist_res$breaks)-1)] + diff(hist_res$breaks)/2
    rel_freq <- hist_res$counts/sum(hist_res$counts)
    
    ptm_hist <- data.frame(metric = marca_clase, counts = rel_freq, 
                           gear = rep("PTM",length(marca_clase)))
    
    # Now I need to get the number of counts in each class interval defined with PTMs
    abs_freq <- sapply(1:length(marca_clase),function(x){
      count <- sum(gear_cluster_1$Prox > hist_res$breaks[x] & 
                     gear_cluster_1$Prox <= hist_res$breaks[x+1])
    })
    
    gear_rel_freq <- abs_freq/sum(abs_freq)
    
    gear_hist <- data.frame(metric = marca_clase, counts = gear_rel_freq, 
                            gear = rep(gear,length(marca_clase)))
    
    hist_df <- rbind.data.frame(ptm_hist,gear_hist)
    hist_df$gear <- as.factor(hist_df$gear)
    
    # filtering out boring values
    hist_df <- hist_df %>% filter(metric > 0.5)
    
    ggplot(hist_df, aes(x = metric, y = counts, fill = gear)) +
      geom_bar(stat = "identity", position = position_dodge()) + 
      scale_fill_viridis(discrete = TRUE, option = "E", begin = 0.1, end = 0.75) + # PTM is blue
      xlab('') + ylab('') +
      theme_bw() +
      theme(legend.position = "none", text = element_text(size = 16))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_hist1_PTM_",
      gear,"_", metric_name,
      "_zoom.pdf"
    ))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_hist1_PTM_",
      gear,"_", metric_name,
      "_zoom.png"
    ))
    
    
    ggplot(hist_df, aes(x = metric, y = counts, group = gear, color = gear)) +
      geom_line() +
      geom_point() +
      scale_color_viridis(discrete = TRUE, option = "E", begin = 0.1, end = 0.75) + # PTM is blue
      xlab('') + ylab('') +
      theme_bw() +
      theme(legend.position = "none", text = element_text(size = 16))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_line1_PTM_",
      gear,"_", metric_name,
      "_zoom.pdf"
    ))
    ggsave(paste0(
      dossier.stats.outputs,
      "clustFree_line1_PTM_",
      gear,"_", metric_name,
      "_zoom.png"
    ))
    
  }
  
  # PROX
  get_hist(ptm_cluster_1, gear_cluster_1, "Prox", break_start = 0, nbins = 30)
  # DI.theta
  get_hist(ptm_cluster_1, gear_cluster_1, "DI.theta", break_start = -1, nbins = 30)
  # PROX
  get_hist(ptm_cluster_1, gear_cluster_1, "DI.d", break_start = 0, nbins = 30)
  
}
