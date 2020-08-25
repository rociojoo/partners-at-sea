Fleet.Analysis <- function(vms.data,dyads,dossier.data.outputs,dossier.stats.outputs,gear){
  
  # vms.data,dyads,Weights,dossier.data.outputs,dossier.stats.outputs,gear,year,fleet.plot.par,remplir.mjm,fleet.cluster=NULL){
  
  vessels.1 <- unique(c(dyads$id.1,dyads$id.2))
  k <- length(unique(dyads$clust_new_order))
  

  num.vessels <- length(vessels.1)
  mat.counts.cl <- array(0, dim = c(num.vessels,num.vessels))
  
  dyads_1 <- dyads[which(dyads$clust_new_order == 1),]
  
  for (i in 1:(num.vessels-1)){
    for (j in (i + 1): num.vessels){
      ind <- which((dyads_1$id.1 == vessels.1[i] & dyads_1$id.2 == vessels.1[j]) | (dyads_1$id.2 == vessels.1[i] & dyads_1$id.1 == vessels.1[j]))
      mat.counts.cl[i,j] <- length(ind)
    }
  }
  
  mat.counts.cl <- t(mat.counts.cl)
  
  
  
  rownames(mat.counts.cl) <- vessels.1
  colnames(mat.counts.cl) <- vessels.1
  
  dim1_zero <- which(apply(mat.counts.cl,MARGIN=1,FUN=sum) == 0)
  dim2_zero <- which(apply(mat.counts.cl,MARGIN=2,FUN=sum) == 0)
  ind_zero <-  intersect(dim1_zero, dim2_zero)
  if (length(ind_zero) > 0){
    mat.counts.cl.non.zero <- mat.counts.cl[-ind_zero,-ind_zero]
  }else{
    mat.counts.cl.non.zero <- mat.counts.cl
  }
  
  
  set.seed(3952)
  
  g.matrix <- graph.adjacency(mat.counts.cl.non.zero, weighted=T, mode = "lower",diag = FALSE)
  g <- simplify(g.matrix)
  
  # General options for plotting.
  V(g)$label.family <- "Helvetica"
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  layout1 <- layout.fruchterman.reingold(g)
  V(g)$label.color <- "black" 
  # V(g)$label.color <- "darkblue"
  # V(g)$label.font <- font_text
  V(g)$frame.color <- "grey" #alpha("black", 0.7)
  V(g)$label.cex <- 1.5
  V(g)$color <- "#66c2a5" #color_back
  V(g)$size <- 10 * V(g)$degree / max(V(g)$degree)+ .2
  # V(g)$size <- 40 * num_sugg/sum(num_sugg)
  egam <- (log(E(g)$weight)+1.4) / max(log(E(g)$weight)+1.4) #3
  E(g)$width <- egam * 10
  # E(g)$arrow.size <- egam/3
  # E(g)$lty <- line_type
  E(g)$color <- alpha("#1b9e77", egam) #line_color
  
  
  pdf(paste0(dossier.stats.outputs,'Network-comptage-c1-',gear,'.pdf'),
      width=24,height=20,pointsize = 20)
  plot(g, layout=layout1, vertex.label.dist = 0.5, vertex.label=NA)
  dev.off()
  Din <- table(degree(g, mode=c("all"))) # think about in, out and total # when you care about direction
  Loyalty <- Din[names(Din) == "1"]/sum(Din[as.numeric(names(Din))>0]) # loyalty
  Trans <-  transitivity(g) # clustering coef (seems to make sense but cannot interpret it perfectly)
  Ed <- 1 - edge_density(g,loops=FALSE)
  
  pdf(paste0(dossier.stats.outputs,'Prop-partners-barplot-cl-1-',gear,'.pdf'),
      width=24,height=20,pointsize = 20)
  barplot(prop.table(table(degree(g))),main='')
  dev.off()
  
  if (length(Loyalty)==0){
    Loyalty <- 0
  }
  stats.fleet <- data.frame(Loyalty=as.numeric(Loyalty),Trans=Trans,Ed=Ed)
  save(stats.fleet,file=paste0(dossier.data.outputs,'StatsFleet-',gear,'.RData'))
  
  print(paste0('Loyalty: ',round(Loyalty,2)))
  print(paste0('Loyal vessels: ', Din[names(Din) == "1"]))
}

