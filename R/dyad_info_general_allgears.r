dyad_info_gral_allgears <- function(tr1,tr2,echant,file.name){
  
  matriz <- rbind(tr1,tr2)
  fecha.min <- min(matriz$date)
  fecha.max <- max(matriz$date)
  
  sink(file.name,append = TRUE)
  cat("Cluster ",echant$clust_new_order,"\n")
  cat("\n")
  cat("\n")
  cat("Segment from ",as.character(fecha.min)," to ",as.character(fecha.max),"(",
      as.character(echant$dur)," hours)","\n")
  cat("\n")
  cat("Vessel 1: ",echant$id.1,"\n")
  cat("Trip code: ",echant$burst.1,"\n")
  sink()

  sink(file.name,append = TRUE)
  cat("\n")
  cat("Vessel 2: ",echant$id.2,"\n")
  cat("Trip code: ",echant$burst.2,"\n")
  sink()
  

}