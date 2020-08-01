pre.video.modif.allgears <- function(subset.plot,epais){
  
  # one color per vessel
  barcos <- unique(subset.plot$id)
  n <- length(unique(barcos))
  
  colores <- c("#d8b365","#5ab4ac")
  
  traits <- rep(1,n)
  epaisseur <- rep(epais,n)
  color.trait <- data.frame(barcos,colores,traits,epaisseur)
  matriz.2 <- merge(subset.plot,color.trait,by.x='id',by.y='barcos')
  
    matriz.2$lon.real <- matriz.2$LONGITUDE
    matriz.2$lat.real <- matriz.2$LATITUDE
    matriz.2$LONGITUDE <- matriz.2$lon.real - min(matriz.2$lon.real)
    matriz.2$LATITUDE <- matriz.2$lat.real - min(matriz.2$lat.real)
    matriz.3 = matriz.2[order(matriz.2$date),]
    variables.plot <- c('id','date','burst','LONGITUDE','LATITUDE','colores',
                        'traits','epaisseur')
    data.plot <- matriz.3[,variables.plot]
  
  data.plot$colores <- as.character(data.plot$colores)
  
  return(data.plot)
}
