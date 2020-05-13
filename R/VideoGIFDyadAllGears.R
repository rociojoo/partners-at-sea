video.dyad.gif.allgears = function(data.video,file.name,image.name,image.dir,sleep=0.25,
                              point.size=0.6,traza=11){ 
  print(file.name)
  
  x <- data.video$LONGITUDE
  y <- data.video$LATITUDE
    xlim = range(x) 
    ylim = range(y) 
  
  tiempos <- unique(data.video$date)
  
  # generating graphic
  ind.bateaux <- which(duplicated(data.video$id) == 'FALSE')
  
  saveGIF({
    
    #ani.record(reset = TRUE)
    
    #ani.options(interval = sleep)
    
    for(i in seq_along(tiempos)){
      
      
        plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=20,
             col=20,main=tiempos[i],xlab='longitude',ylab='latitude')
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
        ind.time <- which(data.video$date == tiempos[1])
        
        if (traza == 0 || i <= traza){
          orig <- 1
        }else{
          ind.time.orig <- which(data.video$date == tiempos[i-traza])
          orig <- ind.time.orig[1]
        }
        ind.time <- which(data.video$date == tiempos[i])
        
        vector <- orig:ind.time[length(ind.time)]
      
      
      if(i>1){ 
        
        bato <- unique(data.video$id[vector])
        
        # for each vessel, we have to search its past
        nothing <- lapply(bato,function(b){
          vector.2 <- 1:ind.time[length(ind.time)]
          ind.bato <- which(data.video$id[vector.2] == b)
          if (length(ind.bato) > 1){
            
            t <- length(ind.bato)-1
            if (traza == 0 || i <= traza || t <= traza){
              orig <- 1
            }else{
              tiempo.seg <- tiempos[i-traza]
              ind.time.orig <- which(data.video$date[vector.2[ind.bato]] == tiempos[i-traza])
              if (length(ind.time.orig)>0){
                orig <- ind.time.orig[1]
              }else{
                ind.time.orig <- which(data.video$date[vector.2[ind.bato]] >= tiempos[i-traza])
                orig <- ind.time.orig[1]
              }
              
              # orig <- t - traza
            }
            
            segments(x[vector.2[ind.bato[orig:t]]],y[ind.bato[vector.2[orig:t]]],
                     x[vector.2[ind.bato[(orig:t)+1]]],y[vector.2[ind.bato[(orig:t)+1]]],                                                                         
                     col=data.video$colores[vector.2[ind.bato[1]]],lty=data.video$traits[vector.2[ind.bato[1]]],
                     lwd=data.video$epaisseur[vector.2[ind.bato[1]]])
            
          }
        })
        # vessels from the present time
        barcos <- unique(data.video$id[ind.time])
        flechas <- sapply(barcos,function(z) {
          vector.2 <- 1:ind.time[length(ind.time)]
          ind.last.barco <- which(data.video$id[vector.2] == z)
          if (length(ind.last.barco) > 1){
            s <- ind.last.barco[(length(ind.last.barco)-1):length(ind.last.barco)]
            if (x[vector.2[s[1]]] != x[vector.2[s[2]]] || y[vector.2[s[1]]] != y[vector.2[s[2]]]){
              print(i)
              arrows(x[vector.2[s[1]]],y[vector.2[s[1]]],x[vector.2[s[2]]],y[vector.2[s[2]]],length=0.1,
                     lwd=data.video$epaisseur[vector.2[s[1]]],col=data.video$colores[vector.2[s[1]]])
            }
          }
        })
      }
      

      #ani.pause()
      
    }}, img.name = image.name, movie.name = file.name, interval = 0.5)
    #ani.height=800,ani.width=800, autoplay = FALSE,imgdir=image.dir,autobrowse=FALSE,
    #navigator = FALSE,ani.nmax=length(tiempos))
  
}


# saveGIF({
#   for (i in unique(barco$Cod.Viaje.VMS)){
#     
#     barcoViaje <- barco[barco$Cod.Viaje.VMS == i,]
#     plot(barcoViaje$LONGITUDE, barcoViaje$LATITUDE, type = "o", pch = 16, col = 4, 
#          ylab = "LATIDUD",
#          xlab = "LATITUD", xlim = c(-80,-75), ylim = c(-15,-8))
#     lines(shoreline)
#     
#   }#})
# }, movie.name = "barco.gif", interval = 0.5, nmax = 30)