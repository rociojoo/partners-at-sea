plot.dyad.allgears = function(data.video,file.name,point.size=0.6,dossier.stats.outputs=getwd(),line.width=3){ 
  
  x <- data.video$LONGITUDE
  y <- data.video$LATITUDE
  xlim = range(x) 
  ylim = range(y) 
  
  tiempos <- unique(data.video$date)
  
  ind.bateaux <- which(duplicated(data.video$id) == 'FALSE')
  
  pdf(paste0(dossier.stats.outputs,file.name,'.pdf'))

    plot(x[1],y[1],xlim = xlim,ylim = ylim,cex=point.size,pch=20,
         col=20,xlab='',ylab='') #main=range(tiempos),
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")#Coloring the sea lightsteelblue1
  bato <- unique(data.video$id)
  nothing <- lapply(bato,function(b){
    ind.bato <- which(data.video$id == b)
    if (length(ind.bato) > 1){
      t <- length(ind.bato)-1
      segments(x[ind.bato[1:t]],y[ind.bato[1:t]],
               x[ind.bato[(1:t)+1]],y[ind.bato[(1:t)+1]],                                                                         
               col=data.video$colores[ind.bato[1]],lty=data.video$traits[ind.bato[1]],lwd=line.width)
    }
  })
  # vessels in the last time step
  barcos <- unique(data.video$id)
  flechas <- sapply(barcos,function(z) {
    ind.last.barco <- which(data.video$id == z)
    if (length(ind.last.barco) > 1){
      s <- ind.last.barco[(length(ind.last.barco)-1):length(ind.last.barco)]
      if (x[s[1]] != x[s[2]] || y[s[1]] != y[s[2]]){
        arrows(x[s[1]],y[s[1]],x[s[2]],y[s[2]],length=0.1,lwd=3,col=data.video$colores[s[1]])
      }
    }
  })

  dev.off()
  
}
