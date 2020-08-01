plots.video.dyads.allgears <- function(vms.data, dyads, n=5, gear, new.variables, dossier.data.outputs, dossier.stats.outputs, text.dyads=TRUE, video.par, colores=NULL, Prox_delta = c(1,3,5), DI_beta = 1){
  
  k <- length(unique(dyads$clust_new_order))
  groups <- sort(unique(dyads$clust_new_order))
  echantillon <- as.data.frame(matrix(NA,ncol=dim(dyads)[2],nrow = n*k))
  colnames(echantillon) <- names(dyads)
  ## vessels and dyads in first (strongest) group
  rien <- sapply(groups,function(group){
    ind.1 <- which(dyads$clust_new_order == group)
    muestra <- order(dyads$p_cluster[ind.1],decreasing = TRUE)[1:n]
    echantillon[(1:n)+((group-1)*n),] <<- dyads[ind.1[muestra],]
  })
  
  ind_na <- which(apply(X = echantillon, MARGIN = 1, FUN = function(x) {all(is.na(x))}) == TRUE)
  if (length(ind_na) > 0){
    echantillon <- echantillon[-ind_na, ] 
  }
  
  save(echantillon,file = paste0(dossier.data.outputs,'muestra_clusters_CG_',gear,'_',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'.RData'))
  
  if (text.dyads == TRUE){
    dyad.file = paste0(dossier.stats.outputs,'dyads_',gear,'_clusters_',k,'_samplesCG_',paste(str_replace_all(new.variables, "[[:punct:]]", ""),collapse='-'),'.txt')
    sink(dyad.file)
    sink()
    
  }
  
  if (dir.exists(paste0(dossier.stats.outputs, gear)) == FALSE){
    dir.create(paste0(dossier.stats.outputs, gear)) 
  }
  
  
  action <- sapply(1:(dim(echantillon)[1]),function(x){
    print(x)
    # print(echant)
    echant <- echantillon[x,]
    bato1 <- echant$id.1
    bato2 <- echant$id.2
    
    vms.year <- vms.data[which(lubridate:::year(vms.data$date) == echant$year),]
    
    
    first_lines <- sapply(echant$first.line.1:echant$last.line.1,function(x) match(x,vms.year$line)) # if error, try line.first.1 and line.last.1
    tr1 <- vms.year[first_lines,]
    first_lines <- sapply(echant$first.line.2:echant$last.line.2,function(x) match(x,vms.year$line)) # if error, try line.first.1 and line.last.1
    tr2 <- vms.year[first_lines,] # idem
   
    
    if (text.dyads == TRUE){
      dyad_info_gral_allgears(tr1,tr2,echant,file.name=dyad.file)
    }
    
    matriz <- rbind(tr1,tr2)
    fecha.min <- min(matriz$date)
    fecha.max <- max(matriz$date)
    
    
    data.plot.2 <- pre.video.modif.allgears(subset.plot = matriz, epais = video.par$epais)
    
    
    Name.Mod <- "Modif"
    
    
    
    if (video.par$type == 'video' || video.par$type == 'all'){
      
      data.plot <- data.plot.2
      
      name <- paste0("film-",Name.Mod,"-Two-",gear,'-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                     paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                     paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                     '-cl-',echant$clust_new_order)
      
      image.name <- paste0("film-",Name.Mod,"-Two-id-",gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                           '-cl-',echant$clust_new_order,"-")
      # }
      
      if (video.par$html == TRUE){
        file.name.html <- paste0(name,".html")
        image.dir.movie <- paste0(dossier.stats.outputs,gear,"/Movies/",name)
        
        if (dir.exists(image.dir.movie)){
          print(paste0(name,' already exists, so not doing anything!'))
          
        }else{
          
          if (x == 1){
            dir.create(image.dir.movie,recursive=TRUE) # creating directory where png files will be stocked
          }else{dir.create(image.dir.movie,recursive=FALSE)} # creating directory where png files will be stocked
          
          png.dir <- paste0(dossier.stats.outputs,gear,"/Movies/",name)
          
          # creating html
          no.display <- video.dyad.general.allgears(data.video = data.plot, file.name = file.name.html, image.name = image.name, image.dir = png.dir, sleep=video.par$sleep, point.size=video.par$point.size, traza=video.par$traza)
          
          
          if (video.par$mp4 == TRUE){
            # creating mp4 (needs png created during html process)
            
            fichiers= paste0(image.dir.movie,"/",image.name,"%d.png")
            fichier_out <- paste0(dossier.stats.outputs,gear,"/Movies/",name,".mp4")
            system(paste("ffmpeg -framerate 10 -i ",fichiers," -c:v libx264 ", fichier_out,sep=""))
          }
          
        }
        
      }
      
      
      
      if (video.par$gif == TRUE){
        file.name.gif <- paste0(name,".gif")
        
        no.display <- video.dyad.gif.allgears(data.video=data.plot, file.name = file.name.gif, image.name = image.name, image.dir = png.dir, sleep = video.par$sleep, point.size = video.par$point.size, traza = video.par$traza)
        
        
      }
      
    }
    
    
    
    
    if (video.par$type == 'plot' || video.par$type == 'all'){
      
      image.name <- paste0("plot-id-",str_replace_all(bato1, fixed(" "), ""),'-',str_replace_all(bato2, fixed(" "), ""),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),"-",
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                           '-cl-',echant$clust_new_order)
      plot.dyad.allgears(data.video = data.plot.2, file.name = image.name, point.size = video.par$point.size, dossier.stats.outputs = paste0(dossier.stats.outputs,gear,"/"),line.width=3)
      
      
    }
    
    if (video.par$type == 'series' || video.par$type == 'all'){
      
      variable <- "DISTANCE"
      
      DO <- sqrt(((tr1$x - tr2$x)^2) + ((tr1$y - tr2$y)^2))/1000
      fechas <- tr1$date
      n.prox <- length(Prox_delta)
      if (is.null(colores)){
        colores=brewer.pal(n.prox, "Set1") # max 9
      }
      
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                           '-',
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                           '-cl-',echant$clust_new_order,
                           '-',variable)
      
      if (video.par$series_pdf == TRUE){
        pdf(paste0(dossier.stats.outputs,gear,'/',image.name,'.pdf'),width=10,height=7,pointsize=20)
        par(las=2)
        subplot.uni(fechas,DO,"DISTANCE - Prox")
        texte <- NULL
        for (prox in n.prox:1){
          points(fechas[which(DO <= Prox_delta[prox])],DO[which(DO <= Prox_delta[prox])],col=colores[prox],cex=1,pch=21,bg=colores[prox])
          texte <- c(texte,paste0("<",Prox_delta[prox], " (",round(sum(DO <= Prox_delta[prox])/length(fechas),2),")"))
        }
        legend("topleft",texte[n.prox:1],col=colores,pch=20,bty="n")
        dev.off()
      }
      
      if (video.par$series_gif == TRUE){
        file.name = paste0(image.name, ".gif")
        subplot.uni.DO.gif(image.name, file.name, fechas, variable = DO, n.prox, 
                           param_Prox_delta = Prox_delta, colores)
      }
      
      
      # DId and DItheta -------
      
      distance1 <- sqrt((diff(tr1$x))^2 + (diff(tr1$y))^2)
      distance2 <- sqrt((diff(tr2$x))^2 + (diff(tr2$y))^2)
      DId0 <- 1 - (abs(distance1-distance2)/(distance1+distance2))^DI_beta
      DId0[which(distance1+distance2 == 0)] <- 1
      DId0.d <- c(NA,DId0)
      DItheta <- cos(tr1$abs.angle - tr2$abs.angle)
      DItheta[which(is.na(tr1$abs.angle)== TRUE & is.na(tr2$abs.angle) == TRUE)] <- 1
      DItheta[which(is.na(DItheta))] <- 0
      
      variable <- "DId"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                           '-',
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                           '-cl-',echant$clust_new_order,
                           '-',variable)
      
      if (video.par$series_pdf == TRUE){
        pdf(paste0(dossier.stats.outputs,gear,'/',image.name,'.pdf'),width=10,height=7,pointsize=20)
        par(las=2)
        subplot.uni(fecha=fechas,variable=DId0.d,nombre="DId",cal=TRUE, rango = c(0,1))
        dev.off()
      }
      
      if (video.par$series_gif == TRUE){
        file.name = paste0(image.name, ".gif")
        subplot.uni.DI.gif(image.name, file.name, fechas, variable = DId0.d, nombre="DId", rango = c(0,1))
      }
      
      variable <- "DItheta"
      image.name =  paste0('VariableSeries-id-',gsub(" ", "-", bato1),'-',gsub(" ", "-", bato2),"-",
                           paste(unlist(strsplit(as.character(date(fecha.min)),"-")),collapse = ""),
                           '-',
                           paste(unlist(strsplit(as.character(date(fecha.max)),"-")),collapse = ""),
                           '-cl-',echant$clust_new_order,
                           '-',variable)
      
      if (video.par$series_pdf == TRUE){
        pdf(paste0(dossier.stats.outputs,gear,'/',image.name,'.pdf'),width=10,height=7,pointsize=20)
        par(las=2)
        subplot.uni(fecha=fechas,variable=DItheta,nombre="DItheta",cal=TRUE, rango = c(-1,1))
        dev.off()
      }
      if (video.par$series_gif == TRUE){
        file.name = paste0(image.name, ".gif")
        subplot.uni.DI.gif(image.name, file.name, fechas, variable = DItheta, nombre="DItheta", rango = c(-1,1))
      }
      
    }
    
    
  })
  
}