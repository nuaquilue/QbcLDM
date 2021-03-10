build.pigni <- function(work.path, lambda=0, first.time=F){
  
  library(rgdal)
  
  ## Load land df
  load(file="inputlyrs/rdata/land.rdata")
  
  ## Build a spatial points layer with coordinates of the forest data
  points <- SpatialPoints(land[land$spp!="NonFor",2:3],  
                          CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                               +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    
  ## Read buffers around the observed ignitions 
  buff <- data.frame(points)
  for(i in c("05", 10, 20, 30)){   
    if(first.time){
      BUFF <- readOGR(paste0("C:/WORK/QBCMOD/DataIn/Buffers/Buff", i, "Klight.shp"))  #Kall
      # Change cartographic projection
      BUFFp <- spTransform(BUFF, CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                                +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
      # Overlap point and polygons
      aux <- over(points, BUFFp)
      save(aux, file=paste0("C:/WORK/QBCMOD/DataIn/Buffers/OverlappLandBuff_", i, "Klight.rdata")) #Kall
    }
    else
      load(paste0("C:/WORK/QBCMOD/DataIn/Buffers/OverlappLandBuff_", i, "Klight.rdata"))
    buff$z <- !is.na(aux$Id) #!is.na(aux$BUFF_DIST)
    buff$z <- buff$z*as.numeric(i)
    names(buff)[ncol(buff)] <- paste0("r", i)
  }
  ## We assume a default distance to any ignition point of 40 km
  buff$r40 <- 40
  ## Replace 0 by NA
  for(i in 3:6)
    buff[,i] <- ifelse(buff[,i]==0, NA, buff[,i])
  
  ## Compute the minimum distance to a focal, and then assign prob igni
  pigni <- data.frame(cell.id=land$cell.id[land$spp!="NonFor"], frz=land$frz[land$spp!="NonFor"],
                      d=pmin(buff$r05, buff$r10, buff$r20, buff$r30, buff$r40, na.rm=T))
  
  ## Assign probability of ignition p=exp(-lamda* d), and save it
  pigni$p <- exp(-lambda*pigni$d)
  save(pigni, file="inputlyrs/rdata/pigni_light_static.exp.rdata")
  
  ## Assign probability of ignition p=1/(1+exp(d)), and save it
  pigni$p <- 1/(1+exp(scales::rescale(pigni$d, to=c(-3, 2), from=c(0,40))))
  save(pigni, file="inputlyrs/rdata/pigni_light_static.nexp.rdata")
  
}

