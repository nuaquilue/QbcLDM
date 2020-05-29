build.pigni <- function(work.path, lambda=0){
  
  ## Build a spatial points layer with coordinates of the forest data
  load(file="inputlyrs/rdata/land.rdata")
  points <- SpatialPoints(land[,2:3],  CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  ## Read buffers around the observed ignitions (of fires of size > 200 ha) and 
  buff <- data.frame(points)
  for(i in c("05", 10, 20, 30, 40)){ 
    BUFF <- readOGR(paste0(work.path, "DataIn/Buffers/Buff",i,"K.shp"))
    BUFFp <- spTransform(BUFF, CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                                  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    buff$z <- over(points, BUFFp)
    names(buff)[ncol(buff)] <- paste0("r", i)
  }
  buff[3] <- buff[3] + 5
  buff[4] <- buff[4] + 10
  buff[5] <- buff[5] + 20
  buff[6] <- buff[6] + 30
  buff[7] <- buff[7] + 40
  
  ## Compute the minimum distance to a focal, and then assign prob igni
  pigni <- data.frame(cell.id=land$cell.id, frz=land$FRZone,
                      d=pmin(buff$r05, buff$r10, buff$r20, buff$r30, buff$r40, na.rm=T))
  pigni$p <- exp(-lambda*pigni$d)
  pigni$p[is.na(pigni$p)] <- 0
  
  ## Save the prob igni dataframe
  save(pigni, file=("inputlyrs/rdata/pigni.rdata"))
}
