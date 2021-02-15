rm(list = ls())

library(foreign)
library(RANN)
library(sp)
library(raster)
library(tidyverse)


## 1. Read data from the old dbf 
forest.data.old <- read.dbf("inputlyrs/dbf/points_2k2_ll.dbf")
forest.data.old$X_COORD <- round(forest.data.old$X_COORD, 0)
forest.data.old$Y_COORD <- round(forest.data.old$Y_COORD, 0)  

## 2. Build a Raster object from the X and Y coordinates and a dummy variable Z=1 
## Fix first cell size (in m)
## Note that the points.shp is +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
## But the coordinates of the dbf is in the following projection:
# crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
#                   +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
cell.size <- 2000 # in m
MASK.OLD <- rasterFromXYZ(data.frame(forest.data.old[,2:3], z=forest.data.old$UNIQUE_ID), 
                          res=c(cell.size, cell.size), digits=0,
                          crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
                             +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
MASK.OLD
plot(MASK.OLD)

## 3. Build a data frame with cell.id, mask values (0, 188.872) and raster coordinates 
dta.old <- data.frame(cell.indx = 1:ncell(MASK.OLD), UNIQUE_ID=MASK.OLD[], round(coordinates(MASK.OLD),0))



## 1. Read data from the new dbf 
forest.data.new <- read.dbf("inputlyrs/dbf/exp_LDM_v3.dbf")
## 2. Build raster
MASK.NEW <- rasterFromXYZ(data.frame(forest.data.new[,1:2], z=1), res=c(cell.size, cell.size), digits=0,
                      crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
                             +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
MASK.NEW
plot(MASK.NEW)
## 3. Build data frame
dta.new <- data.frame(cell.id = 1:ncell(MASK.NEW), mask=MASK.NEW[], round(coordinates(MASK.NEW),0)) %>% 
           filter(!is.na(mask))


## Join them, maintain cell.indx and cell.id
yupi <- left_join(select(dta.new, -mask), dta.old, by=c("x", "y")) 


## Climatic data
rcp <- 85
for(rcp in c(45,85)){
  load(paste0("C:/work/qbcmod/datain/Clim/sc_17_", rcp,".RData"))
  # precipitation
  aux <- df_clim[, c(1,25:40)]
  names(aux)[-1] <- paste0("prec", seq(20,95,5))
  cc.prec <- left_join(yupi, aux, by="UNIQUE_ID") %>% select(-cell.indx, - UNIQUE_ID)
  # temperature
  aux <- df_clim[, c(1,64:79)]
  names(aux)[-1] <- paste0("temp", seq(20,95,5))
  cc.temp <- left_join(yupi, aux, by="UNIQUE_ID") %>% select(-cell.indx, - UNIQUE_ID)
  # clean NAs  
  zcells <- filter(cc.prec, is.na(prec20))
  r <- 3
  while(nrow(zcells)>0){
    neighs <- nn2(select(yupi, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
    # prec
    values <- matrix(cc.prec$prec20[neighs$nn.idx], ncol=r^2)
    cc.prec$prec20[is.na(cc.prec$prec20)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec25[neighs$nn.idx], ncol=r^2)
    cc.prec$prec25[is.na(cc.prec$prec25)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec30[neighs$nn.idx], ncol=r^2)
    cc.prec$prec30[is.na(cc.prec$prec30)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec35[neighs$nn.idx], ncol=r^2)
    cc.prec$prec35[is.na(cc.prec$prec35)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec40[neighs$nn.idx], ncol=r^2)
    cc.prec$prec40[is.na(cc.prec$prec40)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec45[neighs$nn.idx], ncol=r^2)
    cc.prec$prec45[is.na(cc.prec$prec45)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec50[neighs$nn.idx], ncol=r^2)
    cc.prec$prec50[is.na(cc.prec$prec50)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec55[neighs$nn.idx], ncol=r^2)
    cc.prec$prec55[is.na(cc.prec$prec55)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec60[neighs$nn.idx], ncol=r^2)
    cc.prec$prec60[is.na(cc.prec$prec60)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec65[neighs$nn.idx], ncol=r^2)
    cc.prec$prec65[is.na(cc.prec$prec65)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec70[neighs$nn.idx], ncol=r^2)
    cc.prec$prec70[is.na(cc.prec$prec70)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec75[neighs$nn.idx], ncol=r^2)
    cc.prec$prec75[is.na(cc.prec$prec75)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec80[neighs$nn.idx], ncol=r^2)
    cc.prec$prec80[is.na(cc.prec$prec80)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec85[neighs$nn.idx], ncol=r^2)
    cc.prec$prec85[is.na(cc.prec$prec85)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec90[neighs$nn.idx], ncol=r^2)
    cc.prec$prec90[is.na(cc.prec$prec90)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.prec$prec95[neighs$nn.idx], ncol=r^2)
    cc.prec$prec95[is.na(cc.prec$prec95)] <- apply(values, 1, mean, na.rm=T)
    # temp
    values <- matrix(cc.temp$temp20[neighs$nn.idx], ncol=r^2)
    cc.temp$temp20[is.na(cc.temp$temp20)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp25[neighs$nn.idx], ncol=r^2)
    cc.temp$temp25[is.na(cc.temp$temp25)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp30[neighs$nn.idx], ncol=r^2)
    cc.temp$temp30[is.na(cc.temp$temp30)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp35[neighs$nn.idx], ncol=r^2)
    cc.temp$temp35[is.na(cc.temp$temp35)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp40[neighs$nn.idx], ncol=r^2)
    cc.temp$temp40[is.na(cc.temp$temp40)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp45[neighs$nn.idx], ncol=r^2)
    cc.temp$temp45[is.na(cc.temp$temp45)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp50[neighs$nn.idx], ncol=r^2)
    cc.temp$temp50[is.na(cc.temp$temp50)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp55[neighs$nn.idx], ncol=r^2)
    cc.temp$temp55[is.na(cc.temp$temp55)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp60[neighs$nn.idx], ncol=r^2)
    cc.temp$temp60[is.na(cc.temp$temp60)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp65[neighs$nn.idx], ncol=r^2)
    cc.temp$temp65[is.na(cc.temp$temp65)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp70[neighs$nn.idx], ncol=r^2)
    cc.temp$temp70[is.na(cc.temp$temp70)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp75[neighs$nn.idx], ncol=r^2)
    cc.temp$temp75[is.na(cc.temp$temp75)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp80[neighs$nn.idx], ncol=r^2)
    cc.temp$temp80[is.na(cc.temp$temp80)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp85[neighs$nn.idx], ncol=r^2)
    cc.temp$temp85[is.na(cc.temp$temp85)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp90[neighs$nn.idx], ncol=r^2)
    cc.temp$temp90[is.na(cc.temp$temp90)] <- apply(values, 1, mean, na.rm=T)
    values <- matrix(cc.temp$temp95[neighs$nn.idx], ncol=r^2)
    cc.temp$temp95[is.na(cc.temp$temp95)] <- apply(values, 1, mean, na.rm=T)
    # new NAs
    zcells <- filter(cc.prec, is.na(prec20))
    r <- r+2
  }
  
  save(cc.prec, file=paste0("inputlyrs/rdata/prec_rcp", rcp, "_MIROC_ESM_CHEM.rdata")) 
  save(cc.temp, file=paste0("inputlyrs/rdata/temp_rcp", rcp, "_MIROC_ESM_CHEM.rdata"))
}


