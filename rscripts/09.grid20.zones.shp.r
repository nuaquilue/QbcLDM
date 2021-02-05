rm(list=ls())

library(rgdal)
library(foreign)
library(RANN)
library(tidvyerse)

forest.data <- read.dbf("inputlyrs/dbf/exp_LDM_v3.dbf")
cell.size <- 2000
MASK <- rasterFromXYZ(data.frame(forest.data[,1:2], z=1), res=c(cell.size, cell.size), digits=0,
                      crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
                             +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Shearch and Remove duplicates
forest.data$XY <- paste0(forest.data$XCOO,"_",forest.data$YCOO)
a <- duplicated(forest.data$XY) 
forest.data <- forest.data[-which(a),] %>% select(-XY)

# Fill gaps 
dta <- data.frame(cell.id = 1:ncell(MASK), mask=MASK[], round(coordinates(MASK),0)) %>%
       left_join(forest.data, by=c("x"="XCOO", "y"="YCOO")) %>% filter(!is.na(mask))
## 8. Assign FRZ to those forest cells without FRZ informed
find.moda <- function(x){
  a <-  names(which.max(table(x)))
  a <- ifelse(is.null(a), NA, a)
  return(a)
}

zcells <- filter(dta,  is.na(FRZ))
r <- 3
while(nrow(zcells)>0){
  neighs <- nn2(select(dta, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
  values <- matrix(dta$FRZ[neighs$nn.idx], ncol=r^2)
  dta$FRZ[is.na(dta$FRZ)] <- apply(values, 1, find.moda)
  zcells <- filter(dta, is.na(FRZ))
  r <- r+2
}

dta$frz <- ifelse(dta$FRZ=="A", 1, ifelse(dta$FRZ=="B" | dta$FRZ=="C" | dta$FRZ=="D", 2,
           ifelse(dta$FRZ=="E" | dta$FRZ=="F" | dta$FRZ=="H" | dta$FRZ=="I" | dta$FRZ=="J", 3,
           ifelse(dta$FRZ=="G", 4, ifelse(dta$FRZ=="K", 5,  
           ifelse(dta$FRZ=="L" | dta$FRZ=="M", 6, ifelse(dta$FRZ=="Y", 7, ifelse(dta$FRZ=="Z", 8, NA))))))))
MAP <- MASK
MAP[!is.na(MASK[])] <- dta$frz
levelplot(MAP, margin=FALSE, colorkey=T, par.settings=magmaTheme())


## Raster to pol
frz.pol <- rasterToPolygons(MAP, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=T)
writeOGR(frz.pol, dsn="C:/WORK/QBCMOD/DataIn/ZonageFeux/2021.02.05", layer="Zones", driver="ESRI Shapefile")


## sh
shp01 <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones.shp")
grid.zone.area <- shp01@data %>% mutate(area=area(shp01), frz=z) %>% filter(UNIQUE>0, z>0) %>%
                  group_by(frz, UNIQUE) %>% summarize(area.km=sum(area)/10^6)
save(grid.zone.area, file="C:/WORK/QBCMOD/DataIn/SEP/GridZone.rdata")
