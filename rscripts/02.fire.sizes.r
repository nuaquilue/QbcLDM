rm(list=ls())
suppressPackageStartupMessages({
  library(foreign)
  library(rgdal)
  library(raster)
  library(tidyverse)
})

## Forest Inventory plots from shape file
nfi <- readOGR("C:/WORK/QBCMOD/QbcLDM/inputlyrs/dbf/points_2k2_ll.shp")
nfi; crs(nfi)
  # +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
coord.nfi <- coordinates(nfi)
names(coord.nfi) <- c("long", "lat")

## Forest inventory plots form dbf (in Lambert Conformal Conic)
forest.data <- read.dbf("C:/WORK/QBCMOD/QbcLDM/inputlyrs/dbf/points_2k2_ll.dbf")
forest.data$X_COORD <- round(forest.data$X_COORD, 0)
forest.data$Y_COORD <- round(forest.data$Y_COORD, 0)  
forest.data <- cbind(forest.data, coord.nfi)
    # crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
    #                 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")


## Fires larger 200 ha (in Albers Equal Area Conic)
fires <- readOGR("C:/WORK/QBCMOD/DataIn/NFDB_poly_large_fires/NFDB_poly_20190607_large_fires.shp")
fires; crs(fires)
    # CRS arguments:
    #   +proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs
    # +ellps=GRS80 +towgs84=0,0,0

## Reproject fires to Lambert Conformal Conic
fires.lcc <- spTransform(fires, crs("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                                     +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

## Reproject raster of Fire regime zones to Albers Equal Area
zones <- raster("C:/WORK/QBCMOD/QbcLDM/inputlyrs/asc/FRZone.asc")
crs(zones)  <- crs("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                                     +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
zones.aea <- projectRaster(zones, res=2000, crs=crs(fires))
zones.aea[] <- round(zones.aea[])
writeRaster(zones.aea, "C:/WORK/QBCMOD/QbcLDM/inputlyrs/asc/FRZone_aea.asc", format="ascii", overwrite=T)


## Overlap fire perimeters and fire regime zones (agencies in Québec province: QC and PC-LM)
v <- raster::extract(zones, fires.lcc) 
major <- function(x){
  y <- ifelse(length(x)==0, NA, median(x, na.rm=T))
  return(y)
}
a <- unlist(lapply(v, major))
fires.zone <- data.frame(reg=fires$SRC_AGENCY, id=fires$FIRE_ID, year=fires$YEAR,
                         area=fires$SIZE_HA, zone=a) %>% filter(!is.na(zone))
table(fires.zone$zone)
table(fires.zone$reg)
table(fires.zone$year)

year <- 1970:2019
lustrum <- data.frame(year=as.factor(year), lust=rep(year[seq(1, 50, 5)], each=5))
zone.lust <- filter(fires.zone, zone==1) %>% left_join(lustrum) %>%
            filter(lust!=1970) %>%
            group_by(lust) %>% summarize(n=length(year))
zone.lust
par(mfrow=c(1,2))
hist(zone.lust$n); hist(log(zone.lust$n))
zone.year <- filter(fires.zone, zone==1) %>% group_by(year) %>% summarize(n=length(year))
hist(zone.year$n); hist(log(zone.year$n))


library(MASS); library(fitdistrplus)
fit_ln <- fitdist(log(zone.lust$n),"lnorm")
fit_wb <- fitdist(log(zone.lust$n),"weibull")
fit_gm <- fitdist(log(zone.lust$n),"gamma")
summary(fit_ln); summary(fit_wb); summary(fit_gm)
gofstat(list(fit_ln, fit_wb, fit_gm))

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_wb, fit_gm, fit_ln), legendtext = plot.legend)
