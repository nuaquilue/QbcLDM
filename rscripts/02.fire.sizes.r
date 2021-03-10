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



################### GENERATE FIRE SIZE DISTRIBUTIONS ###################
load(file="inputlyrs/rdata/land.rdata")
fire.regime <-  read.table("inputfiles/FireRegime_Z6.txt", header=T)
fire.sizes <- read.table("inputfiles/FireSizes_Z6.txt", header=T)
sep.zone <- read.table("inputfiles/SEPzone_Z6.txt", header=T)
km2.pixel <- 4
time.step <- 5
time.horizon <- 2100-2020
time.seq <- seq(time.step, time.horizon, time.step) 
# clim.scn <- NA
clim.scn <- "rcp85"
nrun <- 100
result <- data.frame(run=NA, year=NA, frz=NA, target.zone=NA, fire.id=NA, target.fire=NA)
for(irun in 1:nrun){
  for(t in time.seq){
    for(izone in paste0("Z", sample(1:6, 6, replace=FALSE))){
      
      ## Determine target area per fire zone
      baseline.area <- time.step*sum(land$frz==izone)/fire.regime$fri[fire.regime$frz==izone]
      zone.target.area <- rnorm(1, unlist(baseline.area), unlist(baseline.area)*0.1)  
      if(!is.na(clim.scn)){
        sep.zone.cc <- filter(sep.zone, GCM=="MIROC_ESM_CHEM_10km", RCP==clim.scn)
        # if(t<=20) # 2020 to 2040
        #   aux.track$brclima <- aux.track$brvar*1
        if(t>20 & t<=50){ # 2040 to 2070
          zone.target.area <- zone.target.area*sep.zone.cc$rSEP1[sep.zone.cc$frz==izone]
          # aux.track$brclima <- aux.track$brvar*sep.zone.cc$rSEP1[sep.zone.cc$frz==izone]
        }
        if(t>50){ # 2070 to 2100
          zone.target.area <- zone.target.area*sep.zone.cc$rSEP2[sep.zone.cc$frz==izone]
          # aux.track$brclima <- aux.track$brvar*sep.zone.cc$rSEP2[sep.zone.cc$frz==izone]
        }
      }
      ## Round it to have entire cells (in km2)
      zone.target.size <- round(zone.target.area*km2.pixel)
      
      ## Determine target area per fire
      fs.dist <- filter(fire.sizes, frz==izone) %>% select(-frz)
      area.burnt <- 0
      fire.id <- 0
      while(area.burnt < zone.target.size){   ## condition in pixels
        ## ID for each fire event
        fire.id <- fire.id+1
        ## Determine potential area of the fires (i.e. the target area) in km2
        ## Fire size is drawn from an discrete distribution defined in classes of 50 km2 
        fire.class <- sample(1:nrow(fs.dist), 1, replace=F, p=fs.dist$p) 
        fire.target.size <- min(rdunif(1, fs.dist$lower[fire.class], fs.dist$upper[fire.class]), 
                                  fire.regime$max[fire.regime$zone==izone])
        area.burnt <- area.burnt+fire.target.size
        result <- rbind(result, data.frame(run=irun, year=t+2020, frz=izone, target.zone=zone.target.size,
                                           fire.id=fire.id, target.fire=fire.target.size))
      }
    }
  }
}
result <- result[-1,]

## plot histo
ggplot(data=result, aes((target.fire))) + geom_histogram(bins=20) + 
  facet_wrap(~frz,  scales="free_y") + theme_classic() 

## Count number of fires per each size class
distrib <- data.frame(frz=NA, run=NA, X1=NA, X2=NA, X3=NA, X4=NA, X5=NA, X6=NA)
for(izone in paste0("Z", sample(1:6, 6, replace=FALSE))){
  for(irun in 1:nrun){
    a <- filter(result, frz==izone, run==irun) 
    count <- cut(a$target.fire, breaks=c(0,50,100,200,500,1000,2000))
    distrib <- rbind(distrib, data.frame(frz=izone, run=irun, t(as.numeric(table(count)))))
  }
}
distrib <- distrib[-1,]
a <- group_by(distrib, frz) %>% summarise(x50=mean(X1), x100=mean(X2), x200=mean(X3), 
     x500=mean(X4), x1000=mean(X5), x2000=mean(X6)) %>% mutate(tot=x50+x100+x200+x500+x1000+x2000); a
b <- a 
b[,c(2:7)] <- round(100*b[,c(2:7)]/b$tot,1) 
select(b, -tot)


## number fires / zone area
scn <- "TestFires"
zone.area <- read.table(paste0("outputs/", scn, "/SppByFireZone.txt"), header=T) %>% 
  filter(run==1,year==2020) %>% group_by(frz) %>% summarize(area.zone=sum(area)); zone.area
select(a, frz, tot) %>% left_join(zone.area, by="frz") %>% mutate(n=100*tot/area.zone)
