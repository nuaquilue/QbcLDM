rm(list=ls())
library(raster)
library(rgdal)
library(tidyverse)

## Change cartographic projection of  "zones"
hfrqbc <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/Boulanger/HFR_Qbc.shp")
zones <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/2020.05.25/zones_nuria.shp")
zones.lcc <- spTransform(zones, crs(hfrqbc))
writeOGR(zones.lcc, dsn="C:/WORK/QBCMOD/DataIn/ZonageFeux/2020.05.25", 
         layer="zones.lcc", driver="ESRI Shapefile")

## Read the layer of hfrqbc and zones merged
merge <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/MergeZones.shp")
dta <- merge@data
dta <- dplyr::select(dta, Group_1, NAMES_SHOR)
names(dta) <- c("zone", "code")
dta$area <- area(merge)

## Table with % AAB increase each 30y per HFR
predict.aab <- read.table("C:/WORK/QBCMOD/DataIn/ZonageFeux/Boulanger/PredictedAAB.txt", header=T)
res <- filter(dta, !is.na(code)) %>% left_join(predict.aab, by="code") %>%
       mutate(w40=X2040*area, w70=X2070*area, w100=X2100*area) %>%
       group_by(zone) %>% summarize(tot=sum(area), p40=sum(w40)/tot, p70=sum(w70)/tot, p100=sum(w100)/tot ) %>%
       select(-tot)
res

## Table with % AAB observed
obs.aab <- read.table("C:/WORK/QBCMOD/DataIn/ZonageFeux/Boulanger/ObservedAAB.txt", header=T)
res <- filter(dta, !is.na(code)) %>% filter(zone != "Agro-forestier") %>% filter(zone != "Agro-urbain") %>%
  left_join(obs.aab, by="code") %>% mutate(ab=area*PctgAAB/100, abkm2=ab/10^6) %>%
  group_by(zone) %>% summarize(tot=round(sum(abkm2)))
res





kk <- read.dbf("C:/WORK/QBCMOD/DataIn/ZonageFeux/Boulanger/FIRE_NewProjections/FIRE_RCP4.5_CanESM2_2011_2040_mean.dbf")
kks <- read.dbf("C:/WORK/QBCMOD/DataIn/ZonageFeux/Boulanger/FIRE_NewProjections/FIRE__1981_2010_mean.dbf")
