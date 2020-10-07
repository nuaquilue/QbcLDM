rm(list=ls())
library(raster)
library(rgdal)
library(tidyverse)
## Function to select items not in a vector
`%notin%` <- Negate(`%in%`)

################################################# BOULANGER #################################################

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





################################################# SEP #################################################

## Feux recents, years?
feuxr <- readOGR("C:/WORK/QBCMOD/DataIn/Feux/feux_recents.lcc.shp")
dta <- feuxr@data
table(dta$EXERCIC)  # 1976 to 2018

## Grid 20x20 to get UNIQUE in QbC
grid <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid_20x20km.shp")
grid.id <- grid@data  # 1339
names(grid.id)

## SEP data for 3 periods, 3 models, 3 rcp  --> 1338 spatial units
sep <- read.table("C:/WORK/QBCMOD/DataIn/SEP/sep.txt", header = T)
# Which is missing? the UNIQUE 14024 that corresponds to Lac StJean
a <- which(grid.id$UNIQUE %notin% sep$UNIQUE)

## Grid 20x20 in zones to get the true area of each grid within zones
grid.zone <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones.shp")
grid.area <- grid.zone@data %>% mutate(area=area(grid.zone)) %>%
             filter(UNIQUE>0) %>% filter(UNIQUE!=14024) %>% group_by(UNIQUE) %>%
             summarize(area.ha=sum(area)/10^4)
grid.zone.area <- grid.zone@data %>% mutate(area=area(grid.zone)) %>%
                  filter(UNIQUE>0) %>% filter(UNIQUE!=14024) %>% group_by(Group_1, UNIQUE) %>%
                  summarize(area.ha=sum(area)/10^4) %>% left_join(grid.area, by="UNIQUE") %>%
                  mutate(pctg=area.ha.x/area.ha.y)

## Feux recents in zones and Grid20x20
feux.zone.grid <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones_Feux.shp")
feux.zg <- feux.zone.grid@data %>% mutate(area=area(feux.zone.grid))
feux <- mutate(feux.zg, year=as.numeric(EXERCIC)+1975) %>% filter(year>=1991) %>% 
        filter(UNIQUE>0) %>% filter(UNIQUE!=14024) %>%
        group_by(Group_1, UNIQUE) %>% summarise(area.burnt=sum(area)/10^4)

## Join grid&zone, burnt.area and SEP dta
mdl <- "GCM4_ESM2_10km"
scn <- "rcp45"
sep.select <- filter(sep, GCM==mdl, RCP==scn, Period=="1991-2020")
dta <- left_join(grid.zone.area, feux, by=c("Group_1", "UNIQUE")) %>%
       left_join(sep.select, by="UNIQUE")
dta$area.burnt[is.na(dta$area.burnt)] <- 0
dta$pctg.burnt <- dta$area.burnt * dta$area.ha.y / dta$area.ha.x
save(dta, file="C:/WORK/QBCMOD/Docs/rmarkdown/burnt.grid.rdata")

## Relation area.burnt ~ SEP by zone
zone <- "B"
dta.zone <- filter(dta, Group_1==zone)
grid.noburnt <- filter(dta.zone, area.burnt==0)
grid.burnt <- filter(dta.zone, area.burnt!=0)
par(mfrow=c(1,3))
plot(dta.zone$SEP_MEAN, dta.zone$area.burnt)
hist(grid.noburnt$SEP_MEAN)
hist(grid.burnt$SEP_MEAN)
rel <- lm(pctg.burnt ~ SEP_MEAN, data=dta.zone)
summary(rel)


#### BUILD THE SEP TREND PER FIRE REGIME ZONE
head(sep)
zone.area <- group_by(grid.zone.area, Group_1) %>% summarise(tot=sum(area.ha.x))
zone.sep <- left_join(grid.zone.area, sep, by=c("UNIQUE")) %>% 
            mutate(sep=SEP_MEAN*area.ha.x) %>%
            group_by(Group_1, GCM, RCP, Period) %>%
            summarize(sepz=sum(sep)) %>% left_join(zone.area, by="Group_1") %>%
            mutate(sep=sepz/tot) %>% filter(Group_1!="Y" & Group_1!="Z")
zone.sep$Period <- ifelse(zone.sep$Period=="1991-2020", 2020,
                          ifelse(zone.sep$Period=="2041-2070", 2070, 2100))

ggplot(zone.sep, aes(Period, sep, colour=RCP, shape=GCM)) + 
  geom_point() +  facet_wrap(.~Group_1) + theme_classic()


## Interpolate SEP values each 5 years
## From 2020 to 2070
aux2020 <- filter(zone.sep, Period==2020)
aux2070 <- filter(zone.sep, Period==2070)
sep.inc <- select(aux2020, Group_1, GCM, RCP, sep) %>% 
            left_join(select(aux2070, Group_1, GCM, RCP, sep), by=c("Group_1", "GCM", "RCP" )) %>%
            mutate(x=(sep.y-sep.x)/(2070-2020))
zone.sep.5y <- select(zone.sep, -sepz, -tot)
for(y in seq(5, 45,5)){
  aux <- select(sep.inc, -sep.y) %>% mutate(Period=y+2020, sep=sep.x+x*y) %>%
         select(-sep.x, -x)
  zone.sep.5y <- rbind(zone.sep.5y, aux)
}
## From 2070 to 2100
aux2100 <- filter(zone.sep, Period==2100)
sep.inc <- select(aux2070, Group_1, GCM, RCP, sep) %>% 
          left_join(select(aux2100, Group_1, GCM, RCP, sep), by=c("Group_1", "GCM", "RCP" )) %>%
          mutate(x=(sep.y-sep.x)/(2100-2070))
for(y in seq(5, 25,5)){
  aux <- select(sep.inc, -sep.y) %>% mutate(Period=y+2070, sep=sep.x+x*y) %>%
    select(-sep.x, -x)
  zone.sep.5y <- rbind(zone.sep.5y, aux)
}
## Plot SEP from 2020 to 2100 by 5y for the 3 RCP and the 3 GCM
ggplot(zone.sep.5y, aes(Period, sep, colour=RCP)) + #geom_point() +
  geom_line(aes(lty=GCM), size=1.2) +  facet_wrap(.~Group_1) + theme_classic() +
  scale_color_viridis_d() 

## Increment
zone.sep.base <- filter(zone.sep.5y, Period==2020) %>% select(-Period)
zone.sep.inc <- filter(zone.sep.5y, Period!=2020) %>%
                left_join(zone.sep.base, by=c("Group_1", "GCM", "RCP")) %>%
                mutate(inc=sep.x/sep.y)
ggplot(zone.sep.inc, aes(Period, inc, colour=RCP)) + 
  geom_line(aes(lty=GCM), size=1.2) +  facet_wrap(.~Group_1) + theme_classic() +
  scale_color_viridis_d() 
save(zone.sep.inc, file="C:/WORK/QBCMOD/Docs/rmarkdown/zone.sep.inc.rdata")





  