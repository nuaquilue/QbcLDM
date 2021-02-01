################################################# ANNUAL SEP #################################################
rm(list=ls())
library(xlsx)
library(viridis)
library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)

## Function to select items not in a vector
`%notin%` <- Negate(`%in%`)


## Zones
zones <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/2020.11.27/zones_nuria2.shp")
dta.zones <- zones@data %>% mutate(area=area(zones), area.km=area(zones)/10^6) # in m2 and km2

## Grid 20x20 
grid <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid_20x20km.shp")
dta.grid <- grid@data %>% mutate(area=area(grid), area.grid.km=area(grid)/10^6) # in m2 and km2

## Grid 20x20 in zones to know which grids are within each zone
zone.grid <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones.shp")
dta.zone.grid <- zone.grid@data %>% mutate(area=area(zone.grid), Zone=ZONE201120) %>% 
  select(-ZONE201120, -NIV2, -FID_zones_, -HOMO_REGR) %>% filter(UNIQUE>0 & !is.na(Zone)) %>% 
  group_by(Zone, UNIQUE) %>% summarize(area.km=sum(area)/10^6) %>% 
  left_join(dta.grid, by="UNIQUE") %>% mutate(pct=area.km/area.grid.km)


## Annual SEP data from 1994 to 2018  --> 1338 spatial units
sep <- read.xlsx("C:/WORK/QBCMOD/DataIn/SEP/SEP_Observed Historic 1994-2018_by Grid cell.xlsx", sheetName="Feuil1")
# Which is missing? NONE
a <- which(dta.zone.grid$UNIQUE %notin% sep$UNIQUE)
## Convert into a data frame with UNIQUE, YEAR and SEP values
sep <- select(sep, -OID, - PROVINCE, -FISH_AREA, -Centroid_X, -Centroid_Y)
names(sep)[-1] <- as.numeric(substring(names(sep)[-1], 5,9))
sep.y <- pivot_longer(sep, 2:ncol(sep))
sep.y$name <- as.numeric(sep.y$name)
names(sep.y) <- c("UNIQUE", "Year", "SEP")

## Now combine grid.zone and sep.y to assign annual sep values to each zone 
zone.sep <- left_join(dta.zone.grid, sep.y, by="UNIQUE")  %>% group_by(Zone, Year) %>% 
  summarise(SEP=mean(SEP), tot=sum(pct), SEPw=sum(SEP*pct)/tot)
zone.sep$SEP-zone.sep$SEPw    ## weighting by area doesn't change almost anything
zone.sep$SEP <- zone.sep$SEPw ## still I choose weighted SEP, it is methodologically more correct
zone.sep <- select(zone.sep, -tot, -SEPw)  
    
## Plot of annual SEP per FireZone
zone.sep.noYZ <- filter(zone.sep, Zone!="Y" & Zone!="Z")
mypalette <- c("red3", "yellowgreen","purple3", "orange",  "yellow2",  "dodgerblue3", "darkred",  
                "palevioletred2",  "magenta2", "darkgreen", "gold3",  "navy",  "grey50")
jpeg("C:/WORK/QBCMOD/DataOut/SEP_FireZone_Obs.jpg", width=700, height=700)
ggplot(zone.sep.noYZ, aes(x=Year, y=SEP, colour=Zone)) + geom_line(cex=2) + #facet_wrap(~Zone, scales="free")
  facet_wrap(~Zone) + theme_classic() +  theme(text = element_text(size=20)) +
  scale_color_manual(values=mypalette)
dev.off()
 

## SEP data for 3 periods, 3 models, 3 rcp  --> 1338 spatial units
future.sep <- read.table("C:/WORK/QBCMOD/DataIn/SEP/sep.txt", header = T)

## Join estimated SEP to zone.grid data frame, aggregate at the level of Zone, GCM, RCP and Period
## to get a SEP value.
zone.future.sep <- left_join(dta.zone.grid, future.sep, by="UNIQUE") %>% 
  filter(!is.na(GCM), !is.na(RCP), !is.na(Period)) %>% group_by(Zone, GCM, RCP, Period) %>%
  summarise(SEP=mean(SEP_MEAN), tot=sum(pct), SEPw=sum(SEP_MEAN*pct)/tot) %>% 
  filter(Zone!="Y" & Zone!="Z")
zone.future.sep$SEP <- zone.future.sep$SEPw ## still I choose weighted SEP, it is methodologically more correct
zone.future.sep <- select(zone.future.sep, -tot, -SEPw)  

## Now do linear interpolation of SEP values to have a value every year
zone.future.sep$Year <- ifelse(zone.future.sep$Period=="1991-2020", 2020,
                          ifelse(zone.future.sep$Period=="2041-2070", 2070, 2100))

## Annual interpolation from 2020 to 2070
aux2020 <- filter(zone.future.sep, Year==2020) %>% select(-Period, -Year)
aux2070 <- filter(zone.future.sep, Year==2070) %>% select(-Period, -Year)
sep.inc <- left_join(aux2020, aux2070,  by=c("Zone", "GCM", "RCP")) %>%
           mutate(delta=(SEP.y-SEP.x)/(2070-2020))
zone.interpol.sep <- select(zone.future.sep, -Period)
for(y in seq(1, 49,1)){
  aux <- mutate(sep.inc, SEP=SEP.x+delta*y, Year=y+2020, SEP=SEP.x+delta*y) %>%
         select(-SEP.x, -SEP.y, -delta)
  zone.interpol.sep <- rbind(zone.interpol.sep, aux)
}

## Annual interpolation from 2070 to 2100
aux2100 <- filter(zone.future.sep, Year==2100) %>% select(-Period, -Year)
sep.inc <- left_join(aux2070, aux2100,  by=c("Zone", "GCM", "RCP")) %>%
           mutate(delta=(SEP.y-SEP.x)/(2100-2070))
for(y in seq(1, 29, 1)){
  aux <- mutate(sep.inc, SEP=SEP.x+delta*y, Year=y+2070, SEP=SEP.x+delta*y) %>%
    select(-SEP.x, -SEP.y, -delta)
  zone.interpol.sep <- rbind(zone.interpol.sep, aux)
}

## Plot SEP from 2020 to 2100 by 5y for the 3 RCP and the 3 GCM
jpeg("C:/WORK/QBCMOD/DataOut/SEP_FireZone_Future.jpg", width=700, height=700)
ggplot(zone.interpol.sep, aes(Year, SEP, colour=RCP)) + #geom_point() +
  geom_line(aes(lty=GCM), size=1.2) +  facet_wrap(.~Zone) + theme_classic() +
  theme(text = element_text(size=20)) + scale_color_viridis_d()   #, legend.position="bottom"
dev.off()



