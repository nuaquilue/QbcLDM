rm(list=ls())
library(tidyverse)

## Function to select items not in a vector
`%notin%` <- Negate(`%in%`)

## Grid 20x20 in zones to know which grids are within each zone
load(file="C:/WORK/QBCMOD/DocsFires/rmarkdown/GridZone.rdata")
grid.zone.area <- filter(grid.zone.area, Zone!="Y" & Zone!="Z")

## Projected SEP for 1991-2020, 2041-2070, 2071-2100, 3 periods, 3 models   --> 1338 spatial units
sep <- read.table("C:/WORK/QBCMOD/DataIn/SEP/sep.txt", header=T)
length(unique(sep$UNIQUE))
# Which is missing? NONE
a <- which(grid.zone.area$UNIQUE %notin% sep$UNIQUE)

## Now combine grid.zone and sep.y to assign annual sep values to each zone 
zone.sep <- data.frame(Zone=NA, GCM=NA, RCP=NA, SEP0=NA, SEP1=NA, SEP2=NA, rSEP1=NA, rSEP2=NA)
for(mdl in unique(sep$GCM)){
  for(rcp in unique(sep$RCP)){
     sep0 <- filter(sep, GCM==mdl, RCP==rcp, Period=="1991-2020") %>% select(-SEP_SD)
     sep1 <- filter(sep, GCM==mdl, RCP==rcp, Period=="2041-2070") %>% mutate(SEP1=SEP_MEAN) %>% select(UNIQUE, SEP1)
     sep2 <- filter(sep, GCM==mdl, RCP==rcp, Period=="2071-2100") %>% mutate(SEP2=SEP_MEAN) %>% select(UNIQUE, SEP2)
     aux <- left_join(grid.zone.area, sep0, by="UNIQUE") %>% 
            left_join(sep1, by="UNIQUE") %>% left_join(sep2, by="UNIQUE") %>% 
            mutate(rSEP1=(SEP1)/SEP_MEAN, rSEP2=(SEP2)/SEP_MEAN)  %>%   #-SEP_MEAN
            group_by(Zone, GCM, RCP) %>% 
            summarise(SEP0=mean(SEP_MEAN), SEP1=mean(SEP1), SEP2=mean(SEP2), rSEP1=mean(rSEP1), rSEP2=mean(rSEP2))
             # weighting by area doesn't change anything
     zone.sep <- rbind(zone.sep, as.data.frame(aux))
  }
}
zone.sep <- filter(zone.sep, !is.na(Zone))
write.table(zone.sep, "C:/WORK/QBCMOD/QbcLDM/inputfiles/SEPzone.txt", quote=F, row.names=F, sep="\t")
