################################################# ANNUAL SEP #################################################
rm(list=ls())
library(xlsx)
library(viridis)
library(raster)
library(rgdal)
library(tidyverse)

## Function to select items not in a vector
`%notin%` <- Negate(`%in%`)

## Zones, just to check areas
zones <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/2020.06.02/zones_nuria_Area.shp")
dta.zones <- zones@data %>% mutate(area=area(zones), area.km=area/10^6)
zones.lcc <- readOGR("C:/WORK/QBCMOD/DataIn/ZonageFeux/2020.06.02/zones.lcc.shp")
dta.zones.lcc <- zones.lcc@data %>% mutate(area=area(zones.lcc), area.km=area/10^6)
diff <- data.frame(zone=dta.zones$Group_1, x=dta.zones$area.km-dta.zones.lcc$area.km)
diff$r <- round(100*diff$x/dta.zones$area.km,1)
diff


## Feux recents, years?
feuxr <- readOGR("C:/WORK/QBCMOD/DataIn/Feux/feux_recents.shp")
dta.feuxr <- feuxr@data %>% mutate(area=area(feuxr))
dta.feuxr$ID <- 0:(nrow(dta.feuxr)-1)
feuxr.lcc <- readOGR("C:/WORK/QBCMOD/DataIn/Feux/feux_recents.lcc.shp")
dta.feuxr.lcc <- feuxr.lcc@data %>% mutate(area=area(feuxr.lcc))
dta.feuxr.lcc$ID <- 0:(nrow(dta.feuxr.lcc)-1)
filter(dta.feuxr, ID==45456)
filter(dta.feuxr.lcc, ID==45456)
table(dta.feuxr$EXERCIC)  # 1976 to 2018


## Feux recents in zones and Grid20x20  --> annual burnt area per zone
shp01 <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones_Feux.shp")
feux.zone.grid <- shp01@data %>% mutate(area=area(shp01), Zone=Group_1) %>% select(-Group_1) 
save(feux.zone.grid, file="C:/WORK/QBCMOD/DocsFires/rmarkdown/FeuxZone.rdata")
## True area of fires (and number of fires per zone)  --> 57624
feux <- group_by(feux.zone.grid, EXERCIC, FID_feux_r) %>% 
  summarize(SUPERFI=mean(SUPERFI), area=round(sum(area)/10^4,1))
## When also grouping by Zone, some fires are split into pieces  --> 57941
feux.zone <- group_by(feux.zone.grid, EXERCIC, Zone, FID_feux_r) %>% 
  summarize(SUPERFI=mean(SUPERFI), area=round(sum(area)/10^4,1))
## Compute per zone and year, burnt area and number of fires
feux.zone.year <- mutate(feux.zone.grid, Year=as.numeric(EXERCIC)+1975) %>% filter(Year>=1994) %>% 
                  filter(area>=2*10^6) %>% 
                  group_by(Zone, Year) %>% summarise(area.burnt.km=sum(area)/10^6,
                  num.fire=length(unique(FID_feux_r))) %>% filter(Zone!="Y" & Zone!="Z")
## Annual burnt area in km2 and number of fires
ggplot(feux.zone.year, aes(x=Year, y=area.burnt.km, colour=Zone)) + geom_line(cex=2) + 
  facet_wrap(~Zone, scales="free") +  theme_bw() + scale_colour_brewer(palette="Set1")
ggplot(feux.zone.year, aes(x=Year, y=num.fire, colour=Zone)) + geom_line(cex=2) + 
  facet_wrap(~Zone, scales="free") +  theme_bw() + scale_colour_brewer(palette="Set1")


## Grid 20x20 in zones to know which grids are within each zone
shp02 <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones_Areas.shp")
grid.zone.area <- shp02@data %>% mutate(area=area(shp02), Zone=Group_1) %>% select(-Group_1) %>% 
                  filter(UNIQUE>0) %>% group_by(Zone, UNIQUE) %>% summarize(area.km=sum(area)/10^6)
save(grid.zone.area, file="C:/WORK/QBCMOD/DocsFires/rmarkdown/GridZone.rdata")


## Annual SEP data from 1994 to 2018  --> 1338 spatial units
sep <- read.xlsx("C:/WORK/QBCMOD/DataIn/SEP/SEP_Observed Historic 1994-2018_by Grid cell.xlsx", sheetName="Feuil1")
# Which is missing? NONE
a <- which(grid.zone.area$UNIQUE %notin% sep$UNIQUE)
## Convert into a data frame with UNIQUE, YEAR and SEP values
sep <- select(sep, -OID, - PROVINCE, -FISH_AREA, -Centroid_X, -Centroid_Y)
names(sep)[-1] <- as.numeric(substring(names(sep)[-1], 5,9))
sep.y <- pivot_longer(sep, 2:ncol(sep))
sep.y$name <- as.numeric(sep.y$name)
names(sep.y) <- c("UNIQUE", "Year", "SEP")

## Now combine grid.zone and sep.y to assign annual sep values to each zone 
zone.sep <- left_join(grid.zone.area, sep.y, by="UNIQUE") %>% 
            group_by(Zone, Year) %>% summarise(SEP=mean(SEP), tot=sum(area.km), 
                                               SEPw=sum(SEP*area.km)/tot) ## weighting by area doesn't change anything
zone.sep <- select(zone.sep, -tot, -SEPw)
save(zone.sep, file="C:/WORK/QBCMOD/DocsFires/rmarkdown/ZoneSEP.rdata")


## Annual SEP
zone.sep.noYZ <- filter(zone.sep, Zone!="Y" & Zone!="Z")
ggplot(zone.sep.noYZ, aes(x=Year, y=SEP, colour=Zone)) + geom_line(cex=2) + 
  facet_wrap(~Zone, scales="free") +  theme_bw()  + scale_colour_brewer(palette="Set1")


## Merge SEP, BURNT.AREA per year and zone
burnt.sep <- left_join(zone.sep, feux.zone.year, by=c("Zone", "Year")) %>% filter(!is.na(area.burnt.km)) 


## Goodness of fit
## https://www2.stat.duke.edu/courses/Fall17/sta521/knitr/Lec-9-Poisson-Checks/predictive-checking.pdf
## The above p-value 0 suggests that a residual deviance as large or
## larger than what we observed under the model in hiv.glm is highly unlikely!
## Suggests that the model is not adequate or lack of fit.


## Relation area.burnt ~ SEP by zone, for fires of size > 200 ha
zone <- "A"
dta.zone <- filter(burnt.sep, Zone==zone)
area.glm01 <- glm(log(area.burnt.km) ~ SEP, family=gaussian(link="identity"), data=dta.zone); summary(area.glm01)
area.glm02 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=gaussian(link="identity"), data=dta.zone); summary(area.glm02)
area.glm03 <- glm(round(area.burnt.km) ~ SEP, family=poisson(link="log"), data=dta.zone); summary(area.glm03)
area.glm04 <- glm(round(area.burnt.km) ~ SEP+I(SEP^2), family=poisson(link="log"), data=dta.zone); summary(area.glm04)
area.glm05 <- glm(log(area.burnt.km) ~ SEP, family=Gamma, data=dta.zone); summary(area.glm05)
area.glm06 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=Gamma, data=dta.zone); summary(area.glm06)

## Predict
xsep <- seq(5, 45, 0.01)
yburnt01 <- predict(area.glm01, list(SEP = xsep),type="response")
yburnt02 <- predict(area.glm02, list(SEP = xsep),type="response")
yburnt03 <- predict(area.glm03, list(SEP = xsep),type="response")
yburnt04 <- predict(area.glm04, list(SEP = xsep),type="response")
yburnt05 <- predict(area.glm05, list(SEP = xsep),type="response")
yburnt06 <- predict(area.glm06, list(SEP = xsep),type="response")

## Plot
mdl.colors <- c(plasma(5),"#21908CFF")
plot(dta.zone$SEP, dta.zone$area.burnt.km, pch = 16, xlab="SEP", ylab = "AB (km2)", main=paste("Zone", zone))
lines(xsep, yburnt01, col=mdl.colors[1], lwd=2)     # ~ SEP
lines(xsep, yburnt02, col=mdl.colors[2], lwd=2)   # ~ SEP+SEP^2
lines(xsep, yburnt03, col=mdl.colors[3], lwd=2)     # ~ SEP
lines(xsep, yburnt04, col=mdl.colors[4], lwd=2)   # ~ SEP+SEP^2
lines(xsep, yburnt05, col=mdl.colors[5], lwd=2)     # ~ SEP
lines(xsep, yburnt06, col=mdl.colors[6], lwd=2)   # ~ SEP+SEP^2
legend(x=15, y=800, paste0("Model", c("01", "02", "03", "04", "05", "06")), pch=19, 
       col=mdl.colors, lwd=3, bty="n")


## Relation NUMBER OF FIRES ~ SEP by zone
nfire.glm01 <- glm(num.fire ~ SEP, family=gaussian(link="identity"), data=dta.zone); summary(nfire.glm01)
nfire.glm02 <- glm(num.fire ~ SEP+I(SEP^2), family=gaussian(link="identity"), data=dta.zone); summary(nfire.glm02)
nfire.glm03 <- glm(num.fire ~ SEP, family=poisson(link="log"), data=dta.zone); summary(nfire.glm03)
nfire.glm04 <- glm(num.fire ~ SEP+I(SEP^2), family=poisson(link="log"), data=dta.zone); summary(nfire.glm04)
nfire.glm05 <- glm(num.fire ~ SEP, family=Gamma, data=dta.zone); summary(nfire.glm05)
nfire.glm06 <- glm(num.fire ~ SEP+I(SEP^2), family=Gamma, data=dta.zone); summary(nfire.glm06)

## Plot observed and estimated
plot(dta.zone$SEP, dta.zone$num.fire, pch = 16, xlab="SEP", ylab = "# fires", main=paste("Zone", zone))
ynfire <- predict(nfire.glm01, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[1], lwd=2)     
ynfire <- predict(nfire.glm02, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[2], lwd=2)     
ynfire <- predict(nfire.glm03, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[3], lwd=2)     
ynfire <- predict(nfire.glm04, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[4], lwd=2)     
ynfire <- predict(nfire.glm05, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[5], lwd=2)     
ynfire <- predict(nfire.glm06, list(SEP = xsep),type="response")
lines(xsep, ynfire, col=mdl.colors[6], lwd=2)     
legend(x=15, y=200, paste0("Model", c("01", "02", "03", "04", "05", "06")), pch=19, 
       col=mdl.colors, lwd=3, bty="n")


## Relation log(area.burnt) ~ SEP by zone, for fires of size > 200 ha
logarea.glm01 <- glm(log(area.burnt.km) ~ SEP, family=gaussian(link="identity"), data=dta.zone); summary(logarea.glm01)
logarea.glm02 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=gaussian(link="identity"), data=dta.zone); summary(logarea.glm02)
logarea.glm03 <- glm(round(log(area.burnt.km)) ~ SEP, family=poisson(link="log"), data=dta.zone); summary(logarea.glm03)
logarea.glm04 <- glm(round(log(area.burnt.km)) ~ SEP+I(SEP^2), family=poisson(link="log"), data=dta.zone); summary(logarea.glm04)
logarea.glm05 <- glm(log(area.burnt.km) ~ SEP, family=Gamma, data=dta.zone); summary(logarea.glm05)
logarea.glm06 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=Gamma, data=dta.zone); summary(logarea.glm06)

## Plot observed and estimated
plot(dta.zone$SEP, log(dta.zone$area.burnt.km), pch=16, xlab="SEP", ylab="log.area", main=paste("Zone", zone))
ylarea <- predict(logarea.glm01, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[1], lwd=2)     
ylarea <- predict(logarea.glm02, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[2], lwd=2)     
ylarea <- predict(logarea.glm03, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[3], lwd=2)     
ylarea <- predict(logarea.glm04, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[4], lwd=2)     
ylarea <- predict(logarea.glm05, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[5], lwd=2)     
ylarea <- predict(logarea.glm06, list(SEP = xsep),type="response")
lines(xsep, ylarea, col=mdl.colors[6], lwd=2)     
legend(x=12, y=9, paste0("Model", c("01", "02", "03", "04", "05", "06")), pch=19, 
       col=mdl.colors, lwd=3, bty="n")




###### REMOVE OUTLIERS
## Without AREA outlier
dta.zone.select <- filter(burnt.sep, Zone==zone, Year!=1997)
area.glm01 <- glm(log(area.burnt.km) ~ SEP, family=gaussian(link="identity"), data=dta.zone.select)
yburnt01 <- predict(area.glm01, list(SEP = xsep),type="response")
area.glm02 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=gaussian(link="identity"), data=dta.zone.select)
yburnt02 <- predict(area.glm02, list(SEP = xsep),type="response")
area.glm03 <- glm(round(log(area.burnt.km)) ~ SEP, family=poisson(link="log"), data=dta.zone.select)
yburnt03 <- predict(area.glm03, list(SEP = xsep),type="response")
area.glm04 <- glm(round(log(area.burnt.km)) ~ SEP+I(SEP^2), family=poisson(link="log"), data=dta.zone.select)
yburnt04 <- predict(area.glm04, list(SEP = xsep),type="response")
area.glm05 <- glm(log(area.burnt.km) ~ SEP, family=Gamma(link="inverse"), data=dta.zone.select)
yburnt05 <- predict(area.glm05, list(SEP = xsep),type="response")
area.glm06 <- glm(log(area.burnt.km) ~ SEP+I(SEP^2), family=Gamma(link="inverse"), data=dta.zone.select)
yburnt06 <- predict(area.glm06, list(SEP = xsep),type="response")
plot(dta.zone.select$SEP, log(dta.zone.select$area.burnt.km), pch=16, xlab="SEP", ylab="log(area.burnt.km)", main=paste("Zone", zone, "without outlier"))
lines(xsep, yburnt01, col=mdl.colors[1], lwd=2)   
lines(xsep, yburnt02, col=mdl.colors[2], lwd=2)   
lines(xsep, yburnt03, col=mdl.colors[3], lwd=2)   
lines(xsep, yburnt04, col=mdl.colors[4], lwd=2)   
lines(xsep, yburnt05, col=mdl.colors[5], lwd=2)   
lines(xsep, yburnt06, col=mdl.colors[6], lwd=2)   
legend(x=20, y=2.5, paste0("Model", c("01", "02", "03", "04", "05", "06")), 
       pch=19, col=mdl.colors, lwd=3, bty="n", ncol=2)
## Goodness of fit when removing outliers
pchisq(area.glm01$deviance, df=area.glm01$df.residual, lower.tail=FALSE)
pchisq(area.glm02$deviance, df=area.glm02$df.residual, lower.tail=FALSE)
pchisq(area.glm03$deviance, df=area.glm03$df.residual, lower.tail=FALSE)
pchisq(area.glm04$deviance, df=area.glm04$df.residual, lower.tail=FALSE)
pchisq(area.glm05$deviance, df=area.glm05$df.residual, lower.tail=FALSE)
pchisq(area.glm05$deviance, df=area.glm06$df.residual, lower.tail=FALSE)
## Without NUM.FIRE outlier
dta.zone.select <- filter(burnt.sep, Zone==zone, Year!=2010)
nfire.glm01 <- glm(num.fire ~ SEP, family=gaussian(link="identity"), data=dta.zone.select)
yburnt01 <- predict(nfire.glm01, list(SEP = xsep),type="response")
nfire.glm02 <- glm(num.fire ~ SEP+I(SEP^2), family=gaussian(link="identity"), data=dta.zone.select)
yburnt02 <- predict(nfire.glm02, list(SEP = xsep),type="response")
nfire.glm03 <- glm(num.fire ~ SEP, family=poisson(link="log"), data=dta.zone.select)
yburnt03 <- predict(nfire.glm03, list(SEP = xsep),type="response")
nfire.glm04 <- glm(num.fire ~ SEP+I(SEP^2), family=poisson(link="log"), data=dta.zone.select)
yburnt04 <- predict(nfire.glm04, list(SEP = xsep),type="response")
nfire.glm05 <- glm(num.fire ~ SEP, family=Gamma(link="inverse"), data=dta.zone.select)
yburnt05 <- predict(nfire.glm05, list(SEP = xsep),type="response")
# nfire.glm06 <- glm(num.fire ~ SEP+I(SEP^2), family=Gamma(link="inverse"), data=dta.zone.select)
plot(dta.zone.select$SEP, dta.zone.select$num.fire, pch=16, xlab="SEP", ylab="num.fire")
lines(xsep, yburnt01, col=mdl.colors[1], lwd=2)   
lines(xsep, yburnt02, col=mdl.colors[2], lwd=2)   
lines(xsep, yburnt03, col=mdl.colors[3], lwd=2)   
lines(xsep, yburnt04, col=mdl.colors[4], lwd=2)   
lines(xsep, yburnt05, col=mdl.colors[5], lwd=2)   
legend(x=15, y=1500, paste0("Model", c("01", "02", "03", "04", "05")), pch=19, col=mdl.colors, lwd=3, bty="n")
## Goodness of fit when removing outliers
pchisq(nfire.glm01$deviance, df=nfire.glm04$df.residual, lower.tail=FALSE)
pchisq(nfire.glm02$deviance, df=nfire.glm04$df.residual, lower.tail=FALSE)
pchisq(nfire.glm03$deviance, df=nfire.glm04$df.residual, lower.tail=FALSE)
pchisq(nfire.glm04$deviance, df=nfire.glm04$df.residual, lower.tail=FALSE)
pchisq(nfire.glm05$deviance, df=nfire.glm04$df.residual, lower.tail=FALSE)



############################################## MERGE ABCFG zones ##############################################
rm(list=ls())
# 683 uniques
load(file="C:/WORK/QBCMOD/DocsFires/rmarkdown/GridZone.rdata")
grid.onezone <- filter(grid.zone.area, Zone=="A" | Zone=="B" | Zone=="C" | Zone=="F" | Zone=="G") %>%
  group_by(UNIQUE) %>% summarise(x=1)
# 683 uniques (idem)
shp01 <- readOGR("C:/WORK/QBCMOD/DataIn/SEP/Grid20_Zones.shp")
grid.zone.area <- shp01@data %>% mutate(area=area(shp01), Zone=Group_1) %>% select(-Group_1) %>% 
    filter(UNIQUE>0,  Zone=="A" | Zone=="B" | Zone=="C" | Zone=="F" | Zone=="G") %>% 
    group_by(UNIQUE) %>% summarize(area.km=sum(area)/10^6)

## Annual SEP for these Uniques all together
zone.sep <- left_join(grid.zone.area, sep.y, by="UNIQUE") %>% group_by(Year) %>% summarise(SEP=mean(SEP))
p1 <- ggplot(zone.sep, aes(x=Year, y=SEP)) + geom_line(cex=2) + theme_bw() + scale_colour_brewer(palette="Set1")
str(zone.sep)

## Burnt area 
load(file="C:/WORK/QBCMOD/DocsFires/rmarkdown/FeuxZone.rdata")
feux.year <- mutate(feux.zg, Year=as.numeric(EXERCIC)+1975) %>% filter(Year>=1994) %>% 
  filter(UNIQUE>0) %>% filter(Zone=="A" | Zone=="B" | Zone=="C" | Zone=="F" | Zone=="G") %>% 
  group_by(Year) %>% summarise(area.burnt=sum(area)/10^6) 
p2 <- ggplot(feux.year, aes(x=Year, y=area.burnt)) + geom_line(cex=2) +  theme_bw() + scale_colour_brewer(palette="Set1")
str(feux.zone.year)
gridExtra::grid.arrange(p1,p2,nrow=2)

## Merge SEP, BURNT.AREA per year and zone
burnt.sep <- left_join(zone.sep, feux.year, by="Year") %>% filter(!is.na(area.burnt)) %>% mutate(ab=area.burnt/100)

## GLMs
area.glm01 <- lm(ab ~ SEP, data=burnt.sep); summary(area.glm01)  #, family=gaussian(link="identity")
area.glm02 <- lm(ab ~ SEP+I(SEP^2), data=burnt.sep); summary(area.glm02)  #, family=gaussian(link="identity")
area.glm03 <- glm(round(ab) ~ SEP, family=poisson(link="log"), data=burnt.sep); summary(area.glm03)
area.glm04 <- glm(round(ab) ~ SEP+I(SEP^2), family=poisson(link="log"), data=burnt.sep); summary(area.glm04)
area.glm05 <- glm(ab ~ SEP, family=Gamma, data=burnt.sep); summary(area.glm05)
area.glm06 <- glm(ab ~ SEP+I(SEP^2), family=Gamma, data=burnt.sep); summary(area.glm06)

## Predict
xsep <- seq(10, 40, 0.01)
yburnt01 <- predict(area.glm01, list(SEP = xsep),type="response")
yburnt02 <- predict(area.glm02, list(SEP = xsep),type="response")
yburnt03 <- predict(area.glm03, list(SEP = xsep),type="response")
yburnt04 <- predict(area.glm04, list(SEP = xsep),type="response")
yburnt05 <- predict(area.glm05, list(SEP = xsep),type="response")
yburnt06 <- predict(area.glm06, list(SEP = xsep),type="response")

## Plot
mdl.colors <- c(plasma(5),"#21908CFF")
plot(burnt.sep$SEP, burnt.sep$ab, pch = 16, xlab="SEP", ylab = "AB (km2)", main="Zone ABCFG")
lines(xsep, yburnt01, col=mdl.colors[1], lwd=2)     # ~ SEP
lines(xsep, yburnt02, col=mdl.colors[2], lwd=2)   # ~ SEP+SEP^2
lines(xsep, yburnt03, col=mdl.colors[3], lwd=2)     # ~ SEP
lines(xsep, yburnt04, col=mdl.colors[4], lwd=2)   # ~ SEP+SEP^2
lines(xsep, yburnt05, col=mdl.colors[5], lwd=2)     # ~ SEP
lines(xsep, yburnt06, col=mdl.colors[6], lwd=2)   # ~ SEP+SEP^2
legend(x=12, y=3100, paste0("Model", c("01", "02", "03", "04", "05", "06")), pch=19, 
       col=mdl.colors, lwd=3, bty="n")
