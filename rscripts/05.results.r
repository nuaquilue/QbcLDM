#################################### PROBABILITY OF IGNITION ####################################
library(viridis)
load(file="inputlyrs/rdata/mask.rdata")
load(file="inputlyrs/rdata/pigni_static.rdata")
MAP <- MASK
MAP[!is.na(MASK[])] <- pigni$p
plot(MAP, col=plasma(4))


#################################### BURNT RATES ################################################
library(tidyverse)
scn <- "Test_rcp85.fires.cuts.replanif0"
br <- read.table(paste0("outputs/", scn, "/BurntRates.txt"), header=T)
brs <-pivot_longer(br, br:target.area) %>% filter(zone %in% c("A", "C" ,"D", "F"))
jpeg(filename=paste0("C:/work/qbcmod/DataOut/BurnRates_", scn, ".jpg"))
ggplot(data=brs, aes(x=year, y=value, colour=name, group=name)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()

fr <- read.table(paste0("outputs/", scn, "/FireRegime.txt"), header=T)
fr.indx <- select(fr, year, zone, indx.combust, indx.combust.burnt) %>% 
  pivot_longer(indx.combust:indx.combust.burnt)
jpeg(filename=paste0("C:/work/qbcmod/DataOut/IndxCombust_", scn, ".jpg"))
ggplot(data=fr.indx, aes(x=year, y=value, colour=zone, linetype=name)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()

fuel <- read.table(paste0("outputs/", scn, "/FuelByFireZone.txt"), header=T) %>% filter(zone!="Z" & zone!="Y")
jpeg(filename=paste0("C:/work/qbcmod/DataOut/FuelZone_", scn, ".jpg"))
ggplot(data=fuel, aes(x=year, y=pct, colour=zone, linetype=type)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()


#################################### PCT FUEL BURNT ####################################
########## Stacked area chart ##########
library(viridis)
library(tidyverse)
scn <- "Scn_WF_TH_NoCC_Both_FuelTypeBase"
burnt.fuels <- read.table(paste0("outputs/", scn, "/BurntFuels.txt"), header=T) 
all <- group_by(burnt.fuels, run, year) %>% summarize(tot=sum(area))
data <- group_by(burnt.fuels, run, year, type) %>% summarize(area=sum(area)) %>% 
  left_join(all, by=c("run", "year")) %>% mutate(pct=area/tot*100) %>% 
  group_by(year, type) %>% summarise(pct.burnt=mean(pct))

# Plot
tiff(paste0("rscripts/outs/PctBurntFuel_", scn, ".tiff"), width=300, heigh=300)
ggplot(data, aes(x=year, y=pct.burnt, fill=type)) + 
  geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
  theme_bw() + theme(legend.position="none") + ggtitle(substr(scn,5,90))
dev.off()


## Fuel burnt according to the availability of fuel 
zone.area <- read.table(paste0("outputs/", scn, "/SppByFireZone.txt"), header=T) %>% 
  filter(run==1,year==2020) %>% group_by(frz) %>% summarize(area.zone=sum(area)); zone.area
fuel.zone  <- read.table(paste0("outputs/", scn, "/FuelByFireZone.txt"), header=T) %>% 
  left_join(zone.area, by="frz") %>% mutate(area.type=pct*area.zone) %>% select(-pct)
data <- left_join(burnt.fuels, fuel.zone, by=c("run", "year", "frz", "type")) %>%
        group_by(run, year, type) %>% summarize(area.burnt=sum(area), area.type=sum(area.type)) %>% 
        mutate(pct=area.burnt/area.type*100) %>% group_by(year, type) %>% summarise(pct.burnt=mean(pct))
tiff(paste0("rscripts/outs/PctBurntOverFuel_", scn, ".tiff"), width=300, heigh=300)
ggplot(data, aes(x=year, y=pct.burnt, fill=type)) + 
  geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
  theme_bw() + theme(legend.position="none")+ ggtitle(substr(scn,5,90))
dev.off()



#################################### Compare target areas ####################################
scn <- "Test_rcp85.fires"
brf <- read.table(paste0("outputs/", scn, "/BurntRates.txt"), header=T)
brf$scn <- "fire"
scn <- "Test_rcp85.fires.cuts.replanif0"
brfc <- read.table(paste0("outputs/", scn, "/BurntRates.txt"), header=T)
brfc$scn <- "fire.cut"
a <- rbind(brf, brfc) %>%  pivot_longer(br:target.area) %>% filter(name=="target.area")
# ggplot(data=a, aes(x=year, y=value, colour=zone, linetype=scn)) +
#   geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
jpeg(filename="C:/work/qbcmod/DataOut/CompareTargetAreas.jpg", width=300, height = 300)
b <- group_by(a, scn, zone) %>% summarise(target.area=sum(value))
ggplot(data=b, aes(x=zone, y=target.area, colour=scn)) + 
  geom_point(size=2) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()



#################################### Compare harvest levels ####################################
scn <- "Test_rcp85.cuts.replanif0"
cut0 <- read.table(paste0("outputs/", scn, "/Cuts.txt"), header=T)
cut0$scn <- "cut.replan0"
scn <- "Test_rcp85.cuts.replanif1"
cut1 <- read.table(paste0("outputs/", scn, "/Cuts.txt"), header=T)
cut1$scn <- "cut.replan1"
scn <- "Test_rcp85.fires.cuts.replanif0"
cutfire0 <- read.table(paste0("outputs/", scn, "/Cuts.txt"), header=T)
cutfire0$scn <- "fire.cut.replan0"
scn <- "Test_rcp85.fires.cuts.replanif1"
cutfire1 <- read.table(paste0("outputs/", scn, "/Cuts.txt"), header=T)
cutfire1$scn <- "fires.cut.replan1"

## Areas cuts withouth replani
area.cut0 <- rbind(cut0, cutfire0) %>% select(scn, year, MgmtUnit, area.salvaged, area.unaff) %>% 
            pivot_longer(area.salvaged:area.unaff) %>% group_by(scn, year) %>% summarise(area=sum(value)*km2.pixel)
jpeg(filename="C:/work/qbcmod/DataOut/CompareCutAreas_replan0.jpg", width=300, height = 300)
ggplot(data=area.cut0, aes(x=year, y=area, colour=scn)) + 
  geom_line(size=1.2) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()

## The 4 scn
area.cut <- rbind(cut0, cut1, cutfire0, cutfire1) %>% select(scn, year, MgmtUnit, area.salvaged, area.unaff) %>% 
  pivot_longer(area.salvaged:area.unaff) %>% group_by(scn, year) %>% summarise(area=sum(value)*km2.pixel)
jpeg(filename="C:/work/qbcmod/DataOut/CompareCutAreas.jpg", width=300, height = 300)
ggplot(data=area.cut, aes(x=year, y=area, colour=scn)) + 
  geom_line(size=1.2) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()



# One single FMU
area.cut.one <- rbind(cut, cutfire) %>% select(scn, year, MgmtUnit, area.salvaged, area.unaff) %>% 
  pivot_longer(area.salvaged:area.unaff) %>% group_by(scn, year, MgmtUnit) %>% 
  summarise(area=sum(value)) %>% filter(MgmtUnit==2751)
ggplot(data=area.cut.one, aes(x=year, y=area, colour=scn)) + 
  geom_line(size=1.2) + theme_classic() + scale_color_brewer(palette="Set1")



#################################### PLOT raster ####################################
library(rasterVis)
library(raster)
library(tidyverse)
TSC <- raster("C:/WORK/QBCMOD/QbcLDM/outputs/Test_rcp85.cuts.replanif1/lyr/TSCcut_r1t90.tif")
tsc <- TSC[]
# tsc[!is.na(tsc) & tsc<=90] <- 1
tsc[!is.na(tsc) & tsc>90] <- 0
TSC[] <- tsc
TSC@extent@xmin = TSC@extent@xmin / 1000
TSC@extent@xmax = TSC@extent@xmax / 1000
TSC@extent@ymin = TSC@extent@ymin / 1000
TSC@extent@ymax = TSC@extent@ymax / 1000
myTheme <- viridisTheme()
myTheme$regions$col[1] <- "grey95"

levelplot(TSC, margin=FALSE, colorkey=list(space="bottom"), 
          par.settings=myTheme, main="Time since clear-cuts")


TSF <- raster("C:/WORK/QBCMOD/QbcLDM/outputs/Test_wildfires/lyr/TSF_r1t90.tif")
tsf <- TSF[]
# tsc[!is.na(tsc) & tsc<=90] <- 1
tsf[!is.na(tsf) & tsf>90] <- 0
TSF[] <- tsf
TSF@extent@xmin = TSF@extent@xmin / 1000
TSF@extent@xmax = TSF@extent@xmax / 1000
TSF@extent@ymin = TSF@extent@ymin / 1000
TSF@extent@ymax = TSF@extent@ymax / 1000
myTheme <- magmaTheme()
myTheme$regions$col[1] <- "grey95"
levelplot(TSF, margin=FALSE, colorkey=list(space="bottom"), 
          par.settings=myTheme, main="Time since fires")


AGE <- raster("C:/WORK/QBCMOD/QbcLDM/outputs/Test_nothing/lyr/Age_r1t90.tif")
AGE@extent@xmin = AGE@extent@xmin / 1000
AGE@extent@xmax = AGE@extent@xmax / 1000
AGE@extent@ymin = AGE@extent@ymin / 1000
AGE@extent@ymax = AGE@extent@ymax / 1000
levelplot(AGE, margin=FALSE, colorkey=list(space="bottom"), par.settings=BTCTheme(), main="Forest Age")


SPP <- raster("C:/WORK/QBCMOD/QbcLDM/outputs/Test_nothing/lyr/SppGrp_r1t90.tif")
SPP@extent@xmin = SPP@extent@xmin / 1000
SPP@extent@xmax = SPP@extent@xmax / 1000
SPP@extent@ymin = SPP@extent@ymin / 1000
SPP@extent@ymax = SPP@extent@ymax / 1000
myPal <- RColorBrewer::brewer.pal('Set1', n=10)
myTheme <- rasterTheme(region = myPal)
levelplot(SPP, margin=FALSE, colorkey=list(space="bottom"),  par.settings=myTheme, main="Species groups")
