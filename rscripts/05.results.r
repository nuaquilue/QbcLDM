#################################### PROBABILITY OF IGNITION ####################################
library(viridis)
load(file="inputlyrs/rdata/mask.rdata")
load(file="inputlyrs/rdata/pigni_static.rdata")
MAP <- MASK
MAP[!is.na(MASK[])] <- pigni$p
plot(MAP, col=plasma(4))


#################################### TARGET AREA TO BE BURNT  ############################################
library(tidyverse)
list.scn <- c("Scn_WF_NoCC.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_NoCC.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_NoCC.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.fuel.modif_Both_FuelTypeBase")
list.scn2 <- c("Scn_WF_TH_NoCC.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_NoCC.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_NoCC.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.fuel.modif_Both_FuelTypeBase")
list.scn <- c(list.scn, list.scn2)
clim <- c(rep("NoCC",3), rep("RCP45",3), rep("RCP85",3),rep("NoCC",3), rep("RCP45",3), rep("RCP85",3))
modif <- c("both", "clima", "fuel", "both", "clima", "fuel", "both", "clima", "fuel",
           "both", "clima", "fuel", "both", "clima", "fuel", "both", "clima", "fuel")
harvest <- c(rep(F,9), rep(T,9))
dta <- data.frame(run=NA, year=NA, frz=NA, br=NA, brvar=NA, brfuel=NA, 
                  brclima=NA, target.area=NA, clim=NA, modif=NA, harvest=NA)
for(i in 1:length(list.scn)){
  br <- read.table(paste0("outputs/", list.scn[i], "/BurntRates.txt"), header=T)  %>% 
        mutate(clim=clim[i], modif=modif[i], harvest=harvest[i])
  dta <- rbind(dta, br)
}              
dta <- dta[-1,]
# plot
zone <- "Z6"
brs <- filter(dta, frz==zone) %>% group_by(year, clim, modif) %>% 
       summarise(br=mean(br), brvar=mean(brvar), brfuel=mean(brfuel), 
                 brclima=mean(brclima), target.area=mean(target.area)) %>% 
       pivot_longer(br:target.area)# %>% filter(name!="target.area")
names(brs)[5] <- "km2"
jpeg(filename=paste0("C:/work/qbcmod/DataOut/BurnRatesWithTH_", zone, ".jpg"), width=800, height=600)
ggplot(data=brs, aes(x=year, y=km2, colour=name, group=name)) +  geom_line(size=1.2) +
  facet_grid(clim~modif) + theme_classic() + scale_color_brewer(palette="Set1") +
  theme(plot.title = element_text(size=22, face="bold"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text=element_text(size=14),
        strip.text.x = element_text(size=16, face="bold"),
        strip.text.y = element_text(size=16, face="bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  geom_vline(xintercept=2040, linetype="dashed", color = "black") + 
  geom_vline(xintercept=2070, linetype="dashed", color = "black")  + ggtitle(zone)
dev.off()
# table
target <- filter(dta, modif=="both") %>% 
          mutate(period=ifelse(year<=2040, "20-40", ifelse(year<=2070, "40-70", "70-100"))) %>% 
          group_by(run, frz, clim, harvest, period) %>% 
          summarise(target.area=sum(target.area), ny=length(run)) %>% 
          mutate(target5y=target.area/ny) %>% group_by(frz, clim, harvest, period) %>% 
          summarize(mn=round(mean(target5y),1), sd=round(sd(target5y),1))
select(target, -sd) %>% 
  pivot_wider(names_from=c("clim", "harvest"), values_from=mn)
# boxplot
target <- filter(dta, modif=="both") %>% 
          mutate(scn=ifelse(harvest, paste0(clim,"_CUT"), clim)) %>% 
          group_by(run, frz, scn) %>% summarise(target.area=sum(target.area))
ggplot(target, aes(x=scn, y=target.area, fill=scn)) + geom_boxplot() +
theme_bw() +  facet_wrap(.~frz, scales="free_y")+
theme(axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text.x=element_text(size=12, angle=90),
      legend.position="none") +
  scale_fill_manual(values=c("#999999", "#999999", "#56B4E9", "#56B4E9","#E69F00", "#E69F00" ))


 
#################################### FUEL TYPE PER FIRE REGIME ZONE  ############################################
library(tidyverse)
library(viridis)
list.scn <- c("Scn_WF_NoCC.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_NoCC.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_NoCC.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_RCP45.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_RCP85.is.fuel.modif_Both_FuelTypeBase")
list.scn <- c("Scn_WF_TH_NoCC.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_NoCC.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_NoCC.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP45.is.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.clima.fuel.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.clima.modif_Both_FuelTypeBase",
              "Scn_WF_TH_RCP85.is.fuel.modif_Both_FuelTypeBase")
clim <- c(rep("NoCC",3), rep("RCP45",3), rep("RCP85",3))
modif <- c("both", "clima", "fuel", "both", "clima", "fuel", "both", "clima", "fuel")
dta <- data.frame(run=NA, year=NA, frz=NA, type=NA, pct=NA, clim=NA, modif=NA)
for(i in 1:9){
  aux <- read.table(paste0("outputs/", list.scn[i], "/FuelByFireZone.txt"), header=T)  %>% 
         mutate(clim=clim[i], modif=modif[i])
  dta <- rbind(dta, aux)
}              
dta <- dta[-1,]

zone <- "Z6"
fuels <- filter(dta, frz==zone) %>% group_by(year, clim, modif, type) %>% summarise(pct=mean(pct)) 
jpeg(filename=paste0("C:/work/qbcmod/DataOut/FuelByZone_", zone, ".jpg"), width=800, height=600)
ggplot(fuels, aes(x=year, y=pct, fill=type)) + facet_grid(clim~modif) +
  geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
  theme_classic() +  ggtitle(zone) +
  theme(legend.position="bottom",
      plot.title = element_text(size=22, face="bold"),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.text=element_text(size=14),
      strip.text.x = element_text(size=16, face="bold"),
      strip.text.y = element_text(size=16, face="bold"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14)) 
dev.off()


# fr <- read.table(paste0("outputs/", scn, "/FireRegime.txt"), header=T)
# fr.indx <- select(fr, year, zone, indx.combust, indx.combust.burnt) %>% 
#   pivot_longer(indx.combust:indx.combust.burnt)
# jpeg(filename=paste0("C:/work/qbcmod/DataOut/IndxCombust_", scn, ".jpg"))
# ggplot(data=fr.indx, aes(x=year, y=value, colour=zone, linetype=name)) + 
#   geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
# dev.off()



#################################### PCT FUEL CLASSES per FIRE ZONE ####################################
scn <- "Scn_WF_TH_RCP45_Both_FuelTypeBase"
fuels <- read.table(paste0("outputs/", scn, "/FuelByFireZone.txt"), header=T) 
data <- group_by(fuels, run, year, type) %>% summarize(pct=sum(pct)) %>% 
        group_by(year, type) %>% summarise(pct=mean(pct))
tiff(paste0("rscripts/outs/PctFuel_", scn, ".tiff"), width=300, heigh=300)
ggplot(data, aes(x=year, y=pct, fill=type)) + 
  geom_area(alpha=1 , size=.5, colour="grey70") + scale_fill_viridis(discrete = T) +
  theme_bw() + theme(legend.position="none") + ggtitle(substr(scn,5,90))
dev.off()




#################################### PCT FUEL BURNT ####################################
########## Stacked area chart ##########
library(viridis)
library(tidyverse)
scn <- "Scn_WF_NoCC_Large_FuelTypeBase"
scn <- "Scn_WF_NoCC_Small_FuelTypeBase"
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



#################################### DEBUG WILDFIRES ####################################
scn <- "Scn_WF_NoCC_Small_FuelTypeBase"
scn <- "Scn_WF_NoCC_Large_FuelTypeBase"
scn <- "Test_nothing45"
sprd <- read.table(paste0("outputs/", scn, "/FireSprd.txt"), header=T)
sprd <- filter(sprd, step!=1)  # ignore ignitions
# plot(sprd$flam, sprd$sr)
par(mfrow=c(1,2))
hist(sprd$sr); hist(sprd$pb)

table(sprd$burn, sprd$pb)

sprd$pb1 <- 1+log(sprd$sr)
sprd$pb1 <- 1-exp(-sprd$sr*2) 
hist(sprd$pb1); hist(sprd$pb)


## %burnt per fuel category
correspond <- data.frame(fuel=c("1.low", "2.med", "2.med", "3.high"), 
                         type=c("hardwood", "pioneer", "young.conif", "mature.conif"))
list.scn <- c("Scn_WF_NoCC_Small_FuelTypeBase",  "Scn_WF_NoCC_Large_FuelTypeBase",
              "Scn_WF_NoCC_Both_FuelTypeBase")
list.scn2 <- c(paste0(list.scn, "_rpb3"), paste0(list.scn, "_rpb1"), paste0(list.scn, "_rpb4"))
rpb <- c(0.3, 0.3, 0.3, 0.1, 0.1, 0.1, 0.4, 0.4, 0.4)
kk <- rep(c("All_0.1-0.4-0.95", "All_0.95", "Small_0.1-0.4-0.95"),3)
r <- data.frame(fuel=NA, pct.burnt=NA, scn=NA, r=NA)
for(i in 1:9){
  burnt.fuels <- read.table(paste0("outputs/", list.scn2[i], "/BurntFuels.txt"), header=T) 
  all <- group_by(burnt.fuels, run) %>% summarize(tot=sum(area))
  data <- left_join(burnt.fuels, correspond, by="type") %>% 
    group_by( run, fuel) %>% summarize(area=sum(area)) %>% 
    left_join(all, by=c("run")) %>% mutate(pct=area/tot*100) 
  aux <- group_by(data, fuel) %>% summarise(pct.burnt=mean(pct)) %>% mutate(scn=kk[i], r=rpb[i])
  r <- rbind(r,aux)  
}
r <- r[-1,]
ggplot(data=r, aes(x=fuel, y=pct.burnt, colour=scn, group=scn)) +  
  geom_line(size=2) + theme_bw() + theme(legend.position="bottom") +
  scale_color_brewer(palette="Dark2") +facet_grid(.~r)
