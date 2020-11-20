########## PROBABILITY OF IGNITION
library(viridis)
load(file="inputlyrs/rdata/mask.rdata")
load(file="inputlyrs/rdata/pigni_static.rdata")
MAP <- MASK
MAP[!is.na(MASK[])] <- pigni$p
plot(MAP, col=plasma(4))


############ BURNT RATES
library(tidyverse)
scn <- "Test_rcp85.fires.cuts"
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


############ Compare target areas
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



############ Compare harvest levels
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

