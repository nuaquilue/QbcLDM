########## PROBABILITY OF IGNITION
library(viridis)
load(file="inputlyrs/rdata/mask.rdata")
load(file="inputlyrs/rdata/pigni_static.rdata")
MAP <- MASK
MAP[!is.na(MASK[])] <- pigni$p
plot(MAP, col=plasma(4))


############ BURNT RATES

scn <- "Test_rcp85.fires"
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


## Compare target areas
scn <- "Test_rcp85.fires"
brf <- read.table(paste0("outputs/", scn, "/BurntRates.txt"), header=T)
brf$scn <- "fire"
scn <- "Test_rcp85.fire.cuts"
brfc <- read.table(paste0("outputs/", scn, "/BurntRates.txt"), header=T)
brfc$scn <- "fire.cut"
a <- rbind(brf, brfc) %>%  pivot_longer(br:target.area) %>% filter(name=="target.area")

ggplot(data=a, aes(x=year, y=value, colour=zone, linetype=scn)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")

b <- group_by(a, scn, zone) %>% summarise(target.area=sum(value))
ggplot(data=b, aes(x=zone, y=target.area, colour=scn)) + 
  geom_point(size=2) + theme_classic() + scale_color_brewer(palette="Set1")
