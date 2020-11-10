library(tidyverse)
for(i in 1:9){
  fires <- read.table(paste0("outputs/Test00",i,"/Fires.txt"), header=T)
  fires$rem <- fires$atarget-fires$aburnt
  uncomplet <- filter(fires, rem>0)
  # nrow(uncomplet); sum(uncomplet$dif); 
  print(round(sum(uncomplet$rem)/sum(fires$atarget)*100,1))
  
}



extra <- filter(fires, rem<0)

stat <- group_by(fires, run, zone) %>% summarise(atarget=sum(atarget), aburnt=sum(aburnt)) %>%
        group_by(zone) %>% summarize(atarget=mean(atarget), aburnt=mean(aburnt))
stat


library(viridis)
load(file="inputlyrs/rdata/mask.rdata")
load(file="inputlyrs/rdata/pigni_static.rdata")
MAP <- MASK
MAP[!is.na(MASK[])] <- pigni$p
plot(MAP, col=plasma(4))



scn <- "Test02"
br <- read.table("outputs/Test02/BurntRates.txt", header=T)
brs <-pivot_longer(br, br:target.area) %>% filter(zone %in% c("A", "C" ,"D", "F"))
jpeg(filename=paste0("C:/work/qbcmod/DataOut/BurnRates_", scn, ".jpg"))
ggplot(data=brs, aes(x=year, y=value, colour=name, group=name)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()

fr <- read.table("outputs/Test02/FireRegime.txt", header=T)
fr.indx <- select(fr, year, zone, indx.combust, indx.combust.burnt) %>% 
  pivot_longer(indx.combust:indx.combust.burnt)
jpeg(filename=paste0("C:/work/qbcmod/DataOut/IndxCombust_", scn, ".jpg"))
ggplot(data=fr.indx, aes(x=year, y=value, colour=zone, linetype=name)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()

fuel <- read.table("outputs/Test02/FuelByFireZone.txt", header=T) %>% filter(zone!="Z" & zone!="Y")
jpeg(filename=paste0("C:/work/qbcmod/DataOut/FuelZone_", scn, ".jpg"))
ggplot(data=fuel, aes(x=year, y=pct, colour=zone, linetype=ftype)) + 
  geom_line(size=1.2) + facet_wrap(~zone) + theme_classic() + scale_color_brewer(palette="Set1")
dev.off()