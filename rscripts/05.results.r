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
