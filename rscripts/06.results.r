for(i in 1:9){
  fires <- read.table(paste0("outputs/Test00",i,"/Fires.txt"), header=T)
  fires$dif <- fires$atarget-fires$aburnt
  uncomplet <- filter(fires, dif>0)
  # nrow(uncomplet); sum(uncomplet$dif); 
  print(round(sum(uncomplet$dif)/sum(fires$atarget)*100,1))
  
}



extra <- filter(fires, dif<0)

stat <- group_by(fires, run, zone) %>% summarise(atarget=sum(atarget), aburnt=sum(aburnt)) %>%
        group_by(zone) %>% summarize(atarget=mean(atarget), aburnt=mean(aburnt))
stat
