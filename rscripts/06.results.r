fires <- read.table("outputs/Test001/Fires.txt", header=T)
fires$dif <- fires$atarget-fires$aburnt

uncomplet <- filter(fires, dif>0)
nrow(uncomplet); sum(uncomplet$dif); round(sum(uncomplet$dif)/sum(fires$atarget)*100,1)

extra <- filter(fires, dif<0)

stat <- group_by(fires, run, zone) %>% summarise(atarget=sum(atarget), aburnt=sum(aburnt)) %>%
        group_by(zone) %>% summarize(atarget=mean(atarget), aburnt=mean(aburnt))
stat
