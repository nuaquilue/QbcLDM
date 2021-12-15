
rm(list = ls())

library(readxl)

scenarios <- read_xlsx("Scenarios2.xlsx", sheet="Feuil1")

seuils.a.priori <- as.data.frame(matrix(0,length(unique(sc_A$mgmt.unit)),18))

i=3
for (i in c(3,4,5,6,9,10,11,12,15,16,17,18)) {
  sc_A <-  read.table(paste("outputs/",scenarios$No[i],"/Cuts.txt",sep=""),header=TRUE)
  #sc_A <- sc_A[(!substr(sc_A$mgmt.unit,1,2) %in% "AG"),]
  print(i)
  list.UA <- unique(sc_A$mgmt.unit)
  sup.UA <- sc_A$tot.inc[1:length(list.UA)]
  n.simul <- length(unique(sc_A$run))
  sc_A$recT <- sc_A$area.salvaged + sc_A$area.unaff + (sc_A$a.pcut/2)
  #plot(1,t="l",ylim=c(0,max(sc_Ax[,13])), ylab='',  xlim=c(0,22))
  rec.min <- rep(0,n.simul)
  rec.moy <- rep(0,n.simul)
  rec.per1 <- rep(0,n.simul)
  per.crit <- rep(0,n.simul)
  per1.ua <- rep(0,length(list.UA)) 
  min.ua <- rep(0,length(list.UA)) 
  max.ua <- rep(0,length(list.UA)) 
  baisse.ua <- rep(0,length(list.UA))
  med.ua <- rep(0,length(list.UA)) 
  aire.tot.UA <- rep(0,length(list.UA)) 
  per.crit.ua <- rep(0,length(list.UA))
  for(j in 1:length(list.UA))  { #j=1
    sc_Ax <- sc_A[sc_A$mgmt.unit == list.UA[j],]

    for (k in 1:n.simul) {  # k=4
       bbb1 <- sc_Ax[sc_Ax$run==k,]
       #lines(bbb1$recT,col=1, lty=1, lwd=1 )
       per1.run   <- bbb1$recT[1]
       min.run   <- quantile(bbb1$recT)[1]
       moy.run   <- mean(bbb1$recT)
       per.crit.run <- which(bbb1$recT == min.run)[1]
       rec.per1[k] <- per1.run
       rec.min[k] <- min.run
       rec.moy[k] <- moy.run
       per.crit[k] <- per.crit.run
       aire.tot <- bbb1$tot.inc[1]
    }
    per1.ua[j] <- mean(rec.per1)
    min.ua[j] <- min(rec.min)
    max.ua[j] <- max(rec.min)
    med.ua[j] <- round(quantile(rec.min)[3])
    diff.ua.pct <- (rec.per1 - rec.min)/rec.per1
    # baisse fixée à 50% des minimums observés
    baisse.ua[j] <- quantile(diff.ua.pct)[3]
    per.crit.ua[j] <- quantile(per.crit)[3]
    aire.tot.UA[j] <- aire.tot
  }
  # taux feu
  bid.1 <- aggregate(sc_A[,]$a.inc.burnt, by=list(sc_A[,]$mgmt.unit),FUN=mean, na.rm=TRUE)
  bid.2 <- aggregate(sc_A[,]$tot.inc, by=list(sc_A[,]$mgmt.unit),FUN=mean, na.rm=TRUE)
  taux.feu.annuel <- as.data.frame(((bid.1$x/bid.2$x)/5))
  taux.feu.annuel<- round(taux.feu.annuel,5)
  names(taux.feu.annuel) <- "taux_feu"
  output <- as.data.frame(cbind(list.UA,sup.UA,round(taux.feu.annuel,5),per1.ua,min.ua,med.ua,max.ua,
                                baisse.ua, round(2020+(5*per.crit.ua))))
  output$sc <- scenarios$No[i]
  output$sc.no <- i
  output$rcp <- scenarios$rcp[i]
  output$salv <- ifelse(scenarios$Prop.salv[i]==0.2,"LowSalv",ifelse(scenarios$Prop.salv[i]==0.7,"HighSalv","NA"))
  output$replan <- ifelse(scenarios$replanning[i]=="Y","Replan","Buffer")
  output$fire <- ifelse(scenarios$Fire[i]=="Y","WFire","NFire")
  if (i==3) {
    seuils.a.priori <- output
     } else {
       seuils.a.priori <- rbind(seuils.a.priori,output)
   }
}

names(seuils.a.priori)[9] <- "per.crit"

exp.seuils <- c("list.UA","med.ua","baisse.ua","sup.UA","taux.feu","sc","min.ua","max.ua","per1.ua")
seuils.a.priori2 <- seuils.a.priori[,which(names(seuils.a.priori) %in% exp.seuils)]
seuils.a.priori2$baisse.ua <- round(seuils.a.priori2$baisse.ua,2)
seuils.a.priori2$min.ua <- round(seuils.a.priori2$min.ua,1)

write.table(seuils.a.priori2, "outputs/seuils.a.priori.txt", row.names=F,  sep="\t")

# export scnario 12

seuils.a.priori3 <- seuils.a.priori[seuils.a.priori$sc.no==12,][1:59,]
seuils.a.priori3$list.UA <- as.numeric(seuils.a.priori3$list.UA)
write.table(seuils.a.priori3, "outputs/seuils.crit.txt", row.names=F,  sep="\t")

