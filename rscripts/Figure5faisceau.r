
###################
library(readxl)
#library(here)
rm(list=ls())
scenarios <- read_xlsx("Scenarios2.xlsx", sheet="Feuil1")

##########
seuils <- read.table("outputs/seuils.a.priori.txt",   sep="\t", header=T)


###############
################

res.cut <-  read.table(paste("outputs/",scenarios$No[2],"/Cuts.txt",sep=""),header=TRUE)
res.cut$sc <- "sc2"
res.cut$rcp <- scenarios$rcp[2]
res.cut$salv <- ifelse(scenarios$Prop.salv[2]==0.2,"LowSalv",ifelse(scenarios$Prop.salv[1]==0.7,"HighSalv","NA"))
res.cut$replan <- ifelse(scenarios$replanning[2]=="Y","Replan","Buffer")
res.cut$fire <- ifelse(scenarios$Fire[2]=="Y","WFire","NFire")
res.cut$pers <- ifelse(scenarios$persist[1]=="pers","pers","no.pers")
for (i in c(3:6,8:12,14:18)) {
  res.cutx <-  read.table(paste("outputs/",scenarios$No[i],"/Cuts.txt",sep=""),header=TRUE)
  res.cutx$sc <- paste("sc",i,sep="")
  res.cutx$rcp <- scenarios$rcp[i]
  res.cutx$salv <- ifelse(scenarios$Prop.salv[i]==0.2,"LowSalv",ifelse(scenarios$Prop.salv[i]==0.7,"HighSalv","NA"))
  res.cutx$replan <- ifelse(scenarios$replanning[i]=="Y","Replan","Buffer")
  res.cutx$fire <- ifelse(scenarios$Fire[i]=="Y","WFire","NFire")  
  res.cutx$pers <- ifelse(scenarios$persist[i]=="pers","pers","no.pers")
  res.cut <- rbind(res.cut,res.cutx)
}

res.cut.pub <- res.cut[!substr(res.cut$mgmt.unit,1,2) %in% "AG",]

res.cut.pub$aire_tot <- res.cut.pub$area.salvaged + res.cut.pub$area.unaff + res.cut.pub$a.pcut/2

faisc <- res.cut.pub[res.cut.pub$sc=="sc12",which(names(res.cut.pub) %in% 
                     c("run","year","aire_tot","mgmt.unit")) ]

## pour deux UAs
nrun <- length(unique(faisc$run))

faiscUA1 <- faisc[faisc$mgmt.unit== "2751",]
niv.med1 <- seuils[seuils$list.UA=="2751" & seuils$sc == "scen12",]$med.ua
niv.min1 <- seuils[seuils$list.UA=="2751" & seuils$sc == "scen12",]$min.ua
niv.max1 <- seuils[seuils$list.UA=="2751" & seuils$sc == "scen12",]$max.ua
niv.per1 <- seuils[seuils$list.UA=="2751" & seuils$sc == "scen12",]$per1.ua

faiscUA2 <- faisc[faisc$mgmt.unit== "2661",]
niv.med2 <- seuils[seuils$list.UA=="2661" & seuils$sc == "scen12",]$med.ua
niv.min2 <- seuils[seuils$list.UA=="2661" & seuils$sc == "scen12",]$min.ua
niv.max2 <- seuils[seuils$list.UA=="2661" & seuils$sc == "scen12",]$max.ua
niv.per2 <- seuils[seuils$list.UA=="2661" & seuils$sc == "scen12",]$per1.ua

#pdf("Figure5.pdf",width=8.5,height=4)

h.ramp <- heat.colors(6,alpha=0.7)

par(mfrow=c(1,2), oma=c(3, 3, 2, 2),mar=c(2,2,1,0.3))

plot(1, t="l",ylim=c(0,max(faiscUA1$aire_tot)), ylab='',  xlim=c(2020,2100), main = "" )
  rect(2000, niv.max1, 2022, niv.per1, col = h.ramp[1],border = NA )
  rect(2000, niv.med1, 2022, niv.max1, col = h.ramp[2],border = NA )  
  rect(2000, niv.min1, 2022, niv.med1, col = h.ramp[3],border = NA ) 
  rect(2000, 0, 2022, niv.min1, col = h.ramp[4],border = NA ) 
for(i in 1:nrun) {
  sc1p <- faiscUA1[faiscUA1[,1]==i,]

  lines(sc1p[,4]~sc1p[,2],col=1, lty=1, lwd=0.5 ) 
  abline(h=niv.min1, col=1, lty=2, lwd=1)
  abline(h=niv.med1, col=1, lty=2, lwd=1)
  abline(h=niv.max1, col=1, lty=2, lwd=1)
}

plot(1, t="l",ylim=c(0,max(faiscUA2$aire_tot)), ylab='',  xlim=c(2020,2100), main = "" )
rect(2000, niv.max2, 2022, niv.per2, col = h.ramp[1],border = NA )
rect(2000, niv.med2, 2022, niv.max2, col = h.ramp[2],border = NA )  
rect(2000, niv.min2, 2022, niv.med2, col = h.ramp[3],border = NA ) 
rect(2000, 0, 2022, niv.min2, col = h.ramp[4],border = NA ) 
for(i in 1:nrun) {
  sc1p <- faiscUA2[faiscUA2[,1]==i,]
  lines(sc1p[,4]~sc1p[,2],col=1, lty=1, lwd=0.5 ) 
  abline(h=niv.min2, col=1, lty=2, lwd=1)
  abline(h=niv.med2, col=1, lty=2, lwd=1)
  abline(h=niv.max2, col=1, lty=2, lwd=1)
}

mtext('Year (period)',at=.5,side=1,outer=T,cex=1,line=1, las=1)
mtext('Area cut (no. cells/period)',at=0.5,side=2,outer=T,cex=1,line=0.3,las=3)
mtext('FMU 02751',at=0.12,side=3,outer=T,cex=1,line=0.1,las=1)
mtext('FMU 02661',at=0.62,side=3,outer=T,cex=1,line=0.1,las=1)

dev.off()
