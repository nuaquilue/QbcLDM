# Set working directory 
rm(list = ls())
setwd("C:/Users/boumav/Desktop/QLandscapeDynamics")
# Load the model and the default scenario's parameters
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
toremove <- toremove[-which(toremove=="scenar")]
rm(list = c(toremove, 'toremove'))

####

# les outputs originaux sont en km2/ période de 5 ans

fig.bfec <- function(scen){ 
  ### scenario AvecFeu_avecVF
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  sc_A2a <- aggregate(sc_A$cc.area.unaff,list(sc_A$time,sc_A$UA),mean)
  sc_A2b <- aggregate(sc_A$cc.area.salvaged,list(sc_A$time,sc_A$UA),mean)
  sc_A2a$x <- sc_A2a$x + sc_A2b$x
  sc_A2a$x <- (sc_A2a$x*100)/5
  return(sc_A2a)
}

plot.bfec <- function(sc1,sc2,sc3,UA) {
  sc1p <- sc3[sc3[,2]==UA,]
  plot(1, t="l",ylim=c(0,max(sc1p[,3])+100), ylab='',  xlim=c(0,22) , main = UA )
  sc1p <- sc1[sc1[,2]==UA,]
  lines(sc1p[,3],col=1, lty=1, lwd=1 ) 
  sc2p <- sc2[sc2[,2]==UA,]  
  lines(sc2p[,3],col=2, lty=1, lwd=1 ) 
  sc3p <- sc3[sc3[,2]==UA,]  
  lines(sc3p[,3],col=3, lty=1, lwd=1 ) 
}

### ouput en ha par an

fig.bfec.ess <- function(scen){ 
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  harv.EPN2 <- aggregate(sc_A$harv.EPN,list(sc_A$time,sc_A$UA),mean)
  harv.SAB2 <- aggregate(sc_A$harv.SAB,list(sc_A$time,sc_A$UA),mean)
  harv.PET2 <- aggregate(sc_A$harv.PET,list(sc_A$time,sc_A$UA),mean)
  harv.BOJ2 <- aggregate(sc_A$harv.BOJ,list(sc_A$time,sc_A$UA),mean)
  harv.ERS2 <- aggregate(sc_A$harv.ERS,list(sc_A$time,sc_A$UA),mean)
  harv.AUT2 <- aggregate(sc_A$harv.other,list(sc_A$time,sc_A$UA),mean)
  out.harv <- cbind(harv.EPN2, harv.SAB2$x, harv.PET2$x, harv.BOJ2$x, harv.ERS2$x,harv.AUT2$x)
  out.harv[,3:8] <- round((out.harv[,3:8]*100)/5,0)
  out.harv  <- as.data.frame(out.harv)
  names(out.harv) <- c("Periode","UA","hEPN","hSAB","hPET","hBOJ","hERS","hAUT")
  return(out.harv)
}
### ouput en ha par an
fig.bfec.ess.cp <- function(scen){ 
  sc_A <- read.table(paste0("outputs/", scen, "/PartialCut.txt"),header=TRUE)
  harv.EPN2 <- aggregate(sc_A$harv.pc.EPN,list(sc_A$time,sc_A$UA),mean)
  harv.SAB2 <- aggregate(sc_A$harv.pc.SAB,list(sc_A$time,sc_A$UA),mean)
  harv.PET2 <- aggregate(sc_A$harv.pc.PET,list(sc_A$time,sc_A$UA),mean)
  harv.BOJ2 <- aggregate(sc_A$harv.pc.BOJ,list(sc_A$time,sc_A$UA),mean)
  harv.ERS2 <- aggregate(sc_A$harv.pc.ERS,list(sc_A$time,sc_A$UA),mean)
  harv.AUT2 <- aggregate(sc_A$harv.pc.other,list(sc_A$time,sc_A$UA),mean)
  out.harv <- cbind(harv.EPN2, harv.SAB2$x, harv.PET2$x, harv.BOJ2$x, harv.ERS2$x,harv.AUT2$x)
  out.harv[,3:8] <- round((out.harv[,3:8]*100)/5,0)
  out.harv  <- as.data.frame(out.harv)
  names(out.harv) <- c("Periode","UA","hEPN","hSAB","hPET","hBOJ","hERS","hAUT")
  return(out.harv)
}

#####################################
##### premier graphique: sans changements climatiques

scen <- "SCC.AF.post"
SCC.AF.post1<- fig.bfec(scen)
SCC.AF.post1a <- fig.bfec.ess(scen)

scen <- "SCC.AF.pri"
SCC.AF.pri2<- fig.bfec(scen)
SCC.AF.pri2a <- fig.bfec.ess(scen)

scen <- "SCC.SF.post"
SCC.SF.post3<- fig.bfec(scen)
SCC.SF.post3a <- fig.bfec.ess(scen)

##

par(mfrow=c(2,2), oma=c(3, 2, 3, 2),mar=c(2,4,1,0.3))

UA="2371"
plot.bfec(SCC.AF.post1,SCC.AF.pri2,SCC.SF.post3,UA)

UA="2471"
plot.bfec(SCC.AF.post1,SCC.AF.pri2,SCC.SF.post3,UA)

UA="2571"
plot.bfec(SCC.AF.post1,SCC.AF.pri2,SCC.SF.post3,UA)

UA="2751"
plot.bfec(SCC.AF.post1,SCC.AF.pri2,SCC.SF.post3,UA)


mtext("Superficie CT (ha/an)", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Période (x 5 ans)", side=1,  line=1, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Récolte coupe totale - scenario sans changement climatique (20 runs)", side=3,  line=1, outer = TRUE,cex=1.5,family="serif", font=1)
legend("bottomleft", legend=c("Sans feu","Avec feu - a posteriori uniquement","Avec feu - a priori 20%"),
       cex=1,lty=1,col=c(3,1,2),lwd=1, bty="n",seg.len=5)   

##################################################
#### Graph 2 - possibilités avec changements climatiques modérés
####

scen <- "CC.AF.post"
CC.AF.post1 <- fig.bfec(scen)
CC.AF.post1a <- fig.bfec.ess(scen)

scen <- "CC.AF.pri"
CC.AF.pri2 <- fig.bfec(scen)
CC.AF.pri2a <- fig.bfec.ess(scen)

scen <- "CC.SF.post"
CC.SF.post3 <- fig.bfec(scen)
CC.SF.post3a <- fig.bfec.ess(scen)

##
par(mfrow=c(2,2), oma=c(3, 2, 5, 2),mar=c(2,4,1,0.3))

UA="2371"
plot.bfec(CC.AF.post1,CC.AF.pri2,CC.SF.post3,UA)

legend("bottomleft", legend=c("Sans feu","Avec feu - a posteriori uniquement","Avec feu - a priori 20%"),
       cex=1,lty=1,col=c(3,1,2),lwd=1, bty="n",seg.len=5)   

UA="2471"
plot.bfec(CC.AF.post1,CC.AF.pri2,CC.SF.post3,UA)

UA="2571"
plot.bfec(CC.AF.post1,CC.AF.pri2,CC.SF.post3,UA)

UA="2751"
plot.bfec(CC.AF.post1,CC.AF.pri2,CC.SF.post3,UA)

mtext("Superficie CT (ha/an)", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Période (x 5 ans)", side=1,  line=1, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Récolte coupe totale - scenario avec changements climatiques \n cycle de feu + 2.5% par période (20 runs)", side=3,  line=1, outer = TRUE,cex=1.5,family="serif", font=1)
legend("bottomleft", legend=c("Sans feu","Avec feu - a posteriori uniquement","Avec feu - a priori 20%"),
       cex=1,lty=1,col=c(3,1,2),lwd=1, bty="n",seg.len=5)  


#### cycle de feu moyen

### scenario AvecFeu_avecVF
cyc_A <- read.table(paste0("outputs/", "CC.AF.post", "/FireRegime.txt"),header=TRUE)
cyc_A1 <- aggregate(cyc_A$fire.cycle,list(cyc_A$zone),mean)

#########################################################
################### proportion des essences dans la récolte
require(ggplot2)
require(gridExtra)

par(mfrow=c(2,2), oma=c(4, 2, 0, 2),mar=c(2,4,1,0.3))
UA="2371"

scen <- "CC.AF.post"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
d<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Avec chang. clim., tolérance élevée")

scen <- "worst.CC.AF.post"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
e<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Avec chang. clim., tolérance faible")

scen <- "SCC.AF.post"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
f<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sans changements climatiques, AVEC feux")

scen <- "SCC.SF.post"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
g<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sans changements climatiques, SANS feux")

grid.arrange(g,f,d, e, nrow = 2, top = "Superficie récoltée en coupe TOTALE par type de peuplement - 2371")

##### composition coupes partielles

compo.bfec <- function(scen,UA){ 
  ### scenario AvecFeu_avecVF
  sc_A <- read.table(paste0("outputs/", scen, "/AgeBySpp2.txt"),header=TRUE)
  sc_A2a <- aggregate(sc_A$Area,list(sc_A$time,sc_A$UA,sc_A$spp),mean)
  xx <- sc_A2a[sc_A2a[,2]==UA,][,-2]
  
  names(xx) <- c("time","ess","area")
  xx[,3] <- round((xx[,3]),0)
  return(xx)
}

##

UA="2371"
scen <- "CC.AF.post"
CC.AF.post <- compo.bfec(scen,UA)
dd <- ggplot(CC.AF.post, aes(x=time, y=area, fill=ess)) + labs(x="Temps (an)",y="Superficie (km2)")+
  geom_area() + ggtitle("Avec chang. clim., tolérance élevée")

scen <- "worst.CC.AF.post"
worst.CC.AF.post <- compo.bfec(scen,UA)
ee <- ggplot(worst.CC.AF.post, aes(x=time, y=area, fill=ess)) + labs(x="Temps (an)",y="Superficie (km2)")+
  geom_area() + ggtitle("Avec chang. clim., tolérance faible") 

scen <- "SCC.AF.post"
SCC.AF.post <- compo.bfec(scen,UA)
ff <- ggplot(SCC.AF.post, aes(x=time, y=area, fill=ess)) + labs(x="Temps (an)",y="Superficie (km2)")+
  geom_area() + ggtitle("Sans changements climatiques, AVEC feux")    

scen <- "SCC.SF.post"
SCC.SF.post <- compo.bfec(scen,UA)
gg <- ggplot(SCC.SF.post, aes(x=time, y=area, fill=ess)) + labs(x="Temps (an)",y="Superficie (km2)")+
  geom_area() + ggtitle("Sans changements climatiques, SANS feux")    

grid.arrange(gg,ff,dd, ee, nrow = 2, top = "Composition forestière - 2371")
