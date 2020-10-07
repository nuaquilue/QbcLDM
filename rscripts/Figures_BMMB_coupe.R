# Set working directory 
rm(list = ls())
# Load the model and the default scenario's parameters
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
toremove <- toremove[-which(toremove=="scenar")]
rm(list = c(toremove, 'toremove'))

####
# les outputs originaux sont en km2/ période de 5 ans. Transformé en ha/an
#

setwd("C:/Users/boumav/Desktop/QLD2_article")

# niveau de récolte au temps 0, déterminé à partir d'un scenario quelconque
niveau.base <- read.table(paste0("outputs/TotProt/InitialHarvestLevel.txt"),header=TRUE)
niveau.base$tot <- niveau.base$ref.harv.level + niveau.base$ref.harv.level.cp/2
niveau.base$UA <- row.names(niveau.base)

fig.bfec <- function(scen){ # unique(sc_A$run)
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  sc_A2a <- aggregate(sc_A$cc.area.unaff,list(sc_A$time,sc_A$UA),mean)
  sc_A2b <- aggregate(sc_A$cc.area.salvaged,list(sc_A$time,sc_A$UA),mean)
  sc_A2a$recup <-  sc_A2b$x
  sc_A2a$tot_CT <-  sc_A2a$x + sc_A2a$recup
  sc_A2a$tot_CT <- (sc_A2a$tot_CT*100)/5
  sc_A2a$recup <- (sc_A2a$recup*100)/5
  sc_A2a$x <- (sc_A2a$x*100)/5
  # risque d'accident de régénération
  scA.rege.exc <- aggregate(sc_A$reg.fail.ex,list(sc_A$time,sc_A$UA),mean)
  scA.rege.inc <- aggregate(sc_A$reg.fail.inc,list(sc_A$time,sc_A$UA),mean)
  sc_A2a$riskrege_ex <- (scA.rege.exc$x * 100)/5
  sc_A2a$riskrege_inc <- (scA.rege.inc$x * 100)/5
  # superficie en coupe partielle
  sc_B <- read.table(paste0("outputs/", scen, "/PartialCut.txt"),header=TRUE)
  sc_B2a <- aggregate(sc_B$s.rec.pc,list(sc_B$time,sc_B$UA),mean)
  sc_B2a$tot_CP <- (sc_B2a$x*100)/5
  sc_A2a$tot_CP <- sc_B2a$tot_CP
  # équivalent volume total (deux fois moins dans les coupes partielles)
  sc_A2a$vol_tot <- sc_A2a$tot_CP/2 + sc_A2a$tot_CT

    # % failures = taux de récolte baisse en bas de 80% niveau initial
    # incluant les coupes partielles
  sc_A$harv.pc <- sc_B$s.rec.pc
  sc_ref <- merge(sc_A, niveau.base, by = "UA")
  sc_ref2 <- aggregate((sc_ref$cc.area.unaff + sc_ref$cc.area.salvaged + (sc_ref$harv.pc/2))<(sc_ref$tot*0.7),list(sc_ref$time,sc_ref$UA),mean)
  sc_A2a$failure <- sc_ref2[,3]
  
  #sc_A2a <- aggregate(sc_A$cc.area.unaff,list(sc_A$time,sc_A$UA),mean)
  return(sc_A2a)
}

###########################
### Figure volumes récoltés par coupe totale

scen.UA <- c("2751","2571","2371","2471")
scen.UA <- "2751"

scen <- "BMMB.recupH.replanif"
scenAx<- fig.bfec(scen)
scenAx2 <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,9)]
scenA <- aggregate(scenAx2$vol_tot,list(scenAx2$Group.1),sum)

scen <- "BMMB.recupF.replanif"
#scen <- "BMMB.recupH.replanif.50"
scenBx <- fig.bfec(scen)
scenBx2 <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,9)]
scenB <- aggregate(scenBx2$vol_tot,list(scenBx2$Group.1),sum)

scen <- "BMMB.recupH.reserve"
#scen <- "BMMB.recupH.replanif.50.SCC"
scenCx <- fig.bfec(scen)
scenCx2 <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,9)]
scenC <- aggregate(scenCx2$vol_tot,list(scenCx2$Group.1),sum)

scen <- "TotProt"
scenDx <- fig.bfec(scen)
scenDx2 <- scenDx[scenDx$Group.2  %in% scen.UA,c(1,9)]
scenD <- aggregate(scenDx2$vol_tot,list(scenDx2$Group.1),sum)

par(mfrow=c(1,1), oma=c(3, 2, 2, 2),mar=c(1,3,1,0.3))

plot(scenA, t="l",ylim=c(00000,max(scenD[,2])),col=1,main="",xaxt='n',ylab="")
mtext("ha / an", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2030","2060","2090")),at=c(20,50,80),las=0, tck = -0.02) 

lines(scenB,col=2)
#lines(scenC,col=3)
lines(scenD,col=1,lwd=2, lty=2)
legend("bottomleft", legend=c("récup 80% matures + prématures","récup 30 % matures + prématures","Scénario sans feux"),
       cex=1,col=c(1,2,1),lwd=c(1,1,2),lty=c(1,1,2), bty="n",seg.len=5)   



###########################
###  failures - pourcentage des runs pour lesquelles les feux entrainent une baisse du taux de récolte 
###  baisse en bas de 70% du niveau initial (t=0). 

scen.UA <- c("2751")

scenA.fail <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,10)]
scenB.fail <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,10)]
scenC.fail <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,10)]

plot(scenA.fail, t="l",ylim=c(00000,1),col=1, main = "",xaxt='n',ylab="")
axis(side = 1,labels=(c("2030","2060","2090")),at=c(25,55,85),las=0, tck = -0.02) 
mtext("Taux", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Année", side=1,  line=1, outer = TRUE,cex=1.2,family="serif", font=1)

lines(scenB.fail,col=2)
#lines(scenC.fail,col=3)
legend("topleft", legend=c("récup 80% matures + prématures","récup 30 % matures + prématures"),
       cex=1,col=c(1,2),lwd=c(1,1),lty=c(1,1), bty="n",seg.len=5)   


#########################################################################
### ratio recup/vert
scen.UA <- c("2751","2571","2371","2471")
scen.UA <- c("2751")


scenA.vert <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,3)]
scenA.vert2 <- aggregate(scenA.vert$x,list(scenA.vert$Group.1),sum)
scenA.recup <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,4)]
scenA.recup2 <- aggregate(scenA.recup$recup,list(scenA.recup$Group.1),sum)
vec.recupA <- round(scenA.recup2$x / (scenA.vert2$x +  scenA.recup2$x),2)

scenB.vert <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,3)]
scenB.vert2 <- aggregate(scenB.vert$x,list(scenB.vert$Group.1),sum)
scenB.recup <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,4)]
scenB.recup2 <- aggregate(scenB.recup$recup,list(scenB.recup$Group.1),sum)
vec.recupB <- round(scenB.recup2$x / (scenB.vert2$x +  scenB.recup2$x),2)

scenC.vert <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,3)]
scenC.vert2 <- aggregate(scenC.vert$x,list(scenC.vert$Group.1),sum)
scenC.recup <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,4)]
scenC.recup2 <- aggregate(scenC.recup$recup,list(scenC.recup$Group.1),sum)
vec.recupC <- round(scenC.recup2$x / (scenC.vert2$x +  scenC.recup2$x),2)

plot(vec.recupA, t="l",ylim=c(0,1),col=1,main="",xaxt='n',ylab="")
axis(side = 1,labels=(c("2030","2060","2090")),at=c(5,11,17),las=0, tck = -0.02) 
mtext("Proportion moyenne de bois brûlé", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Année", side=1,  line=1, outer = TRUE,cex=1.2,family="serif", font=1)

lines(vec.recupB,col=2)
#lines(vec.recupC,col=3)
legend("topleft", legend=c("récup 80% matures + prématures","récup 30 % matures + prématures"),
       cex=1,lty=1,col=c(1,2),lwd=1, bty="n",seg.len=5)   



### ratio CP/CT - en SUPERFICIE

scenA.CP <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,8)]
scenA.CP2 <- aggregate(scenA.CP$tot_CP,list(scenA.CP$Group.1),sum)
scenA.CT <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,5)]
scenA.CT2 <- aggregate(scenA.CT$tot_CT,list(scenA.CT$Group.1),sum)
vec.CP.A <- round(scenA.CP2$x / (scenA.CT2$x +  scenA.CP2$x),3)

scenB.CP <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,8)]
scenB.CP2 <- aggregate(scenB.CP$tot_CP,list(scenB.CP$Group.1),sum)
scenB.CT <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,5)]
scenB.CT2 <- aggregate(scenB.CT$tot_CT,list(scenB.CT$Group.1),sum)
vec.CP.B <- round(scenB.CP2$x / (scenB.CT2$x +  scenB.CP2$x),3)

scenC.CP <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,8)]
scenC.CP2 <- aggregate(scenC.CP$tot_CP,list(scenC.CP$Group.1),sum)
scenC.CT <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,5)]
scenC.CT2 <- aggregate(scenC.CT$tot_CT,list(scenC.CT$Group.1),sum)
vec.CP.C <- round(scenC.CP2$x / (scenC.CT2$x +  scenC.CP2$x),3)

scenD.CP <- scenDx[scenDx$Group.2  %in% scen.UA,c(1,8)]
scenD.CP2 <- aggregate(scenD.CP$tot_CP,list(scenD.CP$Group.1),sum)
scenD.CT <- scenCx[scenDx$Group.2  %in% scen.UA,c(1,5)]
scenD.CT2 <- aggregate(scenD.CT$tot_CT,list(scenD.CT$Group.1),sum)
vec.CP.D <- round(scenD.CP2$x / (scenD.CT2$x +  scenD.CP2$x),3)


plot(vec.CP.A, t="l",ylim=c(0,0.2),col=1)
lines(vec.CP.B,col=2)
lines(vec.CP.C,col=3)
lines(vec.CP.D,col=1, lty=2, lwd=2)
legend("topleft", legend=c("recupH.replanif","recupF.replanif","recupH.reserve","TotProt"),
       cex=1,col=c(1,2,3,1),lwd=c(1,1,1,2),lty=c(1,1,1,2), bty="n",seg.len=5)   


###########################################
########## risque rege

scen.UA = 2751
scen.UA <- c("2751","2571","2371","2471")
scen.UA <-as.character(unique(scenAx$Group.2))

#scenA.ex <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,6)]
#scenA.ex2 <- aggregate(scenA.ex$riskrege_ex,list(scenA.ex$Group.1),mean)
#scenB.ex <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,6)]
#scenB.ex2 <- aggregate(scenB.ex$riskrege_ex,list(scenB.ex$Group.1),mean)
#scenC.ex <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,6)]
#scenC.ex2 <- aggregate(scenC.ex$riskrege_ex,list(scenC.ex$Group.1),mean)

#plot(scenA.ex2, t="l",ylim=c(0,1000),col=1,main="exclus",ylab="")
#lines(scenB.ex2,col=2)
#lines(scenC.ex2,col=3)

scenA.inc <- scenAx[scenAx$Group.2  %in% scen.UA,c(1,7)]
scenA.inc2 <- aggregate(scenA.inc$riskrege_inc,list(scenA.inc$Group.1),sum)
scenB.inc <- scenBx[scenBx$Group.2  %in% scen.UA,c(1,7)]
scenB.inc2 <- aggregate(scenB.inc$riskrege_inc,list(scenB.inc$Group.1),sum)
scenC.inc <- scenCx[scenCx$Group.2  %in% scen.UA,c(1,7)]
scenC.inc2 <- aggregate(scenC.inc$riskrege_inc,list(scenC.inc$Group.1),sum)

par(mfrow=c(1,1), oma=c(4, 2, 2, 2),mar=c(1,2.5,1,0.3))

plot(scenA.inc2, t="l",ylim=c(0,max(scenA.inc2)),col=1,main="",ylab="", xaxt='n')
mtext("superficie à risque (ha/an)", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2030","2060","2090")),at=c(20,50,80),las=0, tck = -0.02) 

legend("bottomleft", legend=c("recup forte","recup faible"),
       cex=1,col=c(1,2),lwd=c(1,1),lty=c(1,1), bty="n",seg.len=5)   

lines(scenB.inc2,col=2)
#lines(scenC.inc2,col=3)



############################# superficie récoltée par essence (CT uniquement pour l'instant)

require(ggplot2)
require(gridExtra)
require(reshape2)

fig.bfec.ess <- function(scen){ 
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  harv.EPN2 <- aggregate(sc_A$harv.cc.EPN,list(sc_A$time,sc_A$UA),mean)
  harv.SAB2 <- aggregate(sc_A$harv.cc.SAB,list(sc_A$time,sc_A$UA),mean)
  harv.PET2 <- aggregate(sc_A$harv.cc.PET,list(sc_A$time,sc_A$UA),mean)
  harv.BOJ2 <- aggregate(sc_A$harv.cc.BOJ,list(sc_A$time,sc_A$UA),mean)
  harv.ERS2 <- aggregate(sc_A$harv.cc.ERS,list(sc_A$time,sc_A$UA),mean)
  harv.AUT2 <- aggregate(sc_A$harv.cc.other,list(sc_A$time,sc_A$UA),mean)
  out.harv <- cbind(harv.EPN2, harv.SAB2$x, harv.PET2$x, harv.BOJ2$x, harv.ERS2$x,harv.AUT2$x)
  out.harv[,3:8] <- round((out.harv[,3:8]*100)/5,0)
  out.harv  <- as.data.frame(out.harv)
  names(out.harv) <- c("Periode","UA","hEPN","hSAB","hPET","hBOJ","hERS","hAUT")
  return(out.harv)
}

UA="2371"

scen <- "Afeuil_Post_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
d<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Post_Lsalv")

scen <- "Afeuil_Pri_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
e<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Pri_Lsalv")

scen <- "Sfeuil_Post_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
f<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Post_Lsalv")

scen <- "Sfeuil_Pri_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
g<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Pri_Lsalv")

scen <- "Afeuil_Post_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
h<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Post_Hsalv")

scen <- "Afeuil_Pri_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
i<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Pri_Hsalv")

scen <- "Sfeuil_Post_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
j<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Post_Hsalv")

scen <- "Sfeuil_Pri_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
k<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("TotProt")

grid.arrange(d+coord_cartesian(ylim = c(0, 15000)),
             e+coord_cartesian(ylim = c(0, 15000)),
             f+coord_cartesian(ylim = c(0, 15000)),
             g+coord_cartesian(ylim = c(0, 15000)),
             h+coord_cartesian(ylim = c(0, 15000)),
             i+coord_cartesian(ylim = c(0, 15000)),
             j+coord_cartesian(ylim = c(0, 15000)),
             k+coord_cartesian(ylim = c(0, 15000)), nrow = 4, top = "Superficie récoltée en coupe TOTALE par type de peuplement - 2371")


################################
#######  BMMB
############################

UA="2751"


scen <- "BMMB.recupH.replanif"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
jj<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("recupH.replanif" )

scen <- "BMMB.recupF.replanif"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
kk<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("recupF.replanif")

scen <- "BMMB.recupH.reserve"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
ll<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("recupH.reserve")

scen <- "TotProt"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
mm<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("TotProt")

grid.arrange(jj+coord_cartesian(ylim = c(0, 13500))+ theme(legend.position="none"),
             kk+coord_cartesian(ylim = c(0, 13500))+ theme(legend.position="none"),
             ll+coord_cartesian(ylim = c(0, 13500))+ theme(legend.position="none"), 
             mm+coord_cartesian(ylim = c(0, 13500))+ theme(legend.position = c(0.8, 0.5)),
             nrow = 2, top = "Superficie récoltée en coupe TOTALE par type de peuplement - 2751")


