
####
# les outputs originaux sont en km2/ période de 5 ans. Transformé en ha/an
#

setwd("C:/Users/boumav/Desktop/QLD_V2")

fig.ratios <- function(scen){ 
  sc_AA <- read.table(paste0("outputs/", scen, "/Spread.txt"),header=TRUE)
  sc_AA2a <- aggregate(list(sc_AA$brulcl1,sc_AA$brulcl2,sc_AA$brulcl3,sc_AA$zon.cl1,sc_AA$zon.cl12,sc_AA$zon.cl3),
                       list(sc_AA$time,sc_AA$zone),mean)
  names(sc_AA2a) <- c("time","Zone","brulcl1","brulcl2","brulcl3","zon.cl1","zon.cl2","zon.cl3")
  return(sc_AA2a)
}

###########################
### Figure volumes récoltés par coupe totale

scen.UA <- c("2751","2551","2371","2471")

scen <- "SC1.WPlant.Post.Lsalv"
scenAAx<- fig.ratios(scen)
scen <- "SC2.Wplant.Prio.Lsalv"
scenBBx <- fig.ratios(scen)
scen <- "SC3.NoPlant.Post.Lsalv"
scenCCx <- fig.ratios(scen)
scen <- "SC4.NoPlant.Prio.Lsalv"
scenDDx <- fig.ratios(scen)
scen <- "SC5.WPlant.Post.Hsalv"
scenEEx<- fig.ratios(scen)
scen <- "SC6.WPlant.Prio.Hsalv"
scenFFx <- fig.ratios(scen)
scen <- "SC7.NoPlant.Post.Hsalv"
scenGGx <- fig.ratios(scen)
scen <- "SC8.NoPlant.Prio.Hsalv"
scenHHx <- fig.ratios(scen)


###########################################
# ratio

zone.feu <- "ZA"
var.ratio = "zon.cl3"

scenAAx2 <- scenAAx[scenAAx$Zone  %in% zone.feu, names(scenAAx)  %in%  c("time",var.ratio)]
scenBBx2 <- scenBBx[scenBBx$Zone  %in% zone.feu,names(scenBBx)  %in%  c("time",var.ratio)]
scenCCx2 <- scenCCx[scenCCx$Zone  %in% zone.feu,names(scenCCx)  %in%  c("time",var.ratio)]
scenDDx2 <- scenDDx[scenDDx$Zone  %in% zone.feu,names(scenDDx)  %in%  c("time",var.ratio)]
scenEEx2 <- scenEEx[scenEEx$Zone  %in% zone.feu,names(scenEEx)  %in%  c("time",var.ratio)]
scenFFx2 <- scenFFx[scenFFx$Zone  %in% zone.feu,names(scenFFx)  %in%  c("time",var.ratio)]
scenGGx2 <- scenGGx[scenGGx$Zone  %in% zone.feu,names(scenGGx)  %in%  c("time",var.ratio)]
scenHHx2 <- scenHHx[scenHHx$Zone  %in% zone.feu,names(scenHHx)  %in%  c("time",var.ratio)]

plot(scenAAx2, t="l",ylim=c(0.0,0.99),col=1)
lines(scenBBx2,col=1)
lines(scenCCx2,col=2)
lines(scenDDx2,col=2)
lines(scenEEx2,col=3)
lines(scenFFx2,col=3)
lines(scenGGx2,col=3)
lines(scenHHx2,col=3)

ratioZ1 <- colMeans(scenAAx[scenAAx$Zone  %in% "ZA",3:8], na.rm=T)

ratioZ2 <- colMeans(scenAAx[scenAAx$Zone  %in% "ZB",3:8], na.rm=T)

ratioZ3 <- colMeans(scenAAx[scenAAx$Zone  %in% "ZC",3:8], na.rm=T)

ratioZ4 <- colMeans(scenAAx[scenAAx$Zone  %in% "ZD",3:8], na.rm=T)


par(mfrow=c(2,2), oma=c(4, 2, 2, 2),mar=c(1,4,1,0.3))

plot(ratioZ1[1:3],t="l", ylim=c(0,0.8),ylab="",xaxt='n' ,col=2)
lines(ratioZ1[4:6],col=1)
legend("bottomright", legend=c("Sélection simulée","Sélection aléatoire"),
       cex=1,lty=1,col=c(2,1),lwd=1, bty="n",seg.len=5)   
mtext("Zone A - 193 334 km2", side=3,  line=-2, cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("","","")),at=c(1,2,3),las=0, tck = -0.02) 

plot(ratioZ2[1:3],t="l", ylim=c(0,0.8),ylab="",xaxt='n',col=2)
lines(ratioZ2[4:6],col=1)
mtext("Zone B - 291 388 km2", side=3,  line=-2, cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("","","")),at=c(1,2,3),las=0, tck = -0.02)

plot(ratioZ3[1:3],t="l", ylim=c(0,0.8),ylab="",xaxt='n',col=2)
lines(ratioZ3[4:6],col=1)
mtext("Zone C - 222 272 km2", side=3,  line=-2, cex=1.2,family="serif", font=1)
axis(side = 1,  labels=(c("Low","Interm.","High")),at=c(1,2,3),las=0, tck = -0.02) 

plot(ratioZ4[1:3],t="l", ylim=c(0,0.8),ylab="",xaxt='n',col=2)
lines(ratioZ4[4:6],col=1)
mtext("Zone D - 68 668 km2", side=3,  line=-2, cex=1.2,family="serif", font=1)
axis(side = 1,  labels=(c("Low","Interm.","High")),at=c(1,2,3),las=0, tck = -0.02) 

mtext("Proportion sélectionnée", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("Flammability (Stand types)", side=1,  line=2, outer = TRUE,cex=1.2,family="serif", font=1)
mtext("(31 janvier", side=3,  line=0, outer = TRUE,cex=1,family="serif", font=1)



rat.Z1 <- numeric(3)
rat.Z1[1] <- ratioZ1[1]-ratioZ1[4]
rat.Z1[2] <- ratioZ1[2]-ratioZ1[5]
rat.Z1[3] <- ratioZ1[3]-ratioZ1[6]



rat.Z2 <- numeric(3)
rat.Z2[1] <- ratioZ2[1]-ratioZ2[4]
rat.Z2[2] <- ratioZ2[2]-ratioZ2[5]
rat.Z2[3] <- ratioZ2[3]-ratioZ2[6]

rat.Z2 <- numeric(3)
rat.Z2[1] <- ratioZ2[1]-ratioZ2[4]
rat.Z2[2] <- ratioZ2[2]-ratioZ2[5]
rat.Z2[3] <- ratioZ2[3]-ratioZ2[6]

rat.Z3 <- numeric(3)
rat.Z3[1] <- ratioZ3[1]-ratioZ3[4]
rat.Z3[2] <- ratioZ3[2]-ratioZ3[5]
rat.Z3[3] <- ratioZ3[3]-ratioZ3[6]

rat.Z4 <- numeric(3)
rat.Z4[1] <- ratioZ4[1]-ratioZ4[4]
rat.Z4[2] <- ratioZ4[2]-ratioZ4[5]
rat.Z4[3] <- ratioZ4[3]-ratioZ4[6]

plot(rat.Z1,t="l", ylim=c(-0.2,0.2))
lines(rat.Z2)
lines(rat.Z3)
lines(rat.Z4)


