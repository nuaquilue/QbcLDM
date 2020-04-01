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

setwd("C:/Users/boumav/Desktop/QLD_V2")

fig.feu <- function(scen){ 
  sc_AA <- read.table(paste0("outputs/", scen, "/FireRegime.txt"),header=TRUE)
  sc_AA2a <- aggregate(sc_AA$ind.combust,list(sc_AA$time,sc_AA$zone),mean)
  sc_AA2b <- aggregate(sc_AA$n.fires,list(sc_AA$time,sc_AA$zone),mean)
  sc_AA2c <- aggregate(sc_AA$burnt.area,list(sc_AA$time,sc_AA$zone),mean)
  sc_AA2d <- aggregate(sc_AA$fire.cycle,list(sc_AA$time,sc_AA$zone),mean)
  sc_AA2a$x <- sc_AA2a$x
  sc_AA2a$n.feux <-  sc_AA2b$x
  sc_AA2a$burnt.area <-  (sc_AA2c$x * 100)/5
  sc_AA2a$fire.cycle <-  (sc_AA2d$x)
  return(sc_AA2a)
}

###########################
### Figure volumes récoltés par coupe totale

scen.UA <- c("2751","2551","2371","2471")

scen <- "SC1.WPlant.Post.Lsalv"
scenAAx<- fig.feu(scen)
scen <- "SC2.Wplant.Prio.Lsalv"
scenBBx <- fig.feu(scen)
scen <- "SC3.NoPlant.Post.Lsalv"
scenCCx <- fig.feu(scen)
scen <- "SC4.NoPlant.Prio.Lsalv"
scenDDx <- fig.feu(scen)
scen <- "SC5.WPlant.Post.Hsalv"
scenEEx<- fig.feu(scen)
scen <- "SC6.WPlant.Prio.Hsalv"
scenFFx <- fig.feu(scen)
scen <- "SC7.NoPlant.Post.Hsalv"
scenGGx <- fig.feu(scen)
scen <- "SC8.NoPlant.Prio.Hsalv"
scenHHx <- fig.feu(scen)
scen <- "SC10.NoHarvProt"
scenJJx <- fig.feu(scen)

###########################################
# indice combustible

zone.feu <- "ZA"

scenAAx2 <- scenAAx[scenAAx$Group.2  %in% zone.feu,c(1,3)]
scenBBx2 <- scenBBx[scenBBx$Group.2  %in% zone.feu,c(1,3)]
scenCCx2 <- scenCCx[scenCCx$Group.2  %in% zone.feu,c(1,3)]
scenDDx2 <- scenDDx[scenDDx$Group.2  %in% zone.feu,c(1,3)]
scenEEx2 <- scenEEx[scenEEx$Group.2  %in% zone.feu,c(1,3)]
scenFFx2 <- scenFFx[scenFFx$Group.2  %in% zone.feu,c(1,3)]
scenGGx2 <- scenGGx[scenGGx$Group.2  %in% zone.feu,c(1,3)]
scenHHx2 <- scenHHx[scenHHx$Group.2  %in% zone.feu,c(1,3)]
scenJJx2 <- scenJJx[scenJJx$Group.2  %in% zone.feu,c(1,3)]

plot(scenAAx2, t="l",ylim=c(0.2,0.63),col=1)
lines(scenBBx2,col=1)
lines(scenCCx2,col=2)
lines(scenDDx2,col=2)
lines(scenEEx2,col=3)
lines(scenFFx2,col=3)
lines(scenGGx2,col=3)
lines(scenHHx2,col=3)
lines(scenIIx2,col=1, lwd=2)
mtext("Zone D", side=3,  line=0, outer = F,cex=1,family="serif", font=1)


#########################################################################
### nombre de feux

zone.feu <- "ZA"

scenAAx2 <- scenAAx[scenAAx$Group.2  %in% zone.feu,c(1,4)]
scenBBx2 <- scenBBx[scenBBx$Group.2  %in% zone.feu,c(1,4)]
scenCCx2 <- scenCCx[scenCCx$Group.2  %in% zone.feu,c(1,4)]
scenDDx2 <- scenDDx[scenDDx$Group.2  %in% zone.feu,c(1,4)]
scenEEx2 <- scenEEx[scenEEx$Group.2  %in% zone.feu,c(1,4)]
scenFFx2 <- scenFFx[scenFFx$Group.2  %in% zone.feu,c(1,4)]
scenGGx2 <- scenGGx[scenGGx$Group.2  %in% zone.feu,c(1,4)]
scenHHx2 <- scenHHx[scenHHx$Group.2  %in% zone.feu,c(1,4)]


plot(scenAAx2, t="l",ylim=c(0,20),col=1)
lines(scenBBx2,col=1)
lines(scenCCx2,col=2)
lines(scenDDx2,col=2)
lines(scenEEx2,col=1)
lines(scenFFx2,col=1)
lines(scenGGx2,col=2)
lines(scenHHx2,col=2)
mtext("Zone D", side=3,  line=0, outer = F,cex=1,family="serif", font=1)


#########################################################################
### burnt.area

scenAAx2 <- scenAAx[scenAAx$Group.2  %in% zone.feu,c(1,5)]
scenBBx2 <- scenBBx[scenBBx$Group.2  %in% zone.feu,c(1,5)]
scenCCx2 <- scenCCx[scenCCx$Group.2  %in% zone.feu,c(1,5)]
scenDDx2 <- scenDDx[scenDDx$Group.2  %in% zone.feu,c(1,5)]
scenEEx2 <- scenEEx[scenEEx$Group.2  %in% zone.feu,c(1,5)]
scenFFx2 <- scenFFx[scenFFx$Group.2  %in% zone.feu,c(1,5)]
scenGGx2 <- scenGGx[scenGGx$Group.2  %in% zone.feu,c(1,5)]
scenHHx2 <- scenHHx[scenHHx$Group.2  %in% zone.feu,c(1,5)]

plot(scenAAx2, t="l",ylim=c(20000,200000),col=1)
lines(scenBBx2,col=1)
lines(scenCCx2,col=2)
lines(scenDDx2,col=2)
lines(scenEEx2,col=1)
lines(scenFFx2,col=1)
lines(scenGGx2,col=2)
lines(scenHHx2,col=2)
mtext("Zone D", side=3,  line=0, outer = F,cex=1,family="serif", font=1)


#########################################################################
### fire.cycle

zone.feu <- "ZC"

scenAAx2 <- scenAAx[scenAAx$Group.2  %in% zone.feu,c(1,6)]
scenBBx2 <- scenBBx[scenBBx$Group.2  %in% zone.feu,c(1,6)]
scenCCx2 <- scenCCx[scenCCx$Group.2  %in% zone.feu,c(1,6)]
scenDDx2 <- scenDDx[scenDDx$Group.2  %in% zone.feu,c(1,6)]
scenEEx2 <- scenEEx[scenEEx$Group.2  %in% zone.feu,c(1,6)]
scenFFx2 <- scenFFx[scenFFx$Group.2  %in% zone.feu,c(1,6)]
scenGGx2 <- scenGGx[scenGGx$Group.2  %in% zone.feu,c(1,6)]
scenHHx2 <- scenHHx[scenHHx$Group.2  %in% zone.feu,c(1,6)]

plot(scenAAx2, t="l",ylim=c(0,800),col=1)
lines(scenBBx2,col=1)
lines(scenCCx2,col=2)
lines(scenDDx2,col=2)
lines(scenEEx2,col=1)
lines(scenFFx2,col=1)
lines(scenGGx2,col=3)
lines(scenHHx2,col=3)

#########################################################################
############ ratios
