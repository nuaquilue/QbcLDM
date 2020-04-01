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
# Changements de composition par domaine bioclimatique (superficie par type), 
# en fonction des scenarios

setwd("C:/Users/boumav/Desktop/QLD_V2")

fig.compo <- function(scen){ 
  sc_AA <- read.table(paste0("outputs/", scen, "/AgebySpp.txt"),header=TRUE)
  sc_AA2a <- aggregate(sc_AA$Tot,list(sc_AA$time,sc_AA$domain,sc_AA$spp),mean)
 # sc_AA2b <- aggregate(sc_AA$n.fires,list(sc_AA$time,sc_AA$zone),mean)
#  sc_AA2c <- aggregate(sc_AA$burnt.area,list(sc_AA$time,sc_AA$zone),mean)
#  sc_AA2d <- aggregate(sc_AA$fire.cycle,list(sc_AA$time,sc_AA$zone),mean)
  sc_AA2a$x <- round(sc_AA2a$x,0)
  return(sc_AA2a)
}

###########################
### Figure volumes récoltés par coupe totale

scen.UA <- c("2751","2551","2371","2471")

scen <- "SC1.WPlant.Post.Lsalv"
scen1x<- fig.compo(scen)
scen <- "SC2.Wplant.Prio.Lsalv"
scen2x <- fig.compo(scen)
scen <- "SC3.NoPlant.Post.Lsalv"
scen3x <- fig.compo(scen)
scen <- "SC4.NoPlant.Prio.Lsalv"
scen4x <- fig.compo(scen)
scen <- "SC5.WPlant.Post.Hsalv"
scen5x<- fig.compo(scen)
scen <- "SC6.WPlant.Prio.Hsalv"
scen6x <- fig.compo(scen)
scen <- "SC7.NoPlant.Post.Hsalv"
scen7x <- fig.compo(scen)
scen <- "SC8.NoPlant.Prio.Hsalv"
scen8x <- fig.compo(scen)
scen <- "SC9.TotProt"
scen9x <- fig.compo(scen)
scen <- "SC10.NoHarvProt"
scen10x <- fig.compo(scen)


###########################################
# Abondance essences

ess <- "BOJ"
domaine <- 1

scen1x2 <- scen1x[scen1x$Group.3  %in% ess & scen1x$Group.2 ==domaine,c(1,4)]
scen2x2 <- scen2x[scen2x$Group.3  %in% ess & scen2x$Group.2 ==domaine,c(1,4)]
scen3x2 <- scen3x[scen3x$Group.3  %in% ess & scen3x$Group.2 ==domaine,c(1,4)]
scen4x2 <- scen4x[scen4x$Group.3  %in% ess & scen4x$Group.2 ==domaine,c(1,4)]
scen5x2 <- scen5x[scen5x$Group.3  %in% ess & scen5x$Group.2 ==domaine,c(1,4)]
scen6x2 <- scen6x[scen6x$Group.3  %in% ess & scen6x$Group.2 ==domaine,c(1,4)]
scen7x2 <- scen7x[scen7x$Group.3  %in% ess & scen7x$Group.2 ==domaine,c(1,4)]
scen8x2 <- scen8x[scen8x$Group.3  %in% ess & scen8x$Group.2 ==domaine,c(1,4)]
scen9x2 <- scen9x[scen9x$Group.3  %in% ess & scen9x$Group.2 ==domaine,c(1,4)]
scen10x2 <- scen10x[scen10x$Group.3  %in% ess & scen10x$Group.2 ==domaine,c(1,4)]

plot(scen1x2, t="l",ylim=c(0,200000),col=1)
lines(scen2x2,col=1)
lines(scen3x2,col=2)
lines(scen4x2,col=2)
lines(scen5x2,col=1)
lines(scen6x2,col=1)
lines(scen7x2,col=2)
lines(scen8x2,col=2)
lines(scen9x2,col=3,lwd=2)
lines(scen10x2,col=3,lwd=2, lty=2)
mtext("Domaine pessière - Boul jaune", side=3,  line=-1, outer = F,cex=1,family="serif", font=1)

