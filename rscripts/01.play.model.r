############################################ RUN A SCN ##################################################
rm(list=ls())
# setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
source("mdl/define.scenario.r")
source("mdl/landscape.dyn.r")  
scn.name <- "Test_nothing"
define.scenario(scn.name)
nrun <- 1
write.maps <- F
is.wildfires <- T
is.sbw <- F
is.clearcut <- T
is.partialcut <- T
replanif <- 1
pigni.opt <- "static.exp"
dump(c("nrun",  "write.maps", "is.wildfires", "is.sbw", "is.clearcut", "is.partialcut", 
       "pigni.opt", "replanif"), paste0("outputs/", scn.name, "/scn.custom.def.r"))
landscape.dyn(scn.name)


##################################### BUILD INITIAL CONDITIONS ###########################################
rm(list=ls())
# work.path <- "C:/Users/boumav/Desktop/QLandscapeDynamics1"
work.path <- "C:/WORK/QBCMOD/"
source("mdl/read.state.vars.r")
read.state.vars(work.path)
source("mdl/build.pigni.r")
build.pigni(work.path, lambda=0.04, r=0.95, first.time=F)



##################################### CHANGE RESOLUTION SPATIAL INPUTS ###########################################
library(RColorBrewer)
setwd("C:/Users/boumav/Desktop/LandscapeDynamics2")
source("mdl/change.spinput.resol.r")
# load original data
load(file="inputlyrs/rdata/mask.rdata") 
load(file="inputlyrs/rdata/sp.input.rdata") 
load(file="inputlyrs/rdata/land.rdata") 
change.spinput.resol(MASK, sp.input, land, factor=4, is.climate.change=T)
# look at what commes up
load("inputlyrs/rdata/mask.factor4.rdata")
res(MASK)
load("inputlyrs/rdata/sp.input.factor4.rdata")
plot(sp.input$FRZone, col=brewer.pal(4, "Set1"))
load("inputlyrs/rdata/land.factor4.rdata")
load("inputlyrs/rdata/cc.temp.factor4.rdata")



##################################### PLOT  SPATIAL INPUTS ###########################################
load(file="inputlyrs/rdata/sp.input.rdata")
library(viridis)
library(rasterVis)
levelplot(sp.input$FRZone, margin=FALSE, colorkey=T, par.settings=magmaTheme())
levelplot(sp.input$MgmtUnit, margin=FALSE, colorkey=T, par.settings=plasmaTheme())
levelplot(sp.input$SppGrp, margin=FALSE, colorkey=T, par.settings=plasmaTheme())
levelplot(sp.input$Temp, margin=FALSE, colorkey=T, par.settings=viridisTheme())
levelplot(sp.input$Precip, margin=FALSE, colorkey=T, par.settings=viridisTheme())
levelplot(sp.input$SoilType, margin=FALSE, colorkey=T, par.settings=magmaTheme())
levelplot(sp.input$Exclus, margin=FALSE, colorkey=T, par.settings=magmaTheme())
