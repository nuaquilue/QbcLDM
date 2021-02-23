############################################ RUN A SCN ##################################################
rm(list=ls())
# setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
source("mdl/define.scenario.r"); source("mdl/landscape.dyn.r")  
scn.name <- "Test_nothing45"
define.scenario(scn.name)
nrun <- 1
write.maps <- F
plot.fires <- T
time.horizon <- 5 #2100-2020
clim.scn <- "rcp45"
wflam <- 1
wwind <- 0
is.wildfires <- T
is.sbw <- F
is.clearcut <- F
is.partialcut <- F
replanif <- 1
th.small.fire <- 1000 ## all small
dump(c("nrun",  "write.maps", "plot.fires", "time.horizon", "clim.scn", "is.wildfires", "is.sbw", 
       "is.clearcut", "is.partialcut", "replanif", "th.small.fire", "wflam", "wwind"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
landscape.dyn(scn.name)


############################################ RUN A SET OF SCN ##################################################
library(readxl)
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/landscape.dyn.r")  
scenarios <- read_xlsx("Scenarios.xlsx", sheet="Obj1")
for(i in 7:9){
  scn.name <- scenarios$scn.name[i]
  define.scenario(scn.name)
  ## general
  nrun <- 1# scenarios$nrun[i]
  write.maps <- F
  plot.fires <- F
  ## fire parmeters
  wflam <- 1
  wwind <- 0
  rpb <- scenarios$rpb[i]
  ## processes
  is.wildfires <- as.logical(scenarios$is.wildfires[i])
  is.sbw <- as.logical(scenarios$is.sbw[i])
  is.clearcut <- as.logical(scenarios$is.clearcut[i])
  is.partialcut <- as.logical(scenarios$is.partialcut[i])
  ## modifiers of target area
  is.fuel.modifier <- as.logical(scenarios$is.fuel.modifier[i])
  is.clima.modifier <- as.logical(scenarios$is.clima.modifier[i])
  ## scenario parameters
  clim.scn <- ifelse(scenarios$clim.scn[i]=="NA", NA, scenarios$clim.scn[i])
  th.small.fire <- scenarios$th.small.fire[i]
  replanif <- 1
  dump(c("nrun", "write.maps", "plot.fires", "is.wildfires", "is.sbw", "is.clearcut", "is.partialcut", 
         "is.fuel.modifier", "is.clima.modifier", "clim.scn", "th.small.fire", "replanif", "wflam", "wwind", "rpb"), 
       paste0("outputs/", scn.name, "/scn.custom.def.r"))
  landscape.dyn(scn.name)
}



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
