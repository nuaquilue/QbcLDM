############################################ RUN A SCN ##################################################
rm(list=ls())
# setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
source("mdl/define.scenario.r"); source("mdl/landscape.dyn.r")  
scn.name <- "TestTime"
define.scenario(scn.name)
nrun <- 1
write.maps <- F
plot.fires <- F
time.horizon <- 2100-2020
wflam <- 0.7
wwind <- 0.3
is.wildfires <- T
is.clearcut <- T
is.partialcut <- T
replanif <- T
th.small.fire <- 50 ## -1 --> all same flammability
pigni.opt <- "light_static.exp"
dump(c("nrun", "write.maps", "plot.fires", "time.horizon", "is.wildfires", "replanif",
       "is.clearcut", "is.partialcut", "th.small.fire", "wflam", "wwind", "pigni.opt"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
landscape.dyn(scn.name)


############################################ RUN A SET OF SCN ##################################################
library(readxl)
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/landscape.dyn.r")  
scenarios <- read_xlsx("Scenarios.xlsx", sheet="Obj2")
for(i in 17:20){
  scn.name <- scenarios$scn.name[i]
  define.scenario(scn.name)
  ## general
  nrun <- scenarios$nrun[i]
  write.maps <- F
  plot.fires <- F
  ## processes
  is.wildfires <- as.logical(scenarios$is.wildfires[i])
  is.clearcut <- as.logical(scenarios$is.clearcut[i])
  is.partialcut <- as.logical(scenarios$is.partialcut[i])
  ## modifiers of target area
  is.fuel.modifier <- as.logical(scenarios$is.fuel.modifier[i])
  is.clima.modifier <- as.logical(scenarios$is.clima.modifier[i])
  ## scenario parameters
  clim.scn <- ifelse(scenarios$clim.scn[i]=="NA", NA, scenarios$clim.scn[i])
  replanif <- as.logical(scenarios$replanif[i])
  th.small.fire <- scenarios$th.small.fire[i]
  wflam <- scenarios$wflam[i]
  wwind <- scenarios$wwind[i]
  pigni.opt <- scenarios$pigni[i]
  dump(c("nrun", "write.maps", "plot.fires", "is.wildfires", "is.clearcut", "is.partialcut", 
         "is.fuel.modifier", "is.clima.modifier", "clim.scn", "replanif",
         "th.small.fire", "wflam", "wwind", "pigni.opt"), 
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
writeRaster(sp.input$SppGrp, "inputlyrs/tif/Spp_t0.tif", format="GTiff")


########################## PLOT PROBABILITY OF IGNITION ##########################
library(viridis)
library(rasterVis)
library(tidyverse)
load("inputlyrs/rdata/land.rdata")
load("inputlyrs/rdata/pigni_light_static.exp.rdata")
load("inputlyrs/rdata/mask.rdata")
aux <- data.frame(cell.id=land$cell.id[land$spp=="NonFor"], frz=land$frz[land$spp=="NonFor"], d=40)
aux <- rbind(select(pigni,-p), aux) 
aux$p <- exp(-0.05*aux$d)
  # aux$pn <- 1/(1+exp(scales::rescale(aux$d, to=c(-3, 2), from=c(0,40))))
aux <- aux[order(aux$cell.id),]
# plot
PIGNI <- MASK
PIGNI[!is.na(MASK[])] <- aux$p
levelplot(PIGNI, margin=FALSE, colorkey=T, par.settings=magmaTheme())
writeRaster(PIGNI, "C:/WORK/QBCMOD/DataOut/ProbIgni_lightfires_exp05.tif", format="GTiff")
# build and save
source("mdl/build.pigni.r")
build.pigni("C:/WORK/QBCMOD/", lambda=0.05, first.time=F)
