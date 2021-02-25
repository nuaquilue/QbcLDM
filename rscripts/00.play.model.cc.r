############################################ RUN A SCN ##################################################
rm(list=ls())
source("mdl/define.scenario.r"); source("mdl/landscape.dyn.r")  
scn.name <- "TestFires"
define.scenario(scn.name)
## general
nrun <- 1
write.maps <- F
plot.fires <- F
time.horizon <- 2100-2020
## processes
is.wildfires <- T
is.clearcut <- F
is.partialcut <- F
## modifiers of target area
is.fuel.modifier <- T     # if T, fuels flammability modifies target burnt area
is.clima.modifier <- T
## scenario parameters
clim.scn <- NA
replanif <- F
th.small.fire <- 50
wflam <- 1
wwind <- 0
pigni.opt <- "static.exp"
## run
dump(c("nrun", "write.maps", "plot.fires", "time.horizon", "is.wildfires", "is.clearcut", 
       "is.partialcut", "is.fuel.modifier", "is.clima.modifier", "clim.scn", "replanif",
       "th.small.fire", "wflam", "wwind", "pigni.opt"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
landscape.dyn(scn.name)

