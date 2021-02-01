play.landscape.dyn <- function(){
  rm(list=ls())
  # setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
  setwd("C:/work/qbcmod/QbcLDM")
  source("mdl/define.scenario.r")
  source("mdl/landscape.dyn.r")  
  scn.name <- "Test_nothing"
  define.scenario(scn.name)
  nrun <- 1
  write.sp.outputs <- T
  clim.scn <- "rcp85"
  is.wildfires <- F
  is.sbw <- F
  is.clearcut <- F
  is.partialcut <- F
  replanif <- 1
  pigni.opt <- "static.exp"
  dump(c("nrun",  "write.sp.outputs", "clim.scn", "is.wildfires", "is.sbw", "is.clearcut", "is.partialcut", 
         "pigni.opt", "replanif"), 
       paste0("outputs/", scn.name, "/scn.custom.def.r"))
  landscape.dyn(scn.name)
  
}


play.read.state.vars <- function(){
  rm(list=ls())
  # work.path <- "C:/Users/boumav/Desktop/QLandscapeDynamics1"
  work.path <- "C:/WORK/QBCMOD/"
  source("mdl/read.state.vars.r")
  read.state.vars(work.path)
  source("mdl/build.pigni.r")
  build.pigni(work.path, lambda=0.04, r=0.95, first.time=F)
}


play.change.spinput.resol <- function(){
  rm(list=ls())
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
  head(land)
  load("inputlyrs/rdata/cc.temp.factor4.rdata")
  head(land.factor)
}


write.plot.sp.input <- function(){
  rm(list=ls())
  setwd("C:/work/qbcmod/QbcLDM")
  load(file="inputlyrs/rdata/sp.input.rdata")
  writeRaster(sp.input$FRZone, "inputlyrs/asc/FRZone.asc", format="ascii", overwrite=T)
  writeRaster(sp.input$BCDomain, "inputlyrs/asc/BCDomain.asc", format="ascii", overwrite=T)
  writeRaster(sp.input$MgmtUnit, "inputlyrs/asc/MgmtUnit.asc", format="ascii", overwrite=T)    
  writeRaster(sp.input$SppGrp, "inputlyrs/asc/SppGrp_t0.asc", format="ascii", overwrite=T)    
  writeRaster(sp.input$Age, "inputlyrs/asc/Age_t0.asc", format="ascii", overwrite=T)    
  plot(sp.input$FRZone)
  plot(sp.input$BCDomain)
  plot(sp.input$MgmtUnit)
  plot(sp.input$SppGrp)
  plot(sp.input$Temp)
  plot(sp.input$Precip)
  plot(sp.input$SoilType)
  plot(sp.input$Exclus)
}


plot.prob.igni <- function(){
  rm(list=ls())
  
  ## Assign probability of ignition p=1/(1+exp(d))
  dist <- seq(0,50,1)
  r <- 0.95
  prob.igni <- r/(1+exp(scales::rescale(dist, to=c(-3, 2), from=c(0,40))))
  plot(prob.igni~dist, type="l", ylim=c(0,1))
  abline(h=0, col="blue", lty=3)
  abline(h=1, col="blue", lty=3)
  k <- c(5,10,20,30,40)
  a <- r/(1+exp(scales::rescale(k, to=c(-3, 2), from=c(0,40))))
  points(a~k, col="red")
  
  ## Assign probability of ignition p=exp(-lamda* d)
  lambda <- 0.04
  dist <- seq(0,50,1)
  prob.igni <- exp(-lambda*dist)
  plot(prob.igni~dist, type="l", ylim=c(0,1))
  abline(h=0, col="blue", lty=3)
  abline(h=1, col="blue", lty=3)
  k <- c(5,10,20,30,40)
  a <- exp(-lambda*k)
  points(a~k, col="red")
  
  # plot maps
  library(viridis)
  load(file="inputlyrs/rdata/mask.rdata")
  #  p=exp(-lamda* d)
  load(file="inputlyrs/rdata/pigni_static.exp.rdata")
  PIGNI <- MASK 
  PIGNI[!is.na(MASK[])] <- pigni$p
  plot(PIGNI, col=viridis(6), zlim=c(0,0.9))
  #  p=1/(1+exp(d))
  load(file="inputlyrs/rdata/pigni_static.nexp.rdata")
  PIGNI[!is.na(MASK[])] <- pigni$p
  plot(PIGNI, col=viridis(6), zlim=c(0,0.9))
  
}

