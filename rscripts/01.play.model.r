## TO DO DEBUGGING
  rm(list=ls())
  scn.name <- "Test02"  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(tictoc)
    library(raster)
    library(RANN)
    library(tidyverse)
  })
  source("mdl/wildfires.r")
  source("mdl/sbw.outbreak.r") 
  source("mdl/clear.cut.r") 
  source("mdl/partial.cut.r")   
  source("mdl/buffer.mig.r")            
  source("mdl/forest.transitions.r")  
  source("mdl/suitability.r") 
  source("mdl/fuel.type.r")  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
    source(paste0("outputs/", scn.name, "/scn.custom.def.r"))
  if(!file.exists(paste0(out.path, "/lyr")))
      dir.create(file.path(getwd(), out.path, "/lyr"), showWarnings = F) 
  ## Load MASK raster layer of the study area, and compute cell resolution in km2
  load(file="inputlyrs/rdata/mask.rdata")
  km2.pixel <- res(MASK)[1] * res(MASK)[2] / 10^6
  ## Load temperature and precipitation 5-year predictions according to the climatic scenario.
  clim.scn <- "rcp45"
  load(file=paste0("inputlyrs/rdata/temp_", clim.scn, "_ModCan.rdata")) 
  load(file=paste0("inputlyrs/rdata/precip_", clim.scn, "_ModCan.rdata"))  
  ## Build the discrete time sequence according to time.step
  time.seq <- seq(0, time.horizon, time.step) 
  ## Tracking data.frames  ( review description ARE WRONG!!)
  track.spp.frzone <- data.frame(run=NA, year=NA, FRZone=NA, SppGrp=NA, Area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, BCDomain=NA, SppGrp=NA, C20=NA, C40=NA, C60=NA, C80=NA, C100=NA, Cold=NA)
  track.suit.class <- data.frame(run=NA, year=NA, BCDomain=NA, PotSpp=NA, poor=NA, med=NA, good=NA)
  track.land.fuel <- data.frame(run=NA, year=NA, zone=NA, x=NA)
  track.fire.regime <- data.frame(run=NA, year=NA, zone=NA, nfires=NA, atarget=NA, aburnt=NA, fire.cycle=NA, indx.combust=NA)
  track.fires <- data.frame(run=NA, year=NA, zone=NA, fire.id=NA, wind=NA, atarget=NA, aburnt=NA)  
  track.fuels <- data.frame(run=NA, year=NA, zone=NA, flam=NA, pctg.zone=NA, pctg.burnt=NA)
  track.ccut <- data.frame(run=NA, year=NA, UA=NA, tot.inc=NA, even.age=NA, sup.mat=NA, s.inc.burnt=NA, s.inc.mat.burnt=NA, s.inc.kill=NA, s.inc.mat.kill=NA,
                           cc.area.unaff=NA, cc.area.salvaged=NA, harv.cc.EPN=NA, harv.cc.BOJ=NA, harv.cc.PET=NA, harv.cc.SAB=NA, 
                           harv.cc.ERS=NA, harv.cc.OthCB=NA, harv.cc.OthDB=NA, harv.cc.OthCT=NA, harv.cc.OthDT=NA, reg.fail.ex=NA, reg.fail.inc=NA)
  ## Start the simulations
  irun <- 1
  load(file="inputlyrs/rdata/land.rdata")
  fire.schedule <- seq(0, time.horizon, fire.step)
  cc.schedule <- seq(0, time.horizon, cc.step)
  pc.schedule <- seq(0, time.horizon, pc.step)
  sbw.schedule <- seq(sbw.step, time.horizon, sbw.step)
  ## Record initial species distribution per fire zone, and distribution of age classes per BCDomain
  track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=0,  group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
  track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=0, group_by(land, BCDomain, SppGrp) %>% summarize(C20=sum(Age<=20)*km2.pixel, 
  C40=sum(Age<=40)*km2.pixel, C60=sum(Age<=60)*km2.pixel, C80=sum(Age<=80)*km2.pixel, C100=sum(Age<=100)*km2.pixel, Cold=sum(Age>100)*km2.pixel)))
  ## Record initial suitability classes per BCDomain 
  suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
  aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
    group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
  track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=0, aux))
  rm(suitab); rm(aux)
  ## Calculate the baseline fuel at the fire regime zone and track it
  baseline.fuel <- group_by(fuel.type(land, fuel.types.modif), zone) %>% summarize(x=mean(baseline))
  track.land.fuel <- rbind(track.land.fuel, data.frame(run=irun, year=0, baseline.fuel))
  ## Matrix to save the sustained yield level 
  ref.harv.level <- table(land$MgmtUnit)*NA
  ## Start 
  t <- 0





play.landscape.dyn <- function(){
  rm(list=ls())
  # setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
  setwd("C:/work/qbcmod/QbcLDM")
  source("mdl/define.scenario.r")
  source("mdl/landscape.dyn.r")  
  scn.name <- "Test02"
  define.scenario(scn.name)
  pigni.opt <- "static"
  nrun <- 1
  write.sp.outputs <- F
  pb.upper.th <- 0.75 
  pb.lower.th <- -1 
  dump(c("nrun", "pigni.opt", "write.sp.outputs", "pb.lower.th", "pb.upper.th"), 
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
  build.pigni(work.path, lambda=0.03)
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



