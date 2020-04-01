play.landscape.dyn <- function(){
  setwd("C:/Users/boumav/Desktop/LandscapeDynamics3_nu/rscripts")
  #C:\Users\boumav\Desktop\LandscapeDynamics2\rscripts
  scn.name <- "Test02"
  # set the scenario
  source("mdl/define.scenario.r")
  define.scenario(scn.name)
  # run the model
  source("mdl/landscape.dyn.r")  
  landscape.dyn(scn.name)  
}


play.initalize.study.area <- function(){
  rm(list=ls())
  setwd("C:/Users/boumav/Desktop/QLandscapeDynamics1")
  source("mdl/initialize.study.area4.r")
  lyr <- "inputlyrs/dbf/points_2k2_ll"
  neighborhood <- 4
  #initialize.study.area(lyr, neighborhood, plot.raster=T)
  initialize.study.area(lyr, neighborhood, plot.raster=T, file.mask.layer=NULL,
  file.land.df=NULL, file.buff=NULL, file.neighs.ft=NULL, file.neighs.fn=NULL, file.sp.input=NULL)
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


play.plot.sp.input <- function(){
  setwd("C:/Users/boumav/Desktop/LandscapeDynamics2")
  load(file="inputlyrs/rdata/sp.input.rdata") 
  plot(sp.input$FRZone)
  plot(sp.input$BCDomain)
  plot(sp.input$MgmtUnit)
  plot(sp.input$SppGrp)
  plot(sp.input$TSD)
  plot(sp.input$Temp)
  plot(sp.input$Precip)
  plot(sp.input$SoilType)
  plot(sp.input$Exclus)
}



