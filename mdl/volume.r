######################################################################################
###  volume()
###
###  Description >  Calculates harvested volume per FMU. Called in landscape.dyn.
###
###  Arguments >  
###   land : data frame of the state variables       
###   km2.pixel : number of km2 per pixel on the grid
###   min.time.step : the lowest common denominator of the disturbances time schedulings
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###
###  Details > Counts the cells in each age invterval of 20 years for each vegetation class.
###
###  Value > It writes a data frame with the assessment of age class per species group.
######################################################################################

volume <- function(land, km2.pixel){
  
  # call yield curves (three site indexes)
  courbes <-  read.table("C:/Users/boumav/Desktop/courbes_SI.txt", header=T)
  names(courbes) <- c("Age","SI","volume")
  
  # determine volume as a function of stand age and site index
  land.vol <- land
  land.vol$Age [land.vol$Age>150] <- 150
  land.vol$SI <- as.character(land.vol$EcoType2)
  land.vol$SI[land.vol$SI=="RE"] <- "SI9"
  land.vol$SI[land.vol$SI=="ter_co"] <- "SI9"
  land.vol$SI[land.vol$SI=="RS"] <- "SI12"
  land.vol$SI[land.vol$SI=="FE"] <- "SI19"  
  land.vol2 <- land.vol %>% inner_join(courbes, by = c("Age","SI"))
  
  ## Volume per species per management unit

    vol.out <- filter(land.vol2) %>%
    group_by(MgmtUnit, SppGrp, DistType) %>% summarize(x=sum(volume)*km2.pixel*100) 
  
return(vol.out=vol.out)
}
 