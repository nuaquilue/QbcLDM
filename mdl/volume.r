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
  courbes <-  read.table("courbes_SI2.txt", header=T)
  names(courbes) <- c("Age","Matu","VolMax","volume")
  
  # determine volume as a function of stand age and site index
  land.vol <- land
  land.vol$Age [land.vol$Age>150] <- 150
  land.vol$VolMax <- as.character(land.vol$EcoType2)
  land.vol$VolMax[land.vol$VolMax=="RE"] <- "V90"
  land.vol$VolMax[land.vol$VolMax=="ter_co"] <- "V90"
  land.vol$VolMax[land.vol$VolMax=="RS"] <- "V150"
  land.vol$VolMax[land.vol$VolMax=="FE"] <- "V250"  
  
  land.vol$Matu <- land.vol$AgeMatu
  land.vol$Matu[land.vol$Matu %in% c(40,45,50,60)] <- "M50"
  land.vol$Matu[land.vol$Matu %in% c(65,70,75,80)] <- "M70"
  land.vol$Matu[land.vol$Matu %in% c(85,90,95,100,105,110)] <- "M90"
  
  land.vol2 <- land.vol %>% inner_join(courbes, by = c("Age","VolMax","Matu"))
  
  ## Volume per species per management unit

    vol.out <- filter(land.vol2) %>%
    group_by(MgmtUnit, SppGrp, DistType) %>% summarize(x=sum(volume)*km2.pixel*100) 
  
return(vol.out=vol.out)
}
 