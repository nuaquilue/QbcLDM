######################################################################################
###  age.by.spp()
###
###  Description >  Report the age class abundance per vegetation type. Called in landscape.dyn.
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

age.by.spp2 <- function(land, km2.pixel, min.time.step, irun, t){

  # Will only consider cells with vegetation or non-forested (NonFor).
  subland <- land[!is.na(land$SppGrp), ]                  
  
  # Number of Bioclimatic Domains (ecozones), Species Groups and Age Classes  
  nSppGrp <- length(levels(subland$SppGrp))
  nMgmtUnit <- length(levels(subland$FRZone))
  
  # Count the cells in each Age Class per Bioclimatic Domain (in pixels)
  # 'split' functions returns a list, each element corresponds to a Bioclimatic Domain (ecozone)
  # then 'tapply' assess the age distribution per Species Group, it returns a list of
  # as many elements as number of species groups. Each element is a vector with 6 values: 
  # the number of cells per age class   
  
  domains <- unique(subland$FRZone)
  tab_so <- table(subland$FRZone,subland$SppGrp)
  tab_so1 <- tab_so[,c(1,2,3,5,6,7)]
  tab_so2 <- round(tab_so1/rowSums(tab_so1)  ,3)
  tab_so2[rowSums(tab_so1)<15,] <- NA
  # Complete the data frame with the run and the time step, then write it in a text file
  age.spp2 <- data.frame(rep(irun, nrow(tab_so2)), rep(t, nrow(tab_so2)), tab_so2)
  names(age.spp2) <- c("run","time","FRZone", "spp", "Area")
  return(age.spp2)

}
 