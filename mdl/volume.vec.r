######################################################################################
###  # determine volume as a function of stand age and site index
######################################################################################

volume.vec <- function(land, courbes){
  
  land.vol <- land
  
  ## Volum max according to ecological type
  land.vol$vol.max <- substr(land.vol$eco.type, 1, 2)
  land.vol$vol.max <- ifelse(is.na(land.vol$vol.max), "V250",
                      ifelse(land.vol$vol.max=="RE", "V90",
                      ifelse(land.vol$vol.max=="RS", "V150",
                      ifelse(land.vol$vol.max=="FE", "V250", "V250"))))
  
  ## Maturity class
  land.vol$matu[land.vol$age.matu %in% c(40,45,50,60)] <- "M50"
  land.vol$matu[land.vol$age.matu %in% c(65,70,75,80)] <- "M70"
  land.vol$matu[land.vol$age.matu %in% c(85,90,95,100,105,110,120)] <- "M90"
  
  ## Volume per stand maturity class and site index
  land.vol <- land.vol %>% left_join(courbes, by = c("age","vol.max","matu"))
  
  ## Return vVolume per species per management unit
  return(vol.out=land.vol$vol)
}
 