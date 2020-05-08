######################################################################################
###  fuel.types()
###
###  Description >  Assigns a fuel type to each cell. 
###  Arguments >  
###   
###   
###  Details > Called in the landscape.dyn module (for baseline landscape-level conditions, t=0)
###            Called in the disturbance.fire module  (for spread, for ignition, and for landscape-level change
###                                                   from baseline)
###
###  Value > Returns a vector, for non NA cells
######################################################################################

fuel.types <- function(subland,fuel.types.modif){

  # 3 types de combustibles sont définis.
  vec.low.flam <- subland$SppGrp %in% c("BOJ","ERS","NonFor","other")
  vec.inter.flam <-  subland$SppGrp %in% c("PET") | (subland$SppGrp %in% c("EPN","SAB") & subland$TSD <= 40)
  vec.high.flam <-  subland$SppGrp %in% c("EPN","SAB") & subland$TSD > 40

  # 
  fuel.types2 <- (vec.low.flam*fuel.types.modif[1]) + 
                 (vec.inter.flam*fuel.types.modif[2]) + 
                 (vec.high.flam*fuel.types.modif[3])
 
  return(fuel.types2)
 
}
