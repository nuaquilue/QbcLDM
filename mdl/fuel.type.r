fuel.type <- function(land, fuel.types.modif, fire.target.area=NA, th.small.fire=NA){
  
  ## Fuel type according species and age
  fuel <- data.frame(cell.id=land$cell.id, frz=land$frz)
  fuel$type[land$spp %in% c("BOJ", "ERS", "NonFor", "OTH.FEU.S")] <- "hardwood"
  fuel$type[land$spp %in% c("PET", "OTH.FEU.N")] <- "pioneer" 
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age<=40] <- "young.conif"
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age>40] <- "mature.conif"
  fuel$type <- as.factor(fuel$type)
  
  ## Fire size target above which types of fuels do not make a difference in propagation
  ## (extreme weather)
  ## fire.target.area is NA when the function is used to compute initial baseline fuel load at the zone level
  ## otherwise, fire.target.area is in pixels and small fires are those of size <= 200 km2 = 50 pixels
  fire.cat <- ifelse(is.na(fire.target.area) | fire.target.area<=th.small.fire, "small", "large")  
                          
  ## Choose the modifiers according to fire.size.target
  modif <- fuel.types.modif[fuel.types.modif$size==fire.cat,-3] %>% filter(!is.na(frz))
  
  ## Assign to each cell, the fuel modifier
  fuel <- left_join(fuel, modif, by=c("type", "frz")) 
  
  return(fuel)
}