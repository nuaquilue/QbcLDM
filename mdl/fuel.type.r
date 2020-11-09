fuel.type <- function(land, fuel.types.modif, fire.size.target=NA){
  
  ## Fuel type according species and age
  fuel <- data.frame(cell.id=land$cell.id, zone=land$FRZone)
  fuel$type[land$SppGrp %in% c("BOJ", "ERS", "NonFor", "OthDT", "OthDB")] <- 1
  fuel$type[land$SppGrp == "PET"] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age<=40] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age>40] <- 3
  
  ## Fire size target above which types of fuels do not make a difference in propagation
  ## (extreme weather)
  ## fire.size.target is NA when the function is used to compute initial baseline fuel load
  ## at the zone level
  fire.size.cat <- ifelse(is.na(fire.size.target) | fire.size.target<1000, "small", "large")
                          
  ## Choose the modifiers according to fire.size.target
  modif <- fuel.types.modif[fuel.types.modif$size==fire.size.cat,-3]
  
  ## Assign to each cell, the fuel modifier
  fuel <- left_join(fuel, modif, by=c("type", "zone")) %>% select(-type)
    #   sum(is.na(fuel2$baseline))
  
  return(fuel)
}