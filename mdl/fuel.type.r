fuel.type <- function(land, fuel.types.modif, fire.size.target=NA){
  
  ## Fuel type according species and age
  fuel <- data.frame(cell.id=land$cell.id, frz=land$frz)
  fuel$type[land$spp %in% c("BOJ", "ERS", "NonFor", "OTH.FEU.S", "OTH.FEU.N")] <- "low"
  fuel$type[land$spp == "PET"] <- "med"
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age<=40] <- "med"
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age>40] <- "high"
  
  ## Fire size target above which types of fuels do not make a difference in propagation
  ## (extreme weather)
  ## fire.size.target is NA when the function is used to compute initial baseline fuel load
  ## at the zone level
  fire.size.cat <- ifelse(is.na(fire.size.target) | fire.size.target<1000, "small", "large")
                          
  ## Choose the modifiers according to fire.size.target
  modif <- fuel.types.modif[fuel.types.modif$size==fire.size.cat,-3] %>% filter(!is.na(frz))
  
  ## Assign to each cell, the fuel modifier
  fuel <- left_join(fuel, modif, by=c("type", "frz")) #%>% select(-type)
    #   sum(is.na(fuel2$baseline))
  
  return(fuel)
}