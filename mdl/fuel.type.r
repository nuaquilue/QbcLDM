fuel.type <- function(land, fuel.types.modif, fire.size.target=NA){
  options(warn=-1)
  # size above which types of fuels do not make a difference in propagation
  # (extreme weather)
  fire.size.cat <- ifelse(fire.size.target<1000,"small","large")
  fuel <- data.frame(cell.id=land$cell.id, zone=land$FRZone)
  fuel$type[land$SppGrp %in% c("BOJ", "ERS", "NonFor", "OthDT", "OthDB")] <- 1
  fuel$type[land$SppGrp == "PET"] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age<=40] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age>40] <- 3
  if(is.na(fire.size.target)){
    # to have fuel types under initial conditions
    aux2 <- fuel.types.modif[fuel.types.modif$size=="small",-3]
    fuel2 <- left_join(fuel, na.omit(aux2), by=c("type", "zone")) %>% select(-type)
  }  else{  
    aux2 <- fuel.types.modif[fuel.types.modif$size==fire.size.cat,-3]
    fuel2 <- left_join(fuel, na.omit(aux2), by=c("type", "zone")) %>% select(-type)
  }
  sum(is.na(fuel2$baseline))
  return(fuel2)
}