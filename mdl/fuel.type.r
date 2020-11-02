fuel.type <- function(land, fuel.types.modif, fire.size.target=NA){ #fire.size.target <- 600
  options(warn=-1)
  fire.size.cat <- ifelse(fire.size.target<200,"small","large")
  fuel <- data.frame(cell.id=land$cell.id, zone=land$FRZone)
  fuel$type[land$SppGrp %in% c("BOJ", "ERS", "NonFor", "OthDT", "OthDB")] <- 1
  fuel$type[land$SppGrp == "PET"] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age<=40] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age>40] <- 3
  if(is.na(fire.size.target)){
    aux2 <- fuel.types.modif[fuel.types.modif$size=="large",-3]
    fuel2 <- left_join(fuel, na.omit(aux2), by=c("type", "zone")) %>% select(-type)
  }
  else{  # fire.size.target <- 600
    aux2 <- fuel.types.modif[fuel.types.modif$size==fire.size.cat,-3]
    fuel2 <- left_join(fuel, na.omit(aux2), by=c("type", "zone")) %>% select(-type)
  }
  return(fuel2)
}
