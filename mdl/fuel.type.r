fuel.type <- function(land, fuel.types.modif, fire.size.target=NA){
  options(warn=-1)
  fuel <- data.frame(cell.id=land$cell.id, zone=land$FRZone)
  fuel$type[land$SppGrp %in% c("BOJ", "ERS", "NonFor", "OthDT", "OthDB")] <- 1
  fuel$type[land$SppGrp == "PET"] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age<=40] <- 2
  fuel$type[land$SppGrp %in% c("EPN", "SAB", "OthCT", "OthCB") & land$Age>40] <- 3
  if(is.na(fire.size.target)){
    aux <- filter(fuel.types.modif, is.na(size)) %>% select(type, baseline)
    fuel <- left_join(fuel, aux, by="type") %>% select(-type)
  }
  else{
    aux <- filter(fuel.types.modif, !is.na(size), fire.size.target>size) %>% select(-size)
    fuel <- left_join(fuel, aux, by=c("type", "zone")) %>% select(-type)
  }
  return(fuel)
}
