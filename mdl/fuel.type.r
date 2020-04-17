fuel.type <- function(land, fuel.types.modif){
  fuel <- data.frame(cell.id=land$cell.id, zone=land$FRZone)
  fuel$type[land$SppGrp %in% c("BOJ","ERS","NonFor","other")] <- 1
  fuel$type[land$SppGrp == "PET"] <- 2
  fuel$type[land$SppGrp %in% c("EPN","SAB") & land$Age<=40] <- 2
  fuel$type[land$SppGrp %in% c("EPN","SAB") & land$Age>40] <- 3
  fuel <- left_join(fuel, fuel.types.modif, by="type")  %>% select(-type)
  return(fuel)
}