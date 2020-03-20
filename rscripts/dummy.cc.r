dummy.cc <- function(name.resol){
  
  library(raster)
  
  # Load "land" data frame with the state variables
  load(file=paste0("inputlyrs/rdata/land", name.resol, ".rdata"))
  
  # Assume that :
  time.horizon <- 100; time.step <- 5
  
  # Build dummy temperature data for 'land' cells in a CC context
  # We assume a 4ºC increase in 100 years
  cc.temp <- data.frame(cell.indx = land$cell.indx, temp0=land$Temp)
  for(i in 3:(time.horizon/time.step+1))
    cc.temp[,i] <- cc.temp[,i-1]+runif(nrow(land), 0.15, 0.25)
  names(cc.temp)[3:ncol(cc.temp)] <- paste0("temp", 1: (ncol(cc.temp)-2))
  
  # Idem for precipitation
  cc.precip <- data.frame(cell.indx = land$cell.indx, precip0=land$Preci)
  for(i in 3:(time.horizon/time.step+1))
    cc.precip[,i] <- cc.precip[,i-1] + rnorm(nrow(land), mean=0, sd=30)
  names(cc.precip)[3:ncol(cc.precip)] <- paste0("precip", 1: (ncol(cc.precip)-2))

  # Save both data frames
  save(cc.temp, file=paste0("inputlyrs/rdata/cc.temp", name.resol, ".rdata"))
  save(cc.precip, file=paste0("inputlyrs/rdata/cc.precip", name.resol,".rdata"))
  
}