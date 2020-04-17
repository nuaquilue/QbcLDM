read.static.vars <- function(){
  
  ## PROBABILITY OF FIRE IGNITION
  load(file="inputlyrs/rdata/land.rdata")
  pigni <- data.frame(cell.id=land$cell.id, p=runif(nrow(land),0,1))
  save(pigni, file=("inputlyrs/rdata/pigni.rdata"))
}