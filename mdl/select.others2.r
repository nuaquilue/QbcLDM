# rm(list=ls())
# library(tidyverse)
# load(file="inputlyrs/rdata/land.rdata")
#target.cells.oth <- unique(subland$cell.id)[new.spp=="OTH"]
select.others <- function(land, target.cells.oth){

  
  target.cells.oth.xy <- land[land$cell.id %in% target.cells.oth,c("x","y")]
  
  micro.OthCB <- land[land$spp%in% c("OTH.RES.N"),]
  list.cell.buff <- nn2(micro.OthCB[,c("x","y")], target.cells.oth.xy, k=40 , searchtype='priority')
  vec.OthCB <- rowSums(list.cell.buff$nn.dists < 20000)  
  
  micro.OthCT <- land[land$spp%in% c("OTH.RES.S"),]
  list.cell.buff <- nn2(micro.OthCT[,c("x","y")], target.cells.oth.xy, k=40 , searchtype='priority')
  vec.OthCT <- rowSums(list.cell.buff$nn.dists < 20000)
  
  micro.OthDB <- land[land$spp%in% c("OTH.FEU.N"),]
  list.cell.buff <- nn2(micro.OthDB[,c("x","y")], target.cells.oth.xy, k=40 , searchtype='priority')
  vec.OthDB <- rowSums(list.cell.buff$nn.dists < 20000)
  
  micro.OthDT <- land[land$spp%in% c("OTH.FEU.S","ERS","BOJ"),]
  list.cell.buff <- nn2(micro.OthDT[,c("x","y")],target.cells.oth.xy, k=40 , searchtype='priority')
  vec.OthDT <- rowSums(list.cell.buff$nn.dists < 20000)
  
  count.oth <-   cbind(vec.OthCB,vec.OthCT,vec.OthDB,vec.OthDT)
  others <- c("OTH.RES.N","OTH.RES.S","OTH.FEU.N","OTH.FEU.S")
  
  # Choose others according to the number of others in the neighborhood
  spp <- others[apply(count.oth, 1, sample.oth)]

  return(spp)
}

sample.oth <- function(x){
  if(sum(x)==0) # if there aren't others in the neigh, assign either OthCB or OthDB,
                # that are everywhere and in equal proportion
    return(sample(c(1,3), 1, replace=F))
  else
    return(sample(1:4, 1, replace=F, prob=x))
}


