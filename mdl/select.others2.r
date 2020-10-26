# rm(list=ls())
# library(tidyverse)
# load(file="inputlyrs/rdata/land.rdata")
#target.cells.oth <- unique(subland$cell.id)[new.spp=="OTH"]
select.others <- function(land, target.cells.oth){
  ## 39 neigbors (star of 4)
  neigh <- c(-3220, -2416, -2415, -2414, -1612, -1611, - 1610, -1609, -1608, -808, -807, -806, -805, -804, - 803, -802, -4
             -3, -2, -1, 1, 2, 3, 4, 802, 803, 804, 805, 806, 807, 808, 1608, 1609, 1610, 1611, 1612, 2414, 2415, 2416, 3220)
  
  target.cells.oth.xy <- land[land$cell.id %in% target.cells.oth,c("x","y")]
  micro.OthCB <- land[land$SppGrp%in% c("OthCB"),]
  list.cell.buff <- nn2(micro.OthCB[,c("x","y")], target.cells.oth.xy, 
                        k=40 , searchtype='priority')
  vec.OthCB <- rowSums(list.cell.buff$nn.dists < 20000)  
  
  micro.OthCT <- land[land$SppGrp%in% c("OthCT"),]
  list.cell.buff <- nn2(micro.OthCT[,c("x","y")], target.cells.oth.xy, 
                        k=40 , searchtype='priority')
  vec.OthCT <- rowSums(list.cell.buff$nn.dists < 20000)
  
  micro.OthDB <- land[land$SppGrp%in% c("OthDB"),]
  list.cell.buff <- nn2(micro.OthDB[,c("x","y")], target.cells.oth.xy, 
                        k=40 , searchtype='priority')
  vec.OthDB <- rowSums(list.cell.buff$nn.dists < 20000)
  
  micro.OthDT <- land[land$SppGrp%in% c("OthDT","ERS","BOJ"),]
  list.cell.buff <- nn2(micro.OthDT[,c("x","y")],target.cells.oth.xy, 
                        k=40 , searchtype='priority')
  vec.OthDT <- rowSums(list.cell.buff$nn.dists < 20000)
  
  count.oth <-   cbind(vec.OthCB,vec.OthCT,vec.OthDB,vec.OthDT)
  
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


