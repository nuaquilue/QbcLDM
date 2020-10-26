# rm(list=ls())
# library(tidyverse)
# load(file="inputlyrs/rdata/land.rdata")
#target.cells.oth <- unique(subland$cell.id)[new.spp=="OTH"]
select.others <- function(land, target.cells.oth){
  ## 39 neigbors (star of 4)
  neigh <- c(-3220, -2416, -2415, -2414, -1612, -1611, - 1610, -1609, -1608, -808, -807, -806, -805, -804, - 803, -802, -4
             -3, -2, -1, 1, 2, 3, 4, 802, 803, 804, 805, 806, 807, 808, 1608, 1609, 1610, 1611, 1612, 2414, 2415, 2416, 3220)
  neigh.id <- matrix(neigh, nrow=length(target.cells.oth), ncol=length(neigh), byrow=T)
  ids <- data.frame(target.cells.oth)
  for(i in 2:length(neigh))
    ids <- cbind(ids, target.cells.oth)
  names(ids) <- paste0("n",1:length(neigh))
  neigh.id <- neigh.id+ids
  ids <- neigh.id+ids
  ## count n of Oth
  others <- c("OthCB", "OthCT", "OthDB", "OthDT")
  count.oth <- matrix(0, nrow=length(target.cells.oth), ncol=length(others))
  for(oth in others){ # oth="OthDT"
    x <- filter(land, SppGrp==oth) %>% select(cell.id) # land$SppGrp
    for(i in 1:length(neigh)) # i=1
      neigh.id[,i] <- neigh.id[,i] %in% x$cell.id
    count.oth[,which(others == oth)] <- apply(neigh.id, 1, sum)
    neigh.id <- ids
  }
  
  # Choose others according to the number of others in the neighborhood
  spp <- others[apply(count.oth, 1, sample.oth)]
  # mathieu : quelque chose ne marche pas trop de 0. En attendant, 
  # maintien de la composition lorsqu'il n'y a pas de voisins
  #spp[rowSums(count.oth)==0] <- land[land$cell.id %in% target.cells.oth,]$SppGrp[rowSums(count.oth)==0]
  return(spp)
}

sample.oth <- function(x){
  if(sum(x)==0) # if there aren't others in the neigh, assign either OthCB or OthDB,
                # that are everywhere and in equal proportion
    return(sample(c(1,3), 1, replace=F))
  else
    return(sample(1:4, 1, replace=F, prob=x))
}


