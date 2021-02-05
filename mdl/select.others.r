# rm(list=ls())
# library(tidyverse)
# load(file="inputlyrs/rdata/land.rdata")

select.others <- function(land, target.cells){
  ## 39 neigbors (star of 4)
  neigh <- c(-3220, -2416, -2415, -2414, -1612, -1611, - 1610, -1609, -1608, -808, -807, -806, -805, -804, - 803, -802, -4
             -3, -2, -1, 1, 2, 3, 4, 802, 803, 804, 805, 806, 807, 808, 1608, 1609, 1610, 1611, 1612, 2414, 2415, 2416, 3220)
  neigh.id <- matrix(neigh, nrow=length(target.cells), ncol=length(neigh), byrow=T)
  ids <- data.frame(target.cells)
  for(i in 2:length(neigh))
    ids <- cbind(ids, target.cells)
  names(ids) <- paste0("n",1:length(neigh))
  neigh.id <- neigh.id+ids
  ids <- neigh.id+ids
  ## count n of Oth
  others <- c("OTH.RES.N","OTH.RES.S","OTH.FEU.N","OTH.FEU.S")
  count.oth <- matrix(0, nrow=length(target.cells), ncol=length(others))
  for(oth in others){
    x <- filter(land, SppGrp==oth) %>% select(cell.id)
    for(i in 1:length(neigh))
      neigh.id[,i] <- neigh.id[,i] %in% x$cell.id
    count.oth[,which(others == oth)] <- apply(neigh.id, 1, sum)
    neigh.id <- ids
  }
  
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


