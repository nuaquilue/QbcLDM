######################################################################################
###  neighour.spp()
###
###  Description >  Function that returns the forest composition of a circular 
###                 neighbourhood of radius R around the target cells
###                 Called in initialize.study.area
###
###  Arguments >  
###   microland : data frame with the cell.indx, SppGrp, CoordX, and Coord Y
###   target.cells : vector of cells to find the buffer around them
###   radius.neigh : radius in m of the circular neighbourhood to be draw around the target cells
###   km2.pixel <- size in km2 of the raster pixels
###
###  Details > 
###
###  Value >  Matrix with the cell.indx for target.cells (in rows) and the abundance of 
###           of the species in the neighbourhood (one per column)
######################################################################################

      # microland=land[, c("cell.indx", "SppGrp", "EcoType", "CoordX", "CoordY")]
      # target.cells=land[!is.na(land$SppGrp) & land$SppGrp=="rege", c("cell.indx", "CoordX", "CoordY")]
      # radius.neigh=10000
      # km2.pixel=(cell.size/1000)^2


neighour.spp <- function(microland, target.cells, radius.neigh, km2.pixel){
  
  # number of species groups (states, including regeneration, water, or non-forests) in the landscape
  nspp <- length(levels(microland$SppGrp))
  
  # initialize 1 matrix with as many rows as target cells and colums as species
  cells.neigh <- matrix(nrow=nrow(target.cells), ncol=nspp) 
  # and 1 vector of lenght the number of target cells
  target.spp <- numeric(nrow(target.cells))
  
  # Retrieve index of neighbours in a circular neighbourhood of radius
  # radius.buff for each target cell
  # WARNING: the returned index is row.index instead of cell.index !!!
  incells <- ceiling((pi*radius.neigh^2)/(km2.pixel*10^6))
  list.cell.neigh <- nn2(microland[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                         k=incells, searchtype='priority')
  nn.indx <- list.cell.neigh[[1]]
  rm(list.cell.neigh)
  
  # Select a community for the site in regeneration from the pool of suitable species
  spp <-  levels(microland$SppGrp)[-c(4, 7,9)]
  for (i in 1:nrow(target.cells)) {
    ## Count the abundance of each species / land-cover within the neigborhood buffer of the target cells
    cells.neigh[i,] <- table(microland$SppGrp[nn.indx[i,]])
    ## Remove non-suitable land-covers:  NonFor (4), Water (9), and Regeneration (7) for the selection, 
    ## then choose a species id for each target cell according to the abundance in the neighbourhood.
    ## If no species are available, then assign NonFor
    if(sum(cells.neigh[i,-c(4, 7, 9)])==0){
      eco.type <- microland$EcoType[microland$cell.indx==target.cells$cell.indx[i]]  
      target.spp[i] <- ifelse(eco.type=="RE" | eco.type=="ME", "EPN",
                              ifelse(eco.type=="RS" | eco.type=="MS", "SAB",
                                     ifelse(eco.type=="MJ", "BOJ",
                                            ifelse(eco.type=="FE", "ERS", "other"))))
    }
    else{
      id <- sample(1:(nspp-3), 1, replace=FALSE, prob=cells.neigh[i,-c(4,7,9)])
      target.spp[i] <- spp[id]
    }
  }
  
  return(target.spp)
  
}

  
