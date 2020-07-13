######################################################################################
###  neighour.spp()
###
###  Description >  Function that returns the forest composition of a circular 
###                 neighbourhood of radius R around the target cells. The function is
###                 useful to attribute a forest composition class to cells for which this information
###                 is not available on forest maps.
###                 Called in initialize.study.area
###
###  Arguments >  
###   land : data frame with the cell.id, SppGrp, CoordX, and Coord Y
###   target.cells : vector of cells to find the buffer around them
###   radius.neigh : radius in m of the circular neighbourhood to be draw around the target cells
###   km2.pixel <- size in km2 of the raster pixels
###
###  Details > 
###
###  Value >  Matrix with the cell.id for target.cells (in rows) and the abundance of 
###           of the species in the neighbourhood (one per column)
######################################################################################

neighour.spp <- function(land, target.cells, radius.neigh, km2.pixel){
  
  library(RANN)
  
  # number of species groups (states, including regeneration, water, or non-forests) in the landscape
  covers <- levels(land$SppGrp)
  spp <- covers[covers!="NonFor" & covers!="Water" & covers!="rege"]
  
  # initialize 1 matrix with as many rows as target cells and columns as species
  cells.neigh <- matrix(nrow=nrow(target.cells), ncol=length(covers)) 
  # and 1 vector of length corresponding to the number of target cells
  target.spp <- numeric(nrow(target.cells))
  
  # Retrieve index of neighbours in a circular neighbourhood of radius
  # radius.buff for each target cell
  # WARNING: the returned index is row.index instead of cell.index !!!
  incells <- ceiling((pi*radius.neigh^2)/(km2.pixel*10^6))
  list.cell.neigh <- nn2(land[,c("x","y")], target.cells[,c("x","y")], 
                         k=incells, searchtype='priority')
  nn.indx <- list.cell.neigh[[1]]
  rm(list.cell.neigh)
  
  # Select a community for the site in regeneration from the pool of suitable species
  for (i in 1:nrow(target.cells)) {
    ## Count the abundance of each species / land-cover within the neigborhood buffer of the target cells
    cells.neigh[i,] <- table(land$SppGrp[nn.indx[i,]])
    ## Remove non-suitable land-covers:  NonFor (4), Water (9), and Regeneration (7) for the selection, 
    ## then choose a species id for each target cell according to the abundance in the neighbourhood.
    ## If no species are available, then assign NonFor
    if(sum(cells.neigh[i,-c(4, 7, 9)])==0){
      eco.type <- land$EcoType[land$cell.id==target.cells$cell.id[i]]  
      target.spp[i] <- ifelse(eco.type=="RE" | eco.type=="ME", "EPN",
                              ifelse(eco.type=="RS" | eco.type=="MS", "SAB",
                                ifelse(eco.type=="MA" | eco.type=="RC" | eco.type=="RE" | eco.type=="TO", "OthCB",
                                  ifelse(eco.type=="RB" | eco.type=="RP" | eco.type=="RT", "OthCT", 
                                    ifelse(eco.type=="MJ", "BOJ",
                                      ifelse(eco.type=="FE", "ERS", 
                                        ifelse(eco.type=="FC" | eco.type=="FO" | eco.type=="LA" | eco.type=="LL" | eco.type=="MF", "OthDT")))))))
    }
    else {
      id <- sample(1:length(spp), 1, replace=FALSE, prob=cells.neigh[i,-c(4,7,9)])
      target.spp[i] <- spp[id]
    }
  }
  
  return(target.spp)
  
}

  
