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
###   radius.neigh : radius in m of the circular neighbourhood to be draw around the target cells
###   cell.size <- size of the cell in m
###
###  Details > 
###
###  Value >  Matrix with the cell.id for target.cells (in rows) and the abundance of 
###           of the species in the neighbourhood (one per column)
######################################################################################

neighour.spp <- function(land, radius.neigh, cell.size){
  
  ## Species in the landscape, except "rege"
  spp <- levels(land$spp)
  spp <- spp[spp!="rege"]
  
  ## Target cells, those in regeneration
  target.cells <- filter(land, spp =="rege") %>% select(cell.id, x, y)
  
  ## Initialize one matrix with as many rows as target cells and columns as species, and 
  ## one vector of length the number of target cells
  cells.neigh <- matrix(nrow=nrow(target.cells), ncol=length(levels(land$spp))) 
  target.spp <- numeric(nrow(target.cells))
  
  ## Number of cells in the circular neighborhood
  incells <- ceiling((pi*radius.neigh^2)/(cell.size^2))
    
  ## Find the index of neighbours in a circular neighbourhood of radius.neigh for each target cell
  # WARNING: the returned index is row.index instead of cell.index !!!
  list.cell.neigh <- nn2(land[,c("x","y")], target.cells[,c("x","y")], k=incells, searchtype='priority')
  nn.indx <- list.cell.neigh[[1]]
  
  ## Count the number of species from the pool of suitable species
  for(i in 1:nrow(target.cells))
    cells.neigh[i,] <- table(land$spp[nn.indx[i,]])
  cells.neigh <- as.data.frame(cells.neigh)
  names(cells.neigh) <- levels(land$spp)
  cells.neigh <- select(cells.neigh, -rege, -NonFor)
  tot <- apply(cells.neigh, 1, sum)
  
  ## Select a community for the site in regeneration for those sites with some spp in the neighborhood
  indx <- 1:nrow(target.cells)
  indx.with <- indx[tot!=0]
  indx.without <- indx[tot==0]
  spp <- spp[spp!="NonFor"]
  for(j in indx.with){
    id <- sample(1:length(spp), 1, replace=FALSE, prob=cells.neigh[j,])
    target.spp[j] <- spp[id]
  }
  
  ## Select a community for the site in regeneration for those sites with some spp in the neighborhood
  aux <- target.cells[indx.without,]
  aux <- filter(land, cell.id %in% aux$cell.id) %>% mutate(eco.type=substr(eco.type,1,2)) %>% 
         mutate(new.spp = ifelse(eco.type=="RE" | eco.type=="ME", "EPN",
                            ifelse(eco.type=="RS" | eco.type=="MS", "SAB",
                               ifelse(eco.type=="MA" | eco.type=="RC" | eco.type=="RE" | eco.type=="TO", "OTH.RES.N",
                                   ifelse(eco.type=="RB" | eco.type=="RP" | eco.type=="RT", "OTH.RES.S", 
                                       ifelse(eco.type=="MJ", "BOJ",
                                          ifelse(eco.type=="FE", "ERS", 
                                             ifelse(eco.type=="FC" | eco.type=="FO" | eco.type=="LA" | eco.type=="LL" | eco.type=="MF", "OTH.FEU.S", NA))))))))
  target.spp[indx.without] <- aux$new.spp
  target.spp[is.na(target.spp)] <- "EPN"
  
  return(target.spp)
  
}

  
