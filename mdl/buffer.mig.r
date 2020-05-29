######################################################################################
###  buffer.mig()
###
###  Description >  Function to determine the presence of species within a buffer
###                 around disturbed cells. Called in the regeneration/succession
###                 section of landscape.dyn
###
###  Arguments >  
###   land : data frame with the cell.indx, SppGrp, x, and Coord Y
###   target.cells : vector of cells to be colonized and for which the presence of source populations
###                  nearby must be assessed
###   radius.buff : a vector presenting the radius in m corresponding to the maximal colonization distance for each species, 
###                 in the following order: PET(aspen), BOJ(YellowBirch), ERS(SugarMaple), 
###                 SAB (BalsamFir), EPN(BlackSpruce)
###   nb.buff : a vector. Minimum number of source populations that must be present within the colonization 
###             distance (given in radius.buff) to enable colonization in the cell.
###
###  Details > 
###
###  Value >  Data frame with the cell.indx for target.cells and the presence (TRUE) or
###           absence (FALSE) of a sufficient number of source populations for each 
###           species: PET, BOJ, ERS, SAB, EPN, other, NonFor
######################################################################################

 
buffer.mig <- function(land, target.cells, potential.spp){
  
  ## If target data.frame is empty
  if(length(target.cells)==0)
    return(numeric())
  
  ## Get coordinates x,y of target cells
  target.coord <- filter(land, cell.id %in% target.cells) %>% select(x,y)
  
  ## A data frame with target.cells ids, Potential species and whether these are found in the neighborhood
  ## We assume that always at least 1 "other" spceies and "NonFor" cover are in the target neighborhood
  buffer.spp <- data.frame(cell.id=target.cells, 
                           PotSpp=rep(c("other", "NonFor"), each=length(target.cells)),
                           PressBuffer=TRUE)
  
  ## Verify whether the closest neighs of each potential spp are close 
  ## enought of the target cell to be colonized
  for(ispp in 1:nrow(potential.spp)){
    ## Find the first closest 50 neighbors (50*2000m = 10.000m) in the subset of cells with the potential species 
    ## Source cells (i.e. potential colonizers) have minimal age of 50 years and minimal time of last species change 
    ## composition also 50 year.
    list.cell.buff <- nn2(filter(land, SppGrp %in% potential.spp$spp[ispp], Age>=50, Tcomp>=50) %>% select(x,y),
                          target.coord, k=50, searchtype='priority')
    
    ## Now verify if the potential species are close enough of the target cells, the colonization distance is species-specific.
    aux <- apply(list.cell.buff$nn.dists < potential.spp$rad[ispp], 1, sum)>=potential.spp$nneigh[ispp]
    buffer.spp <- rbind(buffer.spp, data.frame(cell.id=target.cells, PotSpp=potential.spp$spp[ispp], PressBuffer=aux))
  }
  
  return(buffer.spp)
}

