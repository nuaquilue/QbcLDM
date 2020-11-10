######################################################################################
###  buffer.mig()
###
###  Description >  Function to determine the presence of species within a buffer
###                 around disturbed cells. Called in the regeneration/succession
###                 section of landscape.dyn
###
###  Arguments >  
###   microland : data frame with the cell.indx, SppGrp, x, and Coord Y
###   target.cells : vector of cells open for colonization, and for which the presence of source populations
###                  nearby must be assessed
###   potential.spp : a matrix describind  the radius in m corresponding to the maximal colonization distance for each species, 
###                 in the following order: PET(aspen), BOJ(YellowBirch), ERS(SugarMaple), 
###                 SAB (BalsamFir), EPN(BlackSpruce); and the inimum number of source populations that must be present within the colonization 
###             distance (given in radius.buff) to enable colonization in the cell.
###
###  Value >  Data frame with the cell.indx for target.cells and the presence (TRUE) or
###           absence (FALSE) of a sufficient number of source populations for each 
###           species: PET, BOJ, ERS, SAB, EPN, other, NonFor
######################################################################################

 # microland <- land[, c("cell.indx", "SppGrp", "x", "y","TSD","Tcomp")]
 # target.cells <- land[land$cell.id %in% burnt.cells, c("cell.indx", "x", "y")]

buffer.mig4 <- function(microland, target.cells.mig, potential.spp){
  
  ## Coordinates of the target cells
  target.cells.mig.xy <- microland[microland$cell.id %in% target.cells.mig, c("cell.id", "x", "y")]
  
  ## Radius ~ max. colonization distance per species, and min number of soruces to enable colonization
  radius.buff <- potential.spp[,2]
  nb.buff <- potential.spp[,3]
    
  # Source cells (i.e. potential colonizers) per species. Minimal age of 50 years.
  micro.boj <- microland[microland$SppGrp=="BOJ" & microland$TSF >= 50 & microland$TSSBW >= 50 & microland$TSCC >= 50 & microland$Tcomp >= 50,]
  micro.pet <- microland[microland$SppGrp=="PET" & microland$TSF >= 50 & microland$TSSBW >= 50 & microland$TSCC >= 50 & microland$Tcomp >= 50,]
  micro.ers <- microland[microland$SppGrp=="ERS" & microland$TSF >= 50 & microland$TSSBW >= 50 & microland$TSCC >= 50 & microland$Tcomp >= 50,]
  micro.epn <- microland[microland$SppGrp=="EPN" & microland$TSF >= 50 & microland$TSSBW >= 50 & microland$TSCC >= 50 & microland$Tcomp >= 50,]
  micro.sab <- microland[microland$SppGrp=="SAB" & microland$TSF >= 50 & microland$TSSBW >= 50 & microland$TSCC >= 50 & microland$Tcomp >= 50,]
  
  ### Calculate number of source populations in the neighbohood of each target cell. Colonization distances
  ### are species-specific.
  # PET
  list.cell.buff <- nn2(micro.pet[,c("x","y")], target.cells.mig.xy[,c("x","y")], 
                        k=nb.buff[4] , searchtype='priority')
  nn.dists.pet <- list.cell.buff$nn.dists[, nb.buff[4]] < radius.buff[4]
  
  # BOJ   
  list.cell.buff <- nn2(micro.boj[,c("x","y")], target.cells.mig.xy[,c("x","y")], 
                        k=nb.buff[1], searchtype='priority')
  nn.dists.boj <- list.cell.buff$nn.dists[,nb.buff[1]]< radius.buff[1]

  # ERS
  list.cell.buff <- nn2(micro.ers[,c("x","y")], target.cells.mig.xy[,c("x","y")], 
                        k=nb.buff[3], searchtype='priority')
  nn.dists.ers <- list.cell.buff$nn.dists[,nb.buff[3]]< radius.buff[3]
  
  # SAB   
  list.cell.buff <- nn2(micro.sab[,c("x","y")], target.cells.mig.xy[,c("x","y")], 
                        k=nb.buff[5],  searchtype='priority')
  nn.dists.sab <- list.cell.buff$nn.dists[,nb.buff[5]]< radius.buff[5]
  
  # EPN   
  list.cell.buff <- nn2(micro.epn[,c("x","y")], target.cells.mig.xy[,c("x","y")], 
                        k=nb.buff[2],  searchtype='priority')
  nn.dists.epn <- list.cell.buff$nn.dists[,nb.buff[2]]< radius.buff[2]
  
  # Build a data frame with the presence or absence of a sufficient number of
  # source populations of each species around each target cell. Currently set
  # at one in all scenarios, but could be modified.
  
  target.df <- data.frame(target.cells.mig.xy, PET=nn.dists.pet, BOJ=nn.dists.boj, ERS=nn.dists.ers, SAB=nn.dists.sab, 
                          EPN=nn.dists.epn, OTH=TRUE, NonFor=TRUE)
  target.df <- target.df[,-c(2,3)]  
  target.df <- melt(target.df,id=c("cell.id"))
  
  names(target.df)[-1] <- c("PotSpp", "PressBuffer")
  target.df$PotSpp <- as.character(target.df$PotSpp)
  return(target.df)

  ## Now for each potential species, look if it can colonize each target cell according to 
  ## the estimated colonization distance
  for(ispp in 1:nrow(potential.spp)){
    ## Cells are potential colonizers if are at least 50 years old and time since last species composition change 
    ## is at least 50 years.
    colonizer <- filter(land, SppGrp %in% potential.spp$spp[ispp], Age>=50, Tcomp>=50)
    ## First find the closest neighbors of all target cells. Look for enough neighbors to cover the maximum
    ## estimated maximum colonization distance (i.e. 75.000 m)
    neighs <- nn2(select(colonizer, x, y), target.coord, searchtype="priority", k=nrow(colonizer))  
    ## Now verify if the potential species are close enough of the target cells, the colonization distance is species-specific.
    aux <- apply(neighs$nn.dists <= potential.spp$rad[ispp], 1, sum) >= potential.spp$nneigh[ispp]
    buffer.spp <- rbind(buffer.spp, data.frame(cell.id=target.cells, PotSpp=potential.spp$spp[ispp], PressBuffer=aux))    
  }
   
  return(buffer.spp)
}

