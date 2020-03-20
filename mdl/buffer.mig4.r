######################################################################################
###  buffer.mig()
###
###  Description >  Function to determine the presence of species within a buffer
###                 around disturbed cells. Called in the regeneration/succession
###                 section of landscape.dyn
###
###  Arguments >  
###   microland : data frame with the cell.indx, SppGrp, CoordX, and Coord Y
###   target.cells : vector of cells open for colonization, and for which the presence of source populations
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

 # microland <- land[, c("cell.indx", "SppGrp", "CoordX", "CoordY","TSD","Tcomp")]
 # target.cells <- land[land$cell.indx %in% burnt.cells, c("cell.indx", "CoordX", "CoordY")]

buffer.mig <- function(microland, target.cells, radius.buff, nb.buff){
  
  # Source cells (i.e. potential colonizers) per species. Minimal age of 50 years.
  micro.boj <- microland[microland$SppGrp=="BOJ" & microland$TSD >= 50 & microland$Tcomp >= 50,]
  micro.pet <- microland[microland$SppGrp=="PET" & microland$TSD >= 50 & microland$Tcomp >= 50,]
  micro.ers <- microland[microland$SppGrp=="ERS" & microland$TSD >= 50 & microland$Tcomp >= 50,]
  micro.epn <- microland[microland$SppGrp=="EPN" & microland$TSD >= 50 & microland$Tcomp >= 50,]
  micro.sab <- microland[microland$SppGrp=="SAB" & microland$TSD >= 50 & microland$Tcomp >= 50,]
  
  ### Calculate number of source populations in the neighbohood of each target cell 
  # PET
  list.cell.buff <- nn2(micro.pet[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                        k=nb.buff[1] , searchtype='priority')
  nn.dists.pet <- list.cell.buff$nn.dists[, nb.buff[1]] < radius.buff[1]
  
  # BOJ   
  list.cell.buff <- nn2(micro.boj[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                        k=nb.buff[2], searchtype='priority')
  nn.dists.boj <- list.cell.buff$nn.dists[,nb.buff[2]]< radius.buff[2]
  
  # ERS
  list.cell.buff <- nn2(micro.ers[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                        k=nb.buff[3], searchtype='priority')
  nn.dists.ers <- list.cell.buff$nn.dists[,nb.buff[3]]< radius.buff[3]
  
  # SAB   
  list.cell.buff <- nn2(micro.sab[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                        k=nb.buff[4],  searchtype='priority')
  nn.dists.sab <- list.cell.buff$nn.dists[,nb.buff[4]]< radius.buff[4]
  
  # EPN   
  list.cell.buff <- nn2(micro.epn[,c("CoordX","CoordY")], target.cells[,c("CoordX","CoordY")], 
                        k=nb.buff[5],  searchtype='priority')
  nn.dists.epn <- list.cell.buff$nn.dists[,nb.buff[5]]< radius.buff[5]
  
  # Build a data frame with the presence or abscence of a sufficient number of
  # source populations of each species around each target cell
  
  target.df <- data.frame(target.cells, PET=nn.dists.pet, BOJ=nn.dists.boj, ERS=nn.dists.ers, SAB=nn.dists.sab, 
                          EPN=nn.dists.epn, other=TRUE, NonFor=TRUE)
  target.df <- target.df[,-c(2,3)]  
  target.df <- melt(target.df,id=c("cell.indx"))
  names(target.df)[-1] <- c("PotSpp", "PresBuffer")
  
  return(target.df)

}

