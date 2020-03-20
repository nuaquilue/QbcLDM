######################################################################################
###  fire.spread()
###
###  Description >  Simulates fire spread from ignitions points according to a spread probability
###
###  Arguments >  
###   landscape : raster of the species 
###   loci : index cells for the ignitions
###   spreadProb : raster or numeric value accounting for the burnt probability at 
###                the cell level
###   mask : raster that indicates where spreading is not allowed (=1) or it is (=0)
###   maxSize : vector of the maximum number of pixels for a single or all events to be spread. 
###             Recycled to match loci length.
###   directions : the number adjacent cells in which to look; default is 8 (Queen case).
###   iterations : Number of iterations to spread. Leaving this NULL allows the spread 
###                to continue until stops spreading itself
###   spreadProbLater : Numeric or rasterLayer. If provided, then this will become 
###                     the spreadProb after the first iteration
###
###  Details > Adapted from SpaDES:spread function 
###
###  Value >  A list of two elements, the first is a vector of the indexes of the burnt cells,
###           the second is a vector of the effective burnt area per fire
######################################################################################


fire.spread <- function(landscape, loci = NA_real_, spreadProb = 1, mask = NA, 
                        maxSize = 100000L, directions = 8L, iterations = 10000L, 
                        spreadProbLater = NA_real_, plot.it = F, out.raster = NULL){

  
  ## If 'spreadProbLater' is not provided, then assign the initial 'spreadProb'
  ## either it is a raster or a numeric value
  if (is.na(spreadProbLater)) 
    spreadProbLater <- spreadProb
  # Assign 0 to NA locations
  spreadProbLater[is.na(spreadProbLater)] <- 0L
  spreadProb[is.na(spreadProb)] <- 0L
  #
  if (is(mask, "Raster") & is.numeric(spreadProb)){
    spreadProb <- raster(extent(landscape), res = res(landscape), vals = spreadProb)
    spreadProb[mask == 1L] <- 0L
    spreadProbLater <- spreadProb
  }
  
  ## Initializations  
  # 'spreads' will track the cells the fire has spread to with the ID value
  #  ID values are consecutive from 1 to number of ignitions
  spreads <- vector("integer", ncell(landscape))
  spreads[loci] <- 1L:length(loci)
  # Recycle maxSize in case is a numeric value for as many ignitions there are
  maxSize <- rep_len(maxSize, length(loci))
  # Vector of the current sizes of fires. Ignition locations always burnt
  size <- rep_len(1L, length(loci))
  # Vector of the raster indexes of the burnt cells
  burnt.cell.indx <- loci
  
  ## Start the spreading process until there are cells to spread from to reach the target area,
  ## the target area has not reached, or the number of iterations is not exceeded
  for(ifire in 1:length(loci)){

        ## Start iteration
    n <- 1L
    events <- loci[ifire]
    
    ## Keep burning if there are active cells in the front, the target size hasn't been
    ## reached yet, and the number of iterations is smaller than the threshold
    while (length(events)>0 & size[ifire]<maxSize[ifire] & (n<=iterations) ) {   
    
      ## Find the neighbours of the active cells in the fire front ('loci'). 
      ## Then subselect the suitable neighours, those not already burnt
      potentials <- adj(landscape, events, directions, pairs = TRUE)
      potentials <- potentials[spreads[potentials[, 2L]] == 0L, , drop = FALSE] 
      
      ## After the first iteration use the 'spreadProbLater' probability
      if (n == 2) 
        spreadProb <- spreadProbLater
      
      ## Assign the spread probability to the potential neighbours
      if (is.numeric(spreadProb)){
        spreadProbs <- spreadProb  
      }
      else{
        spreadProbs <- spreadProb[potentials[, 2L]]   
      }

      ## Remove those neighbours that randomly has value <= spreadsProb
      potentials <- potentials[runif(NROW(potentials)) <= spreadProbs, , drop = FALSE]
      ## Resample potentials (i.e. reorganize randomly)
      potentials <- potentials[sample.int(NROW(potentials)), , drop = FALSE]
      ## Eliminate duplicates
      potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
      
      ## Count the potential neigbhours to be burnt for each fire
      len <- nrow(potentials)

      ## If any of the fires is not supposed to spread, give it another chance.
      ## And force the fires without chace to spread
      a <- 1
      while(len==0 & a<3){
        auxpotentials <- adj(landscape, events, directions, pairs = TRUE)
        auxpotentials <- auxpotentials[spreads[auxpotentials[, 2L]] == 0L, , drop = FALSE]
        if (is.numeric(spreadProb)){
          spreadProbs <- spreadProb
        }
        else{
          spreadProbs <- spreadProb[auxpotentials[, 2L]]
        }
        # aux <- data.frame(auxpotentials, spreadProbs)
        if(a==1) 
          auxpotentials <- auxpotentials[runif(NROW(auxpotentials)) <= spreadProbs, , drop = FALSE]
        if(a==2)
          auxpotentials <- auxpotentials[spreadProbs>0, , drop = FALSE]
        auxpotentials <- auxpotentials[sample.int(NROW(auxpotentials)), , drop = FALSE]
        potentials <- rbind(potentials, auxpotentials)
        potentials <- potentials[!duplicated(potentials[, 2L]), , drop = FALSE]
        len <-  nrow(potentials)
        a <- a+1
      }
      
      ## Then verify if there are more potential neighbours + already burnt cells
      ## than  the maxSize of each fire. If it is the case, 
      ## randomly remove the neighbours in surplus
      toRm <- 0
      if ((size[ifire] + len) > maxSize[ifire]) {   
        toRm <- (size[ifire] + len) - maxSize[ifire]
        potentials <- potentials[-sample(1:len, toRm), , drop = FALSE]
      }
  
      ## Cells that currently burnt, that is the active cells are now the recently burnt
      ## neighbors of fires that can keep spreading if the target size has not been reached yet
      events <- potentials[, 2L]
      ## Increment the size of the fire
      size[ifire] <- size[ifire] + len - toRm
      
      ## Mark effectivelly spread cells in the 'spreads' vector with the IDs of the source cells.
      if (length(events) > 0) 
        spreads[events] <- spreads[potentials[, 1L]]
      
      ## Add the cell.indx of the burnt cells
      burnt.cell.indx <- c(burnt.cell.indx, events)
    
      ## Increment iteration
      n <- n + 1L
  
    }  # while
  
  }  # ifire
  
  ## Count the effective burnt area per fire
  burnt.area.fire <- data.frame(table(spreads)[-1])

  ## Return the cell.indx of burnt locations
  return(list(perims=data.frame(cell.indx=burnt.cell.indx, fire.id=spreads[burnt.cell.indx]), 
              areas=data.frame(target=maxSize, burnt=burnt.area.fire$Freq)))
  
}

