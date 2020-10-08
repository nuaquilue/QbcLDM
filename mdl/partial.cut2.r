######################################################################################
###  partial.cut()
###  selection of cells harvested with partial cuts
######################################################################################

partial.cut2 <- function(land, hor.plan, km2.pixel, pc.step, harv.level.pc){  
  
  cat("Partialcutting2", "\n")
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  

  land <- mutate(land, rndm=runif(nrow(land)))
  land$MgmtUnit <- as.numeric(as.character(land$MgmtUnit))
  land2 <- land[!is.na(land$MgmtUnit),]
  
  land.resineux <- filter(land2,  SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB")
                         & is.na(Exclus) & rndm<=0.05) 
  land.feuil.tol <- filter(land2,  SppGrp %in% c("BOJ", "ERS", "OthDT") 
                           & is.na(Exclus) & rndm<=0.95) 
  land.uea <- rbind(land.resineux, land.feuil.tol)
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$AgeMatuPC <- round(land.uea$AgeMatu,-1)/2
  
  
  ## Subset of harvestable (i.e. mature uneven-aged, ot recently partial cut) cells
  land.rec <- filter(land.uea, Age>=(AgeMatu-15) & TSDist >=(AgeMatu-15) & TSPCut >=AgeMatuPC )
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  s.mat <- group_by(land.rec, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  
  ############################## SELECT CELLS TO BE HARVESTED ##############################
  ## select the cells to be harvested during the current period
  ## Randomly select cells among the uneven-aged mature cells present in non-protected areas
  for(unit in unique(land.uea$MgmtUnit)){  #unit=9351
    pc.cells.ua <- numeric()
    nb.cell.dispo <- length(land.rec$cell.id[land.rec$MgmtUnit==unit])
    poss <- harv.level.pc$x[harv.level.pc$MgmtUnit==unit]
    if(length(poss>0 &  nb.cell.dispo>0)>0) {
      
      niv.rec <- min(poss,nb.cell.dispo)
      pc.cells.ua <- sample(land.rec$cell.id[land.rec$MgmtUnit==unit], size=niv.rec, replace=FALSE)
    ## Add the cells partially cut in this UA:
    pc.cells <- c(pc.cells, pc.cells.ua)   
    }    
  }
  
    
  ################################################# TRACKING #################################################
  ## Species partially cut by management unit
  spp.pcut  <- filter(land, cell.id %in% pc.cells) %>% group_by(MgmtUnit, SppGrp) %>% 
               summarize(x=length(MgmtUnit)*km2.pixel) 
  #as.data.frame(track)
  ## Merge all the info
  track <- left_join(s.uea, s.mat, by="MgmtUnit") %>% left_join(harv.level.pc, by="MgmtUnit")
  names(track)[2:ncol(track)] <- c("uneven.age", "s.mat", "s.rec.pc")
  track[,2:ncol(track)] <- track[,2:ncol(track)]*km2.pixel
  track[is.na(track)] <- 0
  
  # Return the cell.id of the partially cut locations and the tracking info
  return(list(pc.cells=pc.cells, track.cut=track, spp.pcut=spp.pcut))  

}
