######################################################################################
###  partial.cut()
###
######################################################################################

partial.cut <- function(land, hor.plan, km2.pixel, pc.step){  
  
  cat("Partialcutting", "\n")
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  
  ###################################### SUTAINABLE YIELD ######################################
  ## Extract the portion that is managed through uneven-aged silviculture - partial cutting.
  ## Based on stand composition: some species are mostly managed through even aged silviculture
  ## 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
  land <- mutate(land, rndm=runif(nrow(land)))
  land.evenage <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB")
                         & is.na(Exclus) & rndm<=0.05) 
  land.unevenage <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("BOJ", "ERS", "OthDT") 
                           & is.na(Exclus) & rndm<=0.95) 
  land.uea <- rbind(land.evenage, land.unevenage)
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$AgeMatuPC <- round(land.uea$AgeMatu/2)
  
  ## Subset of harvestable (i.e. mature even-aged) cells
  land.rec <- filter(land.uea, Age>=AgeMatuPC)
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  s.mat <- group_by(land.rec, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
    
  # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
  strates <- group_by(land.uea, MgmtUnit, AgeMatuPC) %>% summarize(x=length(unique(AgeMatuPC)))
  
  ## Compute sustainable yield per FMU
  recoltable.s <- cbind(s.uea %>% select(-x), matrix(NA, nrow=nrow(s.uea), ncol=hor.plan))
  for(unit in unique(land.uea$MgmtUnit)){
    strate.fmu <- filter(strates, MgmtUnit==unit)
  
    ## The calculation is only performed if there are cells to harvest
    if(length(strates)>0){
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- recoltable2 <-  matrix(0, nrow(strate.fmu), hor.plan)
      for (j in 1:nrow(strate.fmu)){ 
        age.mat.stra <- strate.fmu$AgeMatuPC[j]
        # pour être admissibles à une CP les peuplements doivent être à la fois
        # matures et ne pas avoir été traités depuis un certain temps. Pour le
        # moment le temps restant à écouler avant la maturité n'est pas pris en compte
        # (donc le taux de coupe partielle sous évalué durant les premieres périodes)
        TSPCstrate <- land.uea$TSPCut[land.uea$MgmtUnit==unit & land.uea$AgeMatuPC==age.mat.stra] 
        # superficie maximale théorique récoltable par période pour chaque strate
        recoltable2[j,] <- (length(TSPCstrate)/(age.mat.stra/pc.step)) * (1:hor.plan)   
        # Determine the period when maturity will be reached for the different age classes
        for (per in 0:(hor.plan-1))  
          recoltable[j,per+1] <- sum(TSPCstrate >= (age.mat.stra-(per*pc.step)))
        for (per in (age.mat.stra/pc.step): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      recoltable.s[which(unit==unique(land.uea$MgmtUnit)),2:(hor.plan+1)] <-  
          colSums(recoltable)/(1:hor.plan)

    }
      
  } #unit
  
  ## Number of cells to harvest (sustained yield level), corresponding to 
  ## the future period with the lowest mature forest availability 
  n.pc.cells <- data.frame(MgmtUnit=recoltable.s$MgmtUnit,
                           x=round(apply(recoltable.s[,2:(hor.plan+1)], 1, min)))
  
    
  ############################## SELECT CELLS TO BE HARVESTED ##############################
  ## Now that the optimal yield level has been determined, 
  ## select the cells to be harvested during the current period
  ## Randomly select cells among the uneven-aged mature cells present in non-protected areas
  for(unit in unique(land.uea$MgmtUnit)){
    pc.cells.ua <- numeric()
    if(n.pc.cells$x[n.pc.cells$MgmtUnit==unit]>0) 
      pc.cells.ua <- sample(land.rec$cell.id, size=n.pc.cells$x[n.pc.cells$MgmtUnit==unit], replace=FALSE)
    ## Add the cells partially cut in this UA:
    pc.cells <- c(pc.cells, pc.cells.ua)   
  }    
  
    
  ################################################# TRACKING #################################################
  ## Species partially cut by management unit
  spp.pcut  <- filter(land, cell.id %in% pc.cells) %>% group_by(MgmtUnit, SppGrp) %>% 
               summarize(x=length(MgmtUnit)*km2.pixel) 

  ## Merge all the info
  track <- left_join(s.uea, s.mat, by="MgmtUnit") %>% left_join(n.pc.cells, by="MgmtUnit")
  names(track)[2:ncol(track)] <- c("uneven.age", "s.mat", "s.rec.pc")
  track[,2:ncol(track)] <- track[,2:ncol(track)]*km2.pixel
  track[is.na(track)] <- 0
  
  # Return the cell.id of the partially cut locations and the tracking info
  return(list(pc.cells=pc.cells, track.cut=track, spp.pcut=spp.pcut))  

}
