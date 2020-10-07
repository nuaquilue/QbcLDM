######################################################################################
###  timber.partial()
###  timber supply calculation for partial cuts
######################################################################################

timber.partial <- function(land, hor.plan, km2.pixel, pc.step){  
  
  cat("Timber Partialcutting", "\n")
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  
  ###################################### SUTAINABLE YIELD ######################################
  ## Extract the portion that is managed through uneven-aged silviculture - partial cutting.
  ## Based on stand composition: some species are mostly managed through even aged silviculture
  ## 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
  land <- mutate(land, rndm=runif(nrow(land)))
  land$MgmtUnit <- as.numeric(as.character(land$MgmtUnit))
  land2 <- land[!is.na(land$MgmtUnit),]
  
  land.evenage <- filter(land2,  SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB")
                         & is.na(Exclus) & rndm<=0.05) 
  land.unevenage <- filter(land2,  SppGrp %in% c("BOJ", "ERS", "OthDT") 
                           & is.na(Exclus) & rndm<=0.95) 
  land.uea <- rbind(land.evenage, land.unevenage)
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$AgeMatuPC <- round(land.uea$AgeMatu,-1)/2
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, MgmtUnit) %>% summarise(x=length(MgmtUnit))    

  # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
  strates <- group_by(land.uea, MgmtUnit, AgeMatuPC) %>% summarize(x=length(unique(AgeMatuPC)))
  #unique(land.uea$AgeMatuPC)
  ## Compute sustainable yield per FMU
  recoltable.s <- cbind(s.uea %>% select(-x), matrix(NA, nrow=nrow(s.uea), ncol=hor.plan))

  ### corriger TSPC en fonction des perturbations sévères récentes
  ### les peuplements sont accessibles à la coupe partielle 15 ans avant d'être matures
  
  vsc.cor.pc2 <- land.uea$Age <land.uea$AgeMatu
  land.uea$TSPCut[vsc.cor.pc2] <- land.uea$Age[vsc.cor.pc2] - (land.uea$AgeMatuPC[vsc.cor.pc2]-15)
        
  vsc.cor.pc <- land.uea$TSDist <land.uea$AgeMatu
  land.uea$TSPCut[vsc.cor.pc] <- land.uea$TSDist[vsc.cor.pc] - (land.uea$AgeMatuPC[vsc.cor.pc]-15)

  unit = 9351
  for(unit in unique(land.uea$MgmtUnit)){
    strate.fmu <- filter(strates, MgmtUnit==unit)
    as.data.frame(strate.fmu)
    ## The calculation is only performed if there are cells to harvest
    if(length(strates)>0){
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- recoltable2 <-  matrix(0, nrow(strate.fmu), hor.plan)
      for (j in 1:nrow(strate.fmu)){ # j=3
        age.mat.stra <- strate.fmu$AgeMatuPC[j]
        # extraire les TSPC pour la strate et l'UA courante
        
        TSPCstrate <- land.uea$TSPCut[land.uea$MgmtUnit==unit & land.uea$AgeMatuPC==age.mat.stra] 
        # superficie maximale thÃ©orique rÃ©coltable par pÃ©riode pour chaque strate
        recoltable2[j,] <- (length(TSPCstrate)/(age.mat.stra/pc.step)) * (1:hor.plan)   
        # Determine the period when maturity will be reached for the different age classes
        for (per in 0:(hor.plan-1))  
          recoltable[j,per+1] <- sum(TSPCstrate >= (age.mat.stra-(per*pc.step)))
        for (per in (age.mat.stra/pc.step): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      
      recoltable.s[which(unit==recoltable.s$MgmtUnit),2:(hor.plan+1)] <-  
          colSums(recoltable)/(1:hor.plan)
  # recoltable.s[60:70,]
    }
      
  } #unit
  
  ## Number of cells to harvest (sustained yield level), corresponding to 
  ## the future period with the lowest mature forest availability 
  n.pc.cells <- data.frame(MgmtUnit=recoltable.s$MgmtUnit,
                           x=round(apply(recoltable.s[,2:(hor.plan+1)], 1, min)))

return(n.pc.cells)    
  
}
