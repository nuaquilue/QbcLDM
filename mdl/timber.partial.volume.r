######################################################################################
###  timber.partial()
###  timber supply calculation for partial cuts
######################################################################################

timber.partial.volume <- function(land, hor.plan, km2.pixel, pc.step){  
  
  cat("Timber Partialcutting volume", "\n")
  
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
  #land.uea$vol.max <- volume(land.uea,km2.pixel)$x
  unit = 9351
  for(unit in unique(land.uea$MgmtUnit)){
    strate.fmu <- filter(strates, MgmtUnit==unit)
    as.data.frame(strate.fmu)
    ## The calculation is only performed if there are cells to harvest
    if(length(strate.fmu)>0){
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- recoltable2 <-  matrix(0, nrow(strate.fmu), hor.plan)
      for (j in 1:nrow(strate.fmu)){ # j=3
        age.mat.stra <- strate.fmu$AgeMatuPC[j]
        # extraire les TSPC pour la strate et l'UA courante
        TSPCstrate <- land.uea$TSPCut[land.uea$MgmtUnit==unit & land.uea$AgeMatuPC==age.mat.stra] 
        # VOLUME: calcul des valeurs maximales en assumant que toutes les cellules sont a maturité
        # seulement pour la strate et l'UA courante
        land.uea2 <- land.uea[land.uea$MgmtUnit==unit &land.uea$AgeMatuPC == age.mat.stra,]
        land.uea2$Age <- land.uea2$AgeMatu
        vol.max.uea <- sum(volume(land.uea2,km2.pixel)$x)/2
        # volume maximal théorique récoltable par période pour chaque strate
        recoltable2[j,] <- (vol.max.uea/(age.mat.stra/pc.step)) * (1:hor.plan)   
        # on revient à l'âge initial pour les calculs subséquents
        land.uea2 <- land.uea[land.uea$MgmtUnit==unit &land.uea$AgeMatuPC == age.mat.stra,]
        # Determine the period when maturity will be reached for the different age classes
        for (per in 0:(hor.plan-1)) {
                    # on calcule le volume des peuplements matures 
          vol.act <- sum(volume(land.uea2[land.uea2$TSPCut>=land.uea2$AgeMatuPC,],km2.pixel)$x)/2
          recoltable[j,per+1] <- vol.act
          # pour chaque période, on update l'âge des peuplements pour la périopde suivante
          land.uea2$TSPCut <- land.uea2$TSPCut + pc.step
        }

        for (per in (age.mat.stra/pc.step): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      
      recoltable.s[which(unit==recoltable.s$MgmtUnit),2:(hor.plan+1)] <-  
          colSums(recoltable)/(1:hor.plan)

    }
      
    
    
  } #unit
  
  ## Number of cells to harvest (sustained yield level), corresponding to 
  ## the future period with the lowest mature forest availability 
  n.pc.cells <- data.frame(MgmtUnit=recoltable.s$MgmtUnit,
                           x=round(apply(recoltable.s[,2:(hor.plan+1)], 1, min)))

  write.table(n.pc.cells, 
              file = paste0(out.path, "/InitialVolumePC.txt"),
              quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)
  
}
