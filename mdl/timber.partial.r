######################################################################################
###  timber.partial()
###  timber supply calculation for partial cuts
######################################################################################

timber.partial <- function(land, hor.plan, km2.pixel, pc.step){  
  
  cat("Timber supply uneven-aged stands - area", "\n")
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  
  ###################################### SUTAINABLE YIELD ######################################
  ## Extract the portion that is managed through uneven-aged silviculture - partial cutting.
  ## Based on stand composition: some species are mostly managed through even aged silviculture
  ## 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
  land2 <- land
  land2$rndm <- runif(nrow(land2))
  
  
 # land2 <- mutate(land2, rndm=runif(nrow(land2)))
  
#  even <- land2$SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB") & is.na(land2$Exclus) & land2$rndm<=0.95
#  sum(even) 
 # even[land2$SppGrp %in% c("BOJ", "ERS", "OthDT")& is.na(land2$Exclus) & land2$rndm>0.95] <- 1
  land2$even[land2$tsfire==0 & land2$spp!="NonFor"] <- 1  ## NÚ - No change harvest regime for NonFor cells.

  land.uea <- land2[land2$even==0,]
  
  land.uea2 <- land.uea[land.uea$tspcut>=0,]   ## NÚ - this condition does not do anything
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea2$age.matu.pc <- round(land.uea2$age.matu,-1)/2   ## NÚ - what does it mean to round " -1 " ?
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea2, mgmt.unit) %>% summarise(x=length(mgmt.unit))    

  # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
  strates <- group_by(land.uea2, mgmt.unit, age.matu.pc) %>% summarize(x=length(unique(age.matu.pc)))

  ## Compute sustainable yield per FMU
  recoltable.s <- cbind(s.uea %>% select(-x), matrix(NA, nrow=nrow(s.uea), ncol=hor.plan))

  ###########################
  ## ON CORRIGE EN FONCTION DES PERTURBATIONS RÉCENTES, PAS DE L'AGE
  ###### A VERIFIER
  
  unit = 9351
  for(unit in unique(land.uea2$mgmt.unit)){
    strate.fmu <- filter(strates, mgmt.unit==unit)
    as.data.frame(strate.fmu)
    
    ## The calculation is only performed if there are cells to harvest
    if(length(strate.fmu)>0){  ## NÚ - The conditions length(strates)>0  does not make sense
                               ## I change to length(strate.fmu)>0
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- recoltable2 <-  matrix(0, nrow(strate.fmu), hor.plan)
      for (j in 1:nrow(strate.fmu)){ # j=3
        age.mat.stra <- strate.fmu$age.matu.pc[j]
        # extraire les TSPC pour la strate et l'UA courante
        TSPCstrate <- land.uea2$tspcut[land.uea2$mgmt.unit==unit & land.uea2$age.matu.pc==age.mat.stra] 
        table(TSPCstrate)
        # superficie maximale théorique récoltable par période pour chaque strate
        recoltable2[j,] <- (length(TSPCstrate)/(age.mat.stra/pc.step)) * (1:hor.plan)   
        # Determine the period when maturity will be reached for the different age classes
        for (per in 0:(hor.plan-1))  
          recoltable[j,per+1] <- sum(TSPCstrate >= (age.mat.stra-(per*pc.step)))
        for (per in (age.mat.stra/pc.step): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      recoltable.s[which(unit==recoltable.s$mgmt.unit),2:(hor.plan+1)] <-  
          colSums(recoltable)/(1:hor.plan)
    }
      
  } #unit
  
  ## Number of cells to harvest (sustained yield level), corresponding to 
  ## the future period with the lowest mature forest availability 
  n.pc.cells <- data.frame(mgmt.unit=recoltable.s$mgmt.unit, 
                           x=round(apply(recoltable.s[,2:(hor.plan+1)], 1, min)))

return(n.pc.cells)    
  
}
