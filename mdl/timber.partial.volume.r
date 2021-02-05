######################################################################################
###  timber.partial()
###  timber supply calculation for partial cuts
######################################################################################

timber.partial.volume <- function(land, hor.plan, km2.pixel, pc.step, courbes){  
  
  cat("Timber supply uneven aged stands - volume", "\n")
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  
  ###################################### SUTAINABLE YIELD ######################################
  ## Extract the portion that is managed through uneven-aged silviculture - partial cutting.
  ## Based on stand composition: some species are mostly managed through even aged silviculture
  ## 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
  land <- mutate(land, rndm=runif(nrow(land)))
  land$mgmt.unit <- as.numeric(as.character(land$mgmt.unit))
  land2 <- land[!is.na(land$mgmt.unit),]
  
  land.evenage <- filter(land2, spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N")
                         & is.na(exclus) & rndm<=0.05) 
  land.unevenage <- filter(land2,  spp %in% c("BOJ", "ERS", "OTH.FEU.S") 
                           & is.na(exclus) & rndm>0.95) 
  land.uea <- rbind(land.evenage, land.unevenage)
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$age.matu.pc <- round(land.uea$age.matu,-1)/2
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, mgmt.unit) %>% summarise(x=length(mgmt.unit))    
  
  # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
  strates <- group_by(land.uea, mgmt.unit, age.matu.pc) %>% summarize(x=length(unique(age.matu.pc)))
  #unique(land.uea$age.matu.pc)
  ## Compute sustainable yield per FMU
  recoltable.s <- cbind(s.uea %>% select(-x), matrix(NA, nrow=nrow(s.uea), ncol=hor.plan))

  ### corriger TSPC en fonction des perturbations s?v?res r?centes
  ### les peuplements sont accessibles ? la coupe partielle 15 ans avant d'?tre matures
  
  vsc.cor.pc2 <- land.uea$age <land.uea$age.matu.pc
  land.uea$tspcut[vsc.cor.pc2] <- land.uea$age[vsc.cor.pc2] - (land.uea$age.matu.pc[vsc.cor.pc2]-15)
        
      ### MATHIEU, here you were using the variable TSDist that is no longer in use
      ### I guess TSF and/or TSCcut should be used but not sure. Just check!
  vsc.cor.pc <- (land.uea$tsfire <land.uea$age.matu.pc) | (land.uea$tsccut <land.uea$age.matu.pc)
  land.uea$tspcut[vsc.cor.pc] <- land.uea$tsfire[vsc.cor.pc] - (land.uea$age.matu.pc[vsc.cor.pc]-15)
  #land.uea$vol.max <- volume(land.uea,km2.pixel)$x
  unit = 9351
  for(unit in unique(land.uea$mgmt.unit)){
    strate.fmu <- filter(strates, mgmt.unit==unit)
    as.data.frame(strate.fmu)
    ## The calculation is only performed if there are cells to harvest
    if(length(strate.fmu)>0){
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- recoltable2 <-  matrix(0, nrow(strate.fmu), hor.plan)
      for (j in 1:nrow(strate.fmu)){ # j=3
        age.mat.stra <- strate.fmu$age.matu.pc[j]
        # extraire les TSPC pour la strate et l'UA courante
        TSPCstrate <- land.uea$TSPcut[land.uea$mgmt.unit==unit & land.uea$age.matu.pc==age.mat.stra] 
        # VOLUME: calcul des valeurs maximales en assumant que toutes les cellules sont a maturit?
        # seulement pour la strate et l'UA courante
        land.uea2 <- land.uea[land.uea$mgmt.unit==unit &land.uea$age.matu.pc == age.mat.stra,]
        land.uea2$age <- land.uea2$age.matu.pc
        vol.max.uea <- sum((volume.vec(land.uea2, courbes))*400)/2
        # volume maximal th?orique r?coltable par p?riode pour chaque strate
        recoltable2[j,] <- (vol.max.uea/(age.mat.stra/pc.step)) * (1:hor.plan)   
        # on revient ? l'?ge initial pour les calculs subs?quents
        land.uea2 <- land.uea[land.uea$mgmt.unit==unit &land.uea$age.matu.pc == age.mat.stra,]
        # Determine the period when maturity will be reached for the different age classes
        for (per in 0:(hor.plan-1)) {
                    # on calcule le volume des peuplements matures 
        # vol.act <- sum(volume(land.uea2[land.uea2$TSPCut>=land.uea2$age.matu.pc,],km2.pixel)$x)/2
          vol.act <- sum((volume.vec(land.uea2[land.uea2$tspcut>=land.uea2$age.matu.pc,], courbes))*400)/2
          
          recoltable[j,per+1] <- vol.act
          # pour chaque p?riode, on update l'?ge des peuplements pour la p?riopde suivante
          land.uea2$tspcut <- land.uea2$tspcut + pc.step
        }

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

  write.table(n.pc.cells, 
              file = paste0(out.path, "/InitialVolumePC.txt"),
              quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)
  return(n.pc.cells)
}
