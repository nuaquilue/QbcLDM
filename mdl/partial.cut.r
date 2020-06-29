######################################################################################
###  partial.cut()
###
######################################################################################

partial.cut <- function(land, hor.plan, km2.pixel, time.step){  
  
  ## Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  ## Name of the management units.
  units <- sort(unique(land$MgmtUnit[!is.na(land$MgmtUnit)]))
  
  ## Tracking harvesting per management unit
  track.cut <- data.frame(UA=NA, uneven.age=NA, sup.mat=NA, s.rec.pc=NA,
                         harv.pc.EPN=NA, harv.pc.BOJ=NA, harv.pc.PET=NA,
                         harv.pc.SAB=NA, harv.pc.ERS=NA, harv.pc.other=NA)

  # For each management unit compute available land to cut (burnt and unburnt), then:
  for(unit in units){  

    ## Extract the portion that is managed through uneven-aged silviculture - partial cutting.
    ## Based on stand composition: some species are mostly managed through even aged silviculture
    ## 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
    land.evenage <- filter(land, MgmtUnit == unit & SppGrp %in% c("EPN", "PET", "SAB", "other") & is.na(Exclus))
    land.evenage <- land.evenage[runif(nrow(land.evenage))<0.05,]
    land.unevenage <- filter(land, MgmtUnit == unit & SppGrp %in% c("BOJ", "ERS") & is.na(Exclus))
    land.unevenage <- land.unevenage[runif(nrow(land.unevenage))<0.95,]
    land.uea <- rbind(land.evenage, land.unevenage)
    
    ## Get the number of cells to be managed under a partial-cut regime
    s.uea <- nrow(land.uea)    
    
    ## The maturity age for partial cuts is half the maturity age for a clear cut
    land.uea$AgeMatuPC <- round(land.uea$AgeMatu/2)
    
    ## Subset of harvestable (i.e. mature even-aged) cells
    ## TSDist is also reset at 0 after partial cut, so no need to indicate the type of the last disturbance
    land.rec.pc <- filter(land.uea, TSDist>=AgeMatuPC)
    s.mat <- nrow(land.rec.pc)
    
    
    #################################### Determine the sustained yield level ####################################
    ## Number of strata corresponding to different ages of maturity present in each FMU. 
    strates <-  sort((unique(land.uea$AgeMatuPC)))
    
    ## The calculation is only performed if there are cells to harvest
    if(length(strates)>0){
      
      ## Calculation of the expected abundance of harvestable stands during future planning periods, 
      ## as the stands that are currently young will age and become harvestable
      recoltable <- matrix(0,length(strates), hor.plan)
      recoltable2 <- matrix(0,length(strates), hor.plan)
      for (j in 1:length(strates)) { # j=1
        age.mat.stra <- strates[j]
        land.uea.str <- land.uea[land.uea$AgeMatuPC==strates[j],]
        # pour être admissibles à une CP les peuplements doivent être à la fois
        # matures et ne pas avoir été traités depuis un certain temps. Pour le
        # moment le temps restant à écouler avant la maturité n'est pas pris en compte
        # (donc le taux de coupe partielle sous évalué durant les premieres périodes)
        TSPC_strate <- land.uea.str$TSDist 
        # superficie maximale théorique récoltable par période pour chaque strate
        recoltable2[j,] <- (length(TSPC_strate)/(age.mat.stra/time.step)) * (1:hor.plan)   
        # calculer le moment ou les superficies arriveront ? maturit? dans le futur
        for (per in 0:(hor.plan-1))  # per=0  
          recoltable[j,per+1] <- sum(TSPC_strate >= (age.mat.stra-(per*5)))
        for (per in (age.mat.stra/5): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      recoltable.s <- colSums(recoltable)
      recoltable.s2 <- recoltable.s/(1:hor.plan)

      ## Number of cells to harvest (sustained yield level), corresponding to 
      ## the future period with the lowest mature forest availability 
      n.pc.cells <- max(0, round(min(recoltable.s2)*1))
    }
      
    ############################## Select cells to be harvested ##############################      
    ## Now that the optimal yield level has been determined, 
    ## select the cells to be harvested during the current period
    ## Randomly select cells among the even-aged mature cells present in non-protected areas
    ## Strata are not considered at this point - mature cells are all put together
    ## Prioritize clear cuts in burnt areas
    pc.cells.ua <- numeric()
    if(n.pc.cells>0) 
      pc.cells.ua <- sample(land.rec.pc$cell.id, size=n.pc.cells, replace=FALSE)
    
    ## Add the cells partially cut in this UA:
    pc.cells <- c(pc.cells, pc.cells.ua)   
    
    
    ## TRACKING
    ## Species partially cut in the current management unit
    vec.pc.compo <- filter(land, MgmtUnit == unit & land$cell.id %in% pc.cells.x) %>% select(SppGrp)
    ## Add all the info about the current UA
    track.cut <- data.frame(UA=unit, uneven.age=s.uea*km2.pixel, sup.mat=s.mat*km2.pixel, 
                            s.rec.pc=n.pc.cells*km2.pixel,
                            harv.pc.EPN=sum(vec.pc.compo=="EPN")*km2.pixel, 
                            harv.pc.BOJ=sum(vec.pc.compo=="BOJ")*km2.pixel, 
                            harv.pc.PET=sum(vec.pc.compo=="PET")*km2.pixel,
                            harv.pc.SAB=sum(vec.pc.compo=="SAB")*km2.pixel, 
                            harv.pc.ERS=sum(vec.pc.compo=="ERS")*km2.pixel, 
                            harv.pc.other=sum(vec.pc.compo=="other")*km2.pixel)
    
 } # mgmt unit

  # Return the cell.id of the partially cut locations and the tracking info
  return(list(pc.cells=pc.cells, track.cut=track.cut[-1,]))  

}
