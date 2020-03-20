######################################################################################
###  disturbance.pc()
###
######################################################################################

# subland <- subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, TSF,age.mat,TSPC), (SppGrp!="NonFor" & !is.na(MgmtUnit)))
# out.overwrite=T

                 
disturbance.pc <- function(subland, cc.step, hor.plan, 
                           write.tbl.outputs=T, km2.pixel=km2.pixel, irun, t, 
                           out.path=NULL, out.overwrite=T){  
  
  # Silence  
  options(warn=-1)

  # Initialize empty vector for the clear cut cells 
  pc.cells <- numeric(0)
  
  # Name of the management units.
  units <- sort(unique(subland$MgmtUnit[!is.na(subland$MgmtUnit)]))

  # For each management unit compute available land to cut (burnt and unburnt), then:
  unit=units[59] # for testing
  for(unit in units){  

    # Extract the portion that is managed through uneven-aged silviculture - partial cutting.
    # Based on stand composition: some species are mostly managed through even aged silviculture
    # 5% of stands dominated by species traditionally managed through clear cuts are managed through PCs
    subland.uea1 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("EPN","PET","SAB","other") & is.na(subland$Exclus),]
    subland.uea2 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("BOJ","ERS") & is.na(subland$Exclus),]
    subland.uea3 <- subland.uea1[runif(nrow(subland.uea1))<0.05,]
    subland.uea4 <- subland.uea2[runif(nrow(subland.uea2))<0.95,]
    subland.uea <- rbind(subland.uea4,subland.uea3)
    
    # Get the area managed under a partial-cut regime
    s.uea <- length(subland.uea$cell.indx)    
    
    # age.maturité pc : moitié de l'age de maturité coupe totale
    subland.uea$age.mat.pc <- round(subland.uea$age.mat/10)*5    
    
    # Subset of harvestable (mature even-aged) cells
    #subland.rec <- subland.uea[subland.uea$TSD >= subland.uea$age.mat,]
    subland.rec.pc <- subland.uea[subland.uea$TSPC >= (subland.uea$age.mat.pc),]
    s.mat <- nrow(subland.rec.pc)
    
    # subland.uea <- subland.rec.pc
    #### Determine the sustained yield level 
    
    # Number of strata corresponding to different ages of maturity present in
    # each FMU. 
    
    strates <-  sort((unique(subland.uea$age.mat.pc)))
    
    # Calculation of the expected abundance of harvestable stands during future planning periods, 
    # as the stands that are currently young will age and become harvestable
    # The calculation is only performed if there are cells to harvest
    if (length(strates)>0) {

      recoltable <- matrix(0,length(strates), hor.plan)
      recoltable2 <- matrix(0,length(strates), hor.plan)
      for (j in 1:length(strates)) { # j=1
        age.mat.stra <- strates[j]
        subland.uea.str <- subland.uea[subland.uea$age.mat.pc==strates[j],]
        # pour être admissibles à une CP les peuplements doivent être à la fois
        # matures et ne pas avoir été traités depuis un certain temps. Pour le
        # moment le temps restant à écouler avant la maturité n'est pas pris en compte
        # (donc le taux de coupe partielle sous évalué durant les premieres périodes)
        TSPC_strate <- subland.uea.str$TSPC # * (subland.uea.str$TSD >= subland.uea.str$age.mat)
        # superficie maximale théorique récoltable par période pour chaque strate
        recoltable2[j,] <- (length(TSPC_strate)/(age.mat.stra/5)) * (1:hor.plan)   
        # calculer le moment ou les superficies arriveront à maturité dans le futur
        for (per in 0:(hor.plan-1))  # per=0  
          recoltable[j,per+1] <- sum(TSPC_strate >= (age.mat.stra-(per*5)))
        for (per in (age.mat.stra/5): hor.plan)
          recoltable[j,per] <- recoltable2[j,per]
      }

      # Total harvestable area, all strata combined
      recoltable.s <- colSums(recoltable)
      recoltable.s2 <- recoltable.s/(1:hor.plan)

      # Number of cells to harvest (sustained yield level) - corresponding to 
      # the future period with the lowest mature forest availability 

          n.pc.cells <- max(0, round(min(recoltable.s2)*1))
    
      #### Now that the optimal yield level has been determined, 
      #### select the cells to be harvested during the current period

      # Randomly select cells among the even-aged mature cells present in non-protected areas
      # Strata are not considered at this point - mature cells are all put together
      # Prioritize clear cuts in burnt areas
      if (n.pc.cells>0) {
        # Available cells for PC: tspc
          subland.non.pertu <- subland.rec.pc#[subland.rec.pc$TSPC>=(subland.rec.pc$age.mat.pc), ]
          size.n <- min((n.pc.cells), nrow(subland.non.pertu))
          pc.cells.x <- sample( subland.non.pertu$cell.indx, size=size.n, replace=FALSE)
      } else {
        pc.cells.x <- numeric(0)
        }
    } else {
      pc.cells.x <- numeric(0)
    }
            
    ####### compiler superficies par groupe d'essences
    
    vec.pc.compo<- subland[subland$MgmtUnit == unit & (subland$cell.indx %in% pc.cells.x),]$SppGrp
    harv.pc.EPN <- sum(vec.pc.compo=="EPN")*km2.pixel
    harv.pc.BOJ <- sum(vec.pc.compo=="BOJ")*km2.pixel
    harv.pc.PET <- sum(vec.pc.compo=="PET")*km2.pixel
    harv.pc.SAB <- sum(vec.pc.compo=="SAB")*km2.pixel
    harv.pc.ERS <- sum(vec.pc.compo=="ERS")*km2.pixel
    harv.pc.other <- sum(vec.pc.compo=="other")*km2.pixel
    
    pc.cells <- c(pc.cells ,pc.cells.x )   
    # Write the results (clear cut by unit) in a output text file
    if  (write.tbl.outputs) 
      write.table(data.frame(run=irun, time=t, UA=unit , uneven.age=s.uea*km2.pixel, 
                             sup.mat=s.mat*km2.pixel, s.rec.pc= n.pc.cells*km2.pixel,
                             harv.pc.EPN = harv.pc.EPN,harv.pc.BOJ = harv.pc.BOJ,harv.pc.PET = harv.pc.PET,
                             harv.pc.SAB = harv.pc.SAB,
                             harv.pc.ERS = harv.pc.ERS,harv.pc.other = harv.pc.other) ,
                  file=paste0(out.path, "/PartialCut.txt"), quote=FALSE, sep="\t",
                  append=(!out.overwrite | unit != "11161"), 
                  row.names=FALSE, col.names=(out.overwrite & unit == "11161")) 
    pc.cells.x <- 0  
 } # mgmt unit

  # Return the cell.indx of the clear cuts, for all management units together for period t
  return(pc.cells)  

}
