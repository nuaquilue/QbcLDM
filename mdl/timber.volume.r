######################################################################################
###  Description >  Calculates sustained-yield levels for each FMU in volume
###
######################################################################################

timber.volume <- function(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, 
                           harv.level, km2.pixel, t, courbes){  

  cat("Timber supply even-aged stands in volume", "\n" )
             
  # Initialize empty vector for the clear cut cells 
  cc.cells <- numeric(0)
  initial.volume<- numeric(0)
  
  # Name of the management units.
  land2 <- land[!is.na(land$mgmt.unit),]
  
  units <- as.character(sort(unique(land2$mgmt.unit[!is.na(land2$mgmt.unit)])))
  
  # Harvest rates have to be calculated separately for each management unit:
  unit=units[17] # for testing  unit="2662"
  for(unit in units){  
    cat(unit, "\n")
    # Separate locations that can be harvested (included) from those that cannot due to environmental or 
    # social constraints (excluded).
    # Some excluded areas are identified directly on the map based on local knowledge.
    # We need to consider excluded cells in some calculations because they contribute to
    # biodiversity objectives (even if they cannot be harvested).
    # Differentiate also between young and mature stands.
    
    s.inc <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= 0 & is.na(land2$exclus)])
    s.ex  <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= 0 & !is.na(land2$exclus)])
    s.inc.mat <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= land2$age.matu & is.na(land2$exclus)])
    s.ex.mat  <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= land2$age.matu & !is.na(land2$exclus)])
    
    # categories of burned area - young (cannot be salvaged) vs mature (can be salvaged)
    
    s.inc.burnt     <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= 0 &  land2$tsfire ==0 & is.na(land2$exclus)])
    s.inc.mat.burnt <- length(land2$cell.id[land2$mgmt.unit == unit &  (land2$age >= land2$age.matu) &  land2$tsfire & is.na(land2$exclus)])
    s.inc.kill     <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= 0 &  land2$tssbw %in% c(0,5) & is.na(land2$exclus)])
    s.inc.mat.kill <- length(land2$cell.id[land2$mgmt.unit == unit &  (land2$age >= land2$age.matu) & land2$tssbw %in% c(0,5) & is.na(land2$exclus)])
    
    #print(paste("tordeuse",s.inc.kill,s.inc.mat.kill))
    
    # Extract the portion that is managed through even-aged silviculture (clearcutting) based 
    # on species dominance. Some species are mostly managed through even aged silviculture (EPN,
    # SAB, PET, others), the rest through unevenaged silviculture.
    land.ea1 <- land2[land2$mgmt.unit == unit &  
                      land2$spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N") & is.na(land2$exclus),]
    land.ea2 <- land2[land2$mgmt.unit == unit &  
                      land2$spp %in% c("BOJ", "ERS", "OTH.FEU.S") & is.na(land2$exclus),]
    land.ea3 <- land.ea2[runif(nrow(land.ea2))<0.05,]
    land.ea4 <- land.ea1[runif(nrow(land.ea1))<0.95,]
    land.ea <- rbind(land.ea4,land.ea3)
    
    # Get the area managed under an even-aged regime
    s.ea <- length(land.ea$cell.id)   
    
    # Area in mature (old) forests that should be maintained in the FMUs in order to meet the conservation target
    target.old.ha  <- target.old.pct * (s.inc + s.ex)
    target.old.ha.ea <- max(0, target.old.ha - s.ex.mat)
    target.old.pct.ea <- target.old.ha.ea/s.ea        
    
    # Subset of harvestable (mature even-aged) cells
    land.rec <- land.ea[land.ea$age >= land.ea$age.matu,]
    s.mat <- nrow(land.rec)
    
    
    #### Determine the sustained yield level
    #### approche alternative volume
    strates <- sort((unique(land.ea$age.matu)))
    recoltable <- matrix(0,length(strates), hor.plan)
    recoltable2 <- matrix(0,length(strates), hor.plan)
    for (j in 1:length(strates)) { # j=1
      age.mat.stra <- strates[j]
      TSD_strate <- land.ea$age[land.ea$age.matu==strates[j]]


      # maximum theoretical harvestable area per period for each stratum
      # VOLUME: ON ASSUME QUE LES PEUPLEMENTS SONT EXACTEMENT ? MATURIT?
      land.ea2 <- land.ea[land.ea$age.matu == age.mat.stra,]
      land.ea2$age <- land.ea2$age.matu
      vol.max <- sum(volume.vec(land.ea2, courbes))*400
      
      recoltable2[j,] <- vol.max/(age.mat.stra/5) * (1:hor.plan)   
      # on revient ? l'?ge initial pour les calculs subs?quents
      land.ea2 <- land.ea[land.ea$age.matu == age.mat.stra,]
      # Determine the period when maturity will be reached for the different age classes
      for (per in 0:(hor.plan-1)) {# per=0  
        # on calcule le volume des peuplements matures 
        vol.act <- sum(volume.vec(land.ea2[land.ea2$age>=land.ea2$age.matu,], courbes))*400
        recoltable[j,per+1] <- vol.act
      # pour chaque periode, on update l'?ge des peuplements pour la p?riopde suivante
        land.ea2$age <- land.ea2$age + 5
      } 

      for (per in (age.mat.stra/5): hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }
    
    recoltable.s <- colSums(recoltable)
    recoltable.s1 <- recoltable.s# pmax(0,recoltable.s-target.old.ha.ea)
    recoltable.s2 <- recoltable.s1/(1:hor.plan)
    recoltable.s3 <- recoltable.s2 * a.priori
    vol.UA <-  max(0, round(min(recoltable.s3)*1))
    initial.volume <- rbind(initial.volume,as.data.frame(cbind(unit,(vol.UA))))
  }
  
  write.table(initial.volume, file = paste0(out.path, "/InitialVolume.txt"),
              quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)
  return(initial.volume)
  
}
