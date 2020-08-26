######################################################################################
###  clear.cut()
###
###  Description >  Calculates sustained-yield levels and simulates clear cuts
###                 harvesting for each management unit
###
###  Arguments >  
###   land : appropiate selection fo the data frame of the state variables
###   cc.step : basic time step of clear cuts
###   age.mat : mature age for even aged populations to be harvested
###   target.old.pct : minimal proportion of cells occupied by mature forests to 
###                   to keep in each unit to 
###                   maintain habitats for animal and plant species
###   diff.prematurite  : a number of years before maturity (e.g. 80y). Defines the minimal age 
###                       of stands that can be salvage-logged (after fire)
###   hor.plan : length of the planning horizon when calculating sustained yield levels
###   salvage.rate.event : Realistic maximal proportion of the mature forests that were burnt by a given fire event 
###                        that can be salvage logged
###   salvage.rate.FMU: Realistic maximal proportion of the harvested area that can be represneted by burnt stands
###                  in a given FMU (to account for the fact that mills cannot take 100% burnt wood)
###   write.tbl.outputs : if TRUE
###   km2.pixel : number of km2 per pixel on the grid 
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###   out.path : directory path to save output text files
###   out.overwrite : if TRUE the output text files are overwritten 
###
###  Details > For each management unit and each period, calculate a theoretically sustainable harvesting rate, 
###            and harvest it in burnt and non-burnt stands
###
###  Value >  A vector of the indexes of the harvested cells.
######################################################################################

clear.cut <- function(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                      salvage.rate.event, salvage.rate.FMU, harv.level, km2.pixel, fire.id, sbw.id, t){  

  cat("Clearcutting", "\n" )
             
  ## Initialize empty vector for the clear cut cells 
  cc.cells.salv <- cc.cells.unaff <- numeric(0)

  
  ###################################### SUTAINABLE YIELD ######################################
  ## Count the number of locations that can be harvested (included) from those that cannot (excluded)
  ## due to environmental or  social constraints. These by default excluded areas are identified directly 
  ## on the map based on local knowledge.
  ## We need to consider excluded cells in some calculations because they contribute to
  ## biodiversity objectives (even if they cannot be harvested).
  ## Differentiate also between young and mature stands.
  s.inc <- filter(land, !is.na(MgmtUnit) & is.na(Exclus)) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.ex <-  filter(land, !is.na(MgmtUnit) & !is.na(Exclus)) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & Age>AgeMatu) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.ex.mat <- filter(land, !is.na(MgmtUnit) & !is.na(Exclus) & Age>AgeMatu) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  
  ## For those locations that can be harvested (included), differentiate those that have been burnt or killed
  ## by an outbreak, and then count the young (cannot be salvaged) vs the mature (can be salvaged)
  s.inc.burnt <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & TSDist==0 & DistType==fire.id) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat.burnt <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & TSDist==0 & DistType==fire.id & Age>AgeMatu) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.kill <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & TSDist%in%c(0,5) & DistType==sbw.id) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat.kill <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & TSDist%in%c(0,5) & DistType==sbw.id & Age>AgeMatu) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  
  ## Also, look for zones at defforestation risk, both included and excluded
  reg.fail.ex <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "SAB", "OthCB"), 
                        TSDist==0, DistType==fire.id, Age<=50, !is.na(Exclus)) %>%
                 group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  reg.fail.inc <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "SAB", "OthCB"), 
                         TSDist==0, DistType==fire.id, Age<=50, is.na(Exclus)) %>%
                  group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  
  ## Select locations that are managed through even-aged silviculture (clearcutting) based on species dominance. 
  ## Some species are mostly managed through even aged silviculture (EPN, SAB, PET, OthCB, OthCT), while the rest
  ## are managed through unevenaged silviculture (BOJ, ERS, OthDB, OthDT).
  land <- mutate(land, rndm=runif(nrow(land)))
  land.evenage <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB")
                         & is.na(Exclus) & rndm<=0.95) 
  land.unevenage <- filter(land, !is.na(MgmtUnit) & SppGrp %in% c("BOJ", "ERS", "OthDT") 
                           & is.na(Exclus) & rndm<=0.05) 
  land.ea <- rbind(land.evenage, land.unevenage)
  
  ## Subset the mature even-aged cells from those that are harvestable
  land.rec <- filter(land.ea, Age>=AgeMatu)
  
  ## Get the area managed under an even-aged regime, and the area harevstable
  s.ea <- group_by(land.ea, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  s.mat <- group_by(land.rec, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  
  ## Area in mature (old) forests that should be maintained in the FMUs in order to meet the conservation target
  target.old.ha  <- s.inc %>% mutate(x=target.old.pct * (s.inc$x + s.ex$x))
  target.old.ha.ea <- target.old.ha %>% mutate(x=pmax(0, target.old.ha$x - s.ex.mat$x))
  
  # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
  # Only one stratum is used in the current version
  strates <- group_by(land.ea, MgmtUnit, AgeMatu) %>% summarize(x=length(unique(AgeMatu)))
  
  ## Compute sustainable yield per FMU
  recoltable.s <- cbind(s.ea %>% select(-x), matrix(NA, nrow=nrow(s.ea), ncol=hor.plan))
  for(unit in unique(land.ea$MgmtUnit)){
    strate.fmu <- filter(strates, MgmtUnit==unit)
    
    ## Calculation of the expected abundance of harvestable stands during future planning periods, 
    ## as the stands that are currently young will age and become harvestable
    recoltable <- recoltable2 <- matrix(0, nrow(strate.fmu), hor.plan)
    for(j in 1:nrow(strate.fmu)){ 
      age.mat.stra <- strate.fmu$AgeMatu[j]
      TSDstrate <- land.ea$TSDist[land.ea$MgmtUnit==unit & land.ea$AgeMatu==age.mat.stra]
      # maximum theoretical harvestable area per period for each stratum
      recoltable2[j,] <- length(TSDstrate)/(age.mat.stra/cc.step) * (1:hor.plan)   
      # Determine the period when maturity will be reached for the different age classes
      for(per in 0:(hor.plan-1))  
        recoltable[j,per+1] <- sum(TSDstrate >= (age.mat.stra-(per*cc.step)))
      for(per in (age.mat.stra/cc.step):hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }
    
    ## Total harvestable area, all strata combined, minus what has to be kept to satisfy the old forest target
    recoltable.s[which(unit==unique(land.ea$MgmtUnit)),2:(hor.plan+1)] <-  
      pmax(0, colSums(recoltable)-target.old.ha.ea$x[target.old.ha.ea$MgmtUnit==unit])/(1:hor.plan) 
    
  } #unit
  
  
  ## Compute the sustainable harvesting level per management unit at time t=0 (initial conditions). 
  ## Corresponds to the period with the lowest mature forest availability. 
  ## If the replanning option is not activated, harvest level is the same for all periods. If there is
  ## replanning, the adjusted harvest level (recalculated at each period for each FMU) is used.
  if(replan | t==0)
    harv.level <- data.frame(MgmtUnit=recoltable.s$MgmtUnit,
                             x=apply(recoltable.s[,2:(hor.plan+1)], 1, min) * a.priori)
  
  ## Finally, compute the number of cells to be harvest (sustained yield level) 
  n.cc.cells <- harv.level %>% mutate(x=pmin(unlist(recoltable.s[2]), pmax(0, round(harv.level$x))))
  
  
  ############################## SELECT CELLS TO BE HARVESTED ##############################
  ## Now that the harvest level has been determined for the current period, select the cells to be harvested
  ## STRATA ARE NOT TAKEN INTO ACCOUNT during this selection    
  
  ## First, Identify mature forests that have been affected by fire or severe SBW outbreaks during the current period, 
  ## and the proportion that is truly harvestable - they will be harvested in priority (salvage logging).
  ## gurned cells must be salvaged immediately, but there are two periods to salvage SBW cells
  land.salv.mature <- filter(land.ea, Age>=AgeMatu-diff.prematurite &
                            ((TSDist==0 & DistType==fire.id) | (TSDist%in%c(0,5) & DistType==sbw.id)))
  
  ## For each management unit, randomly select cells among the even-aged mature cells present in non-protected areas
  ## Prioritize clear cuts in disturbed areas (burnt or killed by outbreak).
  for(unit in unique(land.ea$MgmtUnit)){
    
    ## Initialize vectors of cells
    cells.salv <- numeric()
    cells.unaff <- numeric()
    
    ## If the area for even-aged harvesting is > 0 and the number of cells that can be sustainably harvested...
    if(s.ea$x[s.ea$MgmtUnit==unit]!=0 & n.cc.cells$x[n.cc.cells$MgmtUnit==unit]>0){
      
      ## Find available cells for salvage logging
      land.salv.mature.fmu <- filter(land.salv.mature, MgmtUnit==unit)
      cell.salv.available <- numeric()
      if(nrow(land.salv.mature.fmu)==1)
        cell.salv.available <- land.salv.mature.fmu$cell.id
      else(nrow(land.salv.mature.fmu)>1)
        cell.salv.available <- sample(land.salv.mature.fmu$cell.id, 
                                      round(salvage.rate.event*nrow(land.salv.mature.fmu)), replace=FALSE)
      
      ## When the number harvestable disturbed cells >= sustained yield level, select as many as you can
      ## But when the number harvestable disturbed cells< sustained yield level, select them all
      if(length(cell.salv.available) > round(n.cc.cells$x[n.cc.cells$MgmtUnit==unit]*salvage.rate.FMU)) 
        cells.salv <- sample(cell.salv.available, size=(round(n.cc.cells$x[n.cc.cells$MgmtUnit==unit]*salvage.rate.FMU)), replace=FALSE)                             
      else 
        cells.salv <- cell.salv.available 
      
      ## When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
      ## forests unaffected by disturbances (cc.cells.unaff).
      if(length(cc.cells.salv) < n.cc.cells$x[n.cc.cells$MgmtUnit==unit]){
        land.non.pertu <- filter(land.rec, TSDist!=0)
        size.n <- min((n.cc.cells$x[n.cc.cells$MgmtUnit==unit]-length(cc.cells.salv)), nrow(land.non.pertu)) 
        cells.unaff <- sample(land.non.pertu$cell.id, size=size.n, replace=FALSE)
      } 
      
      ## Finally, combine all types of harvested cells with those already harvested in other FMUs 
      ## during the same time period
      cc.cells.salv <- c(cc.cells.salv, cells.salv)
      cc.cells.unaff <- c(cc.cells.unaff, cells.unaff)
    }
  }
    
  ## INVESTIGATE WHY there are duplicates and cell.id not in land
  cc.cells <- unique(c(cc.cells.salv, cc.cells.unaff))
  cc.cells <- cc.cells[cc.cells %in% land$cell.id]
  
  
  ################################################# TRACKING #################################################
  ## Area salvaged logged and area harvested
  s.salv <- filter(land, cell.id %in% cc.cells.salv) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  s.unaff <- filter(land, cell.id %in% cc.cells.unaff) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  
  ## Species cut by management unit
  spp.ccut <- filter(land, cell.id %in% c(cc.cells.salv, cc.cells.unaff)) %>%
              group_by(MgmtUnit, SppGrp) %>% summarize(x=length(MgmtUnit)*km2.pixel) 
  
  ## Merge all the info 
  track <- left_join(s.inc, s.ea, by="MgmtUnit") %>% left_join(s.mat, by="MgmtUnit") %>% 
           left_join(s.inc.burnt, by="MgmtUnit") %>% left_join(s.inc.mat.burnt, by="MgmtUnit") %>%
           left_join(s.inc.kill, by="MgmtUnit") %>% left_join(s.inc.mat.kill, by="MgmtUnit") %>%
           left_join(reg.fail.ex, by="MgmtUnit") %>% left_join(reg.fail.inc, by="MgmtUnit") %>%
           left_join(s.salv, by="MgmtUnit") %>% left_join(s.unaff, by="MgmtUnit")
  names(track)[2:ncol(track)] <- c("tot.inc", "even.age", "s.mat", "s.inc.burnt", "s.inc.mat.burnt",
     "s.inc.kill", "s.inc.mat.kill", "reg.fail.ex", "reg.fail.in", "area.salvaged", "area.unaff")
  track[,2:ncol(track)] <- track[,2:ncol(track)]*km2.pixel
  track[is.na(track)] <- 0
  
  ## Return the cell.id of the cut locations and the tracking info
  return(list(cc.cells=cc.cells, track.cut=track, spp.ccut=spp.ccut))  
  
}
