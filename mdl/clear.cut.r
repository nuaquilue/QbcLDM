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
                      salvage.rate.event, salvage.rate.FMU, ref.harv.level, km2.pixel, fire.id, sbw.id){  

  ## Initialize empty vector for the clear cut cells 
  cc.cells <- numeric(0)
  
  ## Name of the management units (76)
  units <- sort(unique(land$MgmtUnit[!is.na(land$MgmtUnit)]))
  
  ## Tracking harvesting per management unit
  track.cut <- data.frame(UA=NA, tot.inc=NA, even.age=NA, sup.mat=NA, s.inc.burnt=NA,
                          s.inc.mat.burnt=NA, s.inc.kill=NA, s.inc.mat.kill=NA,
                          cc.area.unaff=NA, cc.area.salvaged=NA, harv.cc.EPN=NA, 
                          harv.cc.BOJ=NA, harv.cc.PET=NA, harv.cc.SAB=NA, 
                          harv.cc.ERS=NA, harv.cc.other=NA, reg.fail.ex=NA, reg.fail.inc=NA)
    
  ## Harvest rates have to be calculated separately for each management unit:
  for(unit in units){  
    
    ## Count the number of locations that can be harvested (included) from those that cannot (excluded)
    ## due to environmental or  social constraints. These by default excluded areas are identified directly 
    ## on the map based on local knowledge.
    ## We need to consider excluded cells in some calculations because they contribute to
    ## biodiversity objectives (even if they cannot be harvested).
    ## Differentiate also between young and mature stands.
    s.inc <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & is.na(land$Exclus))
    s.ex <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & !is.na(land$Exclus))
    s.inc.mat <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & land$Age >= land$AgeMatu & is.na(land$Exclus))
    s.ex.mat <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & land$Age >= land$AgeMatu & !is.na(land$Exclus)) 
    
    ## For those locations that can be harvested (included), differentiate those that have been burnt or killed
    ## by an outbreak, and then count the young (cannot be salvaged) vs the mature (can be salvaged)
    s.inc.burnt <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & is.na(land$Exclus) & land$TSDist == 0 & land$DistType == fire.id)
    s.inc.mat.burnt <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & is.na(land$Exclus) & land$TSDist == 0 & land$DistType == fire.id & land$Age>=land$AgeMatu)
    s.inc.kill <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & is.na(land$Exclus) & land$TSDist %in% c(0,5) & land$DistType == sbw.id)
    s.inc.mat.kill <- sum(!is.na(land$MgmtUnit) & land$MgmtUnit == unit & is.na(land$Exclus) & land$TSDist %in% c(0,5) & land$DistType == sbw.id & land$Age>=land$AgeMatu)
    
    ## Extract the portion that is managed through even-aged silviculture (clearcutting) based on species dominance. 
    ## Some species are mostly managed through even aged silviculture (EPN, SAB, PET, others), 
    ## the rest (BOJ, ERS) through unevenaged silviculture.
    land.evenage <- filter(land, MgmtUnit == unit & SppGrp %in% c("EPN", "PET", "SAB", "other") & is.na(Exclus))
    land.evenage <- land.evenage[runif(nrow(land.evenage))<0.95,]
    land.unevenage <- filter(land, MgmtUnit == unit & SppGrp %in% c("BOJ", "ERS") & is.na(Exclus))
    land.unevenage <- land.unevenage[runif(nrow(land.unevenage))<0.05,]
    land.ea <- rbind(land.evenage, land.unevenage)
    
    ## Get the area managed under an even-aged regime
    s.ea <- nrow(land.ea)    
    
    ## Area in mature (old) forests that should be maintained in the FMUs in order to meet the conservation target
    target.old.ha  <- target.old.pct * (s.inc + s.ex)
    target.old.ha.ea <- max(0, target.old.ha - s.ex.mat)
    target.old.pct.ea <- target.old.ha.ea/s.ea        
    
    ## Subset of harvestable (mature even-aged) cells
    land.rec <- filter(land.ea, Age >= AgeMatu)
    s.mat <- nrow(land.rec)
    
    
    #################################### Determine the sustained yield level ####################################
    
    # Number of strata corresponding to the number of different ages of maturity present in each FMU. 
    # Only one stratum is used in the current version
    strates <- sort((unique(land.ea$AgeMatu)))
    
    ## Calculation of the expected abundance of harvestable stands during future planning periods, 
    ## as the stands that are currently young will age and become harvestable
    recoltable <- matrix(0,length(strates), hor.plan)
    recoltable2 <- matrix(0,length(strates), hor.plan)
    for (j in 1:length(strates)) { # j=1
      age.mat.stra <- strates[j]
      TSD_strate <- land.ea$TSD[land.ea$AgeMatu==strates[j]]
      # maximum theoretical harvestable area per period for each stratum
      recoltable2[j,] <- length(TSD_strate)/(age.mat.stra/5) * (1:hor.plan)   
      # Determine the period when maturity will be reached for the different age classes
      for (per in 0:(hor.plan-1))  # per=0  
        recoltable[j,per+1] <- sum(TSD_strate >= (age.mat.stra-(per*5)))
      for (per in (age.mat.stra/5): hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }
    
    ## Total harvestable area, all strata combined, minus what has to be kept to satisfy the old forest target
    recoltable.s <- colSums(recoltable)
    recoltable.s1 <- pmax(0,recoltable.s-target.old.ha.ea)
    recoltable.s2 <- recoltable.s1/(1:hor.plan)
    
    ## Define sustainable harvest level at time t=0 (initial conditions). Corresponds to 
    ## the period with the lowest mature forest availability. This harvest level is used in 
    ## scenarios where the replanning option is not activated.
    if(t==0) 
        ref.harv.level[which(units %in% unit)] <- min(recoltable.s2) * a.priori
    
    ## If the replanning option is not activated, harvest level is the same for all periods. If there is
    ## replanning, the adjusted harvest level (recalculated at each period for each FMU) is used.
    if(replan)
      recoltable.s3 <- recoltable.s2 * a.priori
    else 
      recoltable.s3 <- ref.harv.level[which(units %in% unit)] 
    
    # Number of cells to harvest (sustained yield level) 
    n.cc.cells <-  max(0, round(min(recoltable.s3)*1))
    n.cc.cells <- min(recoltable.s1[1], n.cc.cells)
    
    
    ############################## Select cells to be harvested ##############################
    ## Now that the harvest level has been determined for the current period, 
    ## select the cells to be harvested
    ## STRATA ARE NOT TAKEN INTO ACCOUNT during this selection    
    ## First, Identify mature forests that have been affected by fire or severe SBW outbreaks during the current period, 
    ## and the proportion that is truly harvestable - they will be harvested in priority (salvage logging)
    ## burned cells must be salvaged immediately, but there are two periods to salvage SBW cells
    land.salv.mature <- filter(land.ea, Age>=AgeMatu-diff.prematurite &
                              ((TSDist==0 & DistType==fire.id) | (TSDist%in%c(0,5) & DistType==sbw.id)))
    cell.salv.available <- sample(land.salv.mature$cell.id, 
                                  round(salvage.rate.event*nrow(land.salv.mature)), replace=FALSE)
    
    ## Initialize vectors of cells
    cc.cells.salv <- numeric()
    cc.cells.unaff <- numeric()
    
    ## Randomly select cells among the even-aged mature cells present in non-protected areas
    ## Prioritize clear cuts in disturbed areas
    if(s.ea!=0){
      ## When the number harvestable disturbed cells >= sustained yield level, select as many as you can
      if(length(cell.salv.available) >= round(n.cc.cells*salvage.rate.FMU)) 
        cc.cells.salv <- sample(cell.salv.available, size=(round(n.cc.cells*salvage.rate.FMU)), replace=FALSE)                             
      ## But when the number harvestable disturbed cells  < sustained yield level, select them all
      else 
        cc.cells.salv <- cell.salv.available
      
      ## When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
      ## forests unaffected by disturbances (cc.cells.unaff).
      if (length(cc.cells.salv) < n.cc.cells) {
        land.non.pertu <- filter(land.rec, TSDist!=0)
        size.n <- min((n.cc.cells-length(cc.cells.salv)), nrow(land.non.pertu)) # number of harvesting cells
        cc.cells.unaff <- sample(land.non.pertu$cell.id, size=size.n, replace=FALSE)
      } 
    }
    
    ## Finally, combine all types of harvested cells with those already harvested in other FMUs 
    ## during the same time period
    cc.cells <- c(cc.cells, cc.cells.salv, cc.cells.unaff)
    
    
    ## TRACKING
    ## Species cut in the current management unit
    vec.cc.compo <- filter(land, MgmtUnit==unit, land$cell.id %in% c(cc.cells.salv, cc.cells.unaff)) %>%
                   select(SppGrp)
    ## Zones at defforestation risk in the UA, both included and excluded
    reg.fail.ex <- filter(land, !is.na(MgmtUnit), MgmtUnit==unit, SppGrp %in% c("EPN","SAB"), 
                          TSDist==0, DistType==fire.id, Age<=50, !is.na(Exclus) )
    reg.fail.inc <- filter(land, !is.na(MgmtUnit), MgmtUnit==unit, SppGrp %in% c("EPN","SAB"), 
                          TSDist==0, DistType==fire.id, Age<=50, is.na(Exclus) )
    ## Add all the info about the current UA
    track.cut <- rbind(track.cut, 
                       data.frame(UA=unit, tot.inc=s.inc*km2.pixel, even.age=s.ea*km2.pixel, sup.mat=s.mat*km2.pixel, 
                                  s.inc.burnt=s.inc.burnt*km2.pixel, s.inc.mat.burnt=s.inc.mat.burnt*km2.pixel, 
                                  s.inc.kill=s.inc.kill*km2.pixel, s.inc.mat.kill=s.inc.mat.kill*km2.pixel,
                                  cc.area.unaff=length(cc.cells.unaff)*km2.pixel, 
                                  cc.area.salvaged=length(cc.cells.salv)*km2.pixel, 
                                  harv.cc.EPN=sum(vec.cc.compo=="EPN")*km2.pixel, 
                                  harv.cc.BOJ=sum(vec.cc.compo=="BOJ")*km2.pixel, 
                                  harv.cc.PET=sum(vec.cc.compo=="PET")*km2.pixel, 
                                  harv.cc.SAB=sum(vec.cc.compo=="SAB")*km2.pixel, 
                                  harv.cc.ERS=sum(vec.cc.compo=="ERS")*km2.pixel, 
                                  harv.cc.other=sum(vec.cc.compo=="other")*km2.pixel, 
                                  reg.fail.ex=nrow(reg.fail.ex)*km2.pixel, 
                                  reg.fail.inc=nrow(reg.fail.ex)*km2.pixel) )
  } # mgmt unit
  
  # Return the cell.id of the cut locations and the tracking info
  return(list(cc.cells=cc.cells, track.cut=track.cut[-1,]))  
  
}
