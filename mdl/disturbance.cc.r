######################################################################################
###  disturbance.cc()
###
###  Description >  Calculates sustained-yield levels and simulates clear cuts
###                 harvesting for each management unit
###
###  Arguments >  
###   subland : appropiate selection fo the data frame of the state variables
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

disturbance.cc <- function(subland, cc.step, target.old.pct, diff.prematurite, hor.plan, 
                           a.priori, replanif, salvage.rate.event, salvage.rate.FMU, ref.harv.level, 
                           write.tbl.outputs=T, 
                           km2.pixel=km2.pixel, irun=1, t=5, 
                           out.path=NULL, out.overwrite) {  
  
  # Silence  
  options(warn=-1)

  # Initialize empty vector for the clear cut cells 
  cc.cells <- numeric(0)
  
  # Name of the management units.
  units <- sort(unique(subland$MgmtUnit[!is.na(subland$MgmtUnit)]))

  # Harvest rates have to be calculated separately for each management unit:
  unit=units[17] # for testing
  for(unit in units){  
    
    # Separate locations that can be harvested (included) from those that cannot due to environmental or 
    # social constraints (excluded).
    # Some excluded areas are identified directly on the map based on local knowledge.
    # We need to consider excluded cells in some calculations because they contribute to
    # biodiversity objectives (even if they cannot be harvested).
    # Differentiate also between young and mature stands.
    
    s.inc <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 & is.na(subland$Exclus)])
    s.ex  <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 & !is.na(subland$Exclus)])
    s.inc.mat <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= subland$age.mat & is.na(subland$Exclus)])
    s.ex.mat  <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= subland$age.mat & !is.na(subland$Exclus)])
    
    # categories of burned area - young (cannot be salvaged) vs mature (can be salvaged)

    s.inc.burnt     <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$TSF == 0 & is.na(subland$Exclus)])
    s.inc.mat.burnt <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSF == 0 & is.na(subland$Exclus)&  subland$TSD >= subland$age.mat])
    s.inc.kill     <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$TSE %in% c(0,5) & is.na(subland$Exclus)])
    s.inc.mat.kill <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSE %in% c(0,5) & is.na(subland$Exclus)&  subland$TSD >= subland$age.mat])
    
    #print(paste("tordeuse",s.inc.kill,s.inc.mat.kill))
    
    # Extract the portion that is managed through even-aged silviculture (clearcutting) based 
    # on species dominance. Some species are mostly managed through even aged silviculture (EPN,
    # SAB, PET, others), the rest through unevenaged silviculture.
    subland.ea1 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("EPN","PET","SAB","other") & is.na(subland$Exclus),]
    subland.ea2 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("BOJ","ERS") & is.na(subland$Exclus),]
    subland.ea3 <- subland.ea2[runif(nrow(subland.ea2))<0.05,]
    subland.ea4 <- subland.ea1[runif(nrow(subland.ea1))<0.95,]
    subland.ea <- rbind(subland.ea4,subland.ea3)
    
    # Get the area managed under an even-aged regime
    s.ea <- length(subland.ea$cell.indx)    
    
    # Area in mature (old) forests that should be maintained in the FMUs in order to meet the conservation target
    target.old.ha  <- target.old.pct * (s.inc + s.ex)
    target.old.ha.ea <- max(0, target.old.ha - s.ex.mat)
    target.old.pct.ea <- target.old.ha.ea/s.ea        
    
    # Subset of harvestable (mature even-aged) cells
    subland.rec <- subland.ea[subland.ea$TSD >= subland.ea$age.mat,]
    s.mat <- nrow(subland.rec)
    
    
    #### Determine the sustained yield level
    
    # Number of strata corresponding to the number of different ages of maturity present in
    # each FMU. Only one stratum is used in the current version
    strates <- sort((unique(subland.ea$age.mat)))
    
    # Calculation of the expected abundance of harvestable stands during future planning periods, 
    # as the stands that are currently young will age and become harvestable
    
    recoltable <- matrix(0,length(strates), hor.plan)
    recoltable2 <- matrix(0,length(strates), hor.plan)
    for (j in 1:length(strates)) { # j=1
      age.mat.stra <- strates[j]
      TSD_strate <- subland.ea$TSD[subland.ea$age.mat==strates[j]]
      # maximum theoretical harvestable area per period for each stratum
      recoltable2[j,] <- length(TSD_strate)/(age.mat.stra/5) * (1:hor.plan)   
      # Determine the period when maturity will be reached for the different age classes
      for (per in 0:(hor.plan-1))  # per=0  
        recoltable[j,per+1] <- sum(TSD_strate >= (age.mat.stra-(per*5)))
      for (per in (age.mat.stra/5): hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }

    # Total harvestable area, all strata combined, minus what has to be kept to satisfy 
    # the old forest target
    recoltable.s <- colSums(recoltable)
    recoltable.s1 <- pmax(0,recoltable.s-target.old.ha.ea)
    recoltable.s2 <- recoltable.s1/(1:hor.plan)
    
    # Define sustainable harvest level at time t=5 (initial conditions). Corresponds to 
    # the  period with the lowest mature forest availability. This harvest level is used in 
    # scenarios where the replanning option is not activated.
    
    if(t==5) {ref.harv.level[unit] <- min(recoltable.s2) * a.priori}
    
    # If the replanning option is not activated, harvest level is the same for all periods. If there is
    # replanning, the adjusted harvest level (recalculated at each period for each FMU) is used.
    
    if (replanif == 1) {
        recoltable.s3 <- recoltable.s2 * a.priori
    } else {
        recoltable.s3 <- ref.harv.level[unit] # *a.priori
    }
    
    # Number of cells to harvest (sustained yield level) 
    n.cc.cells <-  max(0, round(min(recoltable.s3)*1))
    
    n.cc.cells <- min(recoltable.s1[1],n.cc.cells)

    #### Now that the harvest level has been determined for the current period, 
    #### the following procedure selects the cells to be harvested
    #### STRATA ARE NOT TAKEN INTO ACCOUNT during this selection    

    # Identify mature forests that have been affected by fire or severe SBW outbreaks during the current 
    # period, and the proportion that is truly harvestable - they will be harvested in priority (salvage logging)
    # burned cells must be salvaged immediately, but there are two periods to salvage SBW cells
    
    subland.salv.mature <- subland.ea[(subland.ea$TSD >= (subland.ea$age.mat-diff.prematurite)) & 
                                         (subland.ea$TSF==0 | subland.ea$TSE%in%c(0,5)), ]

    cell.salv.available <- sample(subland.salv.mature$cell.indx, round(salvage.rate.event*nrow(subland.salv.mature)), replace=FALSE)

    # Randomly select cells among the even-aged mature cells present in non-protected areas
    # Prioritize clear cuts in disturbed areas
    if (s.ea!=0) {
      
      # When the number harvestable disturbed cells >= sustained yield level, select as many as you can
      if(length(cell.salv.available) >= round(n.cc.cells*salvage.rate.FMU)) 
        cc.cells.salv <- sample(cell.salv.available, size=(round(n.cc.cells*salvage.rate.FMU)), replace=FALSE)                             
      # But when the number harvestable disturbed cells  < sustained yield level, select them all
      else 
        cc.cells.salv <- cell.salv.available
      
      # When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
      # forests unaffected by disturbances (cc.cells.unaff).

      if (length(cc.cells.salv) < n.cc.cells) {
        subland.non.pertu <- subland.rec[subland.rec$TSF!=0, ]
        size.n <- min((n.cc.cells-length(cc.cells.salv)), nrow(subland.non.pertu)) # number of harvesting cells
        cc.cells.unaff <- sample( subland.non.pertu$cell.indx, size=size.n, replace=FALSE)
      } 
      # When there were enough salvaged cells to fulfill the SY constraint, 
      # then no cell will be harvested in non-disturbed forests
      else 
        cc.cells.unaff <- numeric(0)
      
    } else {   # If there's no area to be harvested
      cc.cells.salv <- numeric(0)
      cc.cells.unaff <- numeric(0)
    }
    
    # Combine all types of harvested cells with those already harvested in other FMUs during the same period
    cc.cells <- c(cc.cells, cc.cells.salv, cc.cells.unaff)
    
    
    ####### compiler superficies par groupe d'essences
    
    vec.cc.compo<- subland[subland$MgmtUnit == unit & (subland$cell.indx %in% c(cc.cells.salv,cc.cells.unaff)),]$SppGrp
    harv.cc.EPN <- sum(vec.cc.compo=="EPN")*km2.pixel
    harv.cc.BOJ <- sum(vec.cc.compo=="BOJ")*km2.pixel
    harv.cc.PET <- sum(vec.cc.compo=="PET")*km2.pixel
    harv.cc.SAB <- sum(vec.cc.compo=="SAB")*km2.pixel
    harv.cc.ERS <- sum(vec.cc.compo=="ERS")*km2.pixel
    harv.cc.other <- sum(vec.cc.compo=="other")*km2.pixel
    
    
    ############  superficies à risque de déforestation - inclues dans les UA, ou exclues
    
    reg.fail.ex <-   length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSF == 0 & subland$SppGrp  %in% c("EPN","SAB")
                                           & !is.na(subland$Exclus)  & subland$TSD %in% c(5,10,15,20,25,30,35,40,45,50)]) *km2.pixel
    reg.fail.inc  <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSF == 0 & subland$SppGrp %in% c("EPN","SAB")
                                           & is.na(subland$Exclus)  & subland$TSD %in% c(5,10,15,20,25,30,35,40,45,50)]) *km2.pixel
    
    
    # Write the results (clear cut by unit) in an output text file
    if(write.tbl.outputs) 
      write.table(data.frame(run=irun, time=t, UA=unit, tot.inc = s.inc *km2.pixel, even.age=s.ea*km2.pixel, 
                             sup.mat=s.mat*km2.pixel, s.inc.burnt= s.inc.burnt*km2.pixel ,s.inc.mat.burnt=s.inc.mat.burnt*km2.pixel,
                             s.inc.kill= s.inc.kill*km2.pixel, s.inc.mat.kill=s.inc.mat.kill*km2.pixel,
                             cc.area.unaff=length(cc.cells.unaff)*km2.pixel, cc.area.salvaged=length(cc.cells.salv)*km2.pixel, 
                             harv.cc.EPN = harv.cc.EPN,harv.cc.BOJ = harv.cc.BOJ,harv.cc.PET = harv.cc.PET,
                             harv.cc.SAB = harv.cc.SAB, harv.cc.ERS = harv.cc.ERS,harv.cc.other = harv.cc.other,
                             reg.fail.ex = reg.fail.ex,reg.fail.inc = reg.fail.inc) ,
                  file=paste0(out.path, "/ClearCut.txt"), quote=FALSE, sep="\t",
                  append=(!out.overwrite | unit != "11161"), 
                  row.names=FALSE, col.names=(out.overwrite & unit == "11161")) 
  } # mgmt unit

  # Return the cell.indx of the clear cuts, for all management units together for period t
  return(cc.cells)  

}
