######################################################################################
###  disturbance.cc()
###
###  Description >  Calculates sustained-yield levels and simulates clear cuts
###                 harvesting for each management unit
###
###  Arguments >  
###   subland : appropiate selection fo the data frame of the state variables
###   cc.step : basic time step of clear cuts
###   period.crisis : T or F to activate an economic crisis happening 10% of the time. 
###               The amount of clearcuts declines below sustainable yield levels during economic crises
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

# subland <- subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, TSF,age.mat), (SppGrp!="NonFor" & !is.na(MgmtUnit)))
                  
disturbance.cc <- function(subland, cc.step, period.crisis, target.old.pct, diff.prematurite, hor.plan, 
                           a.priori, replanif, salvage.rate.event, salvage.rate.FMU, ref.harv.level, write.tbl.outputs=T, km2.pixel=km2.pixel, irun, t, 
                           out.path=NULL, out.overwrite=T){  
  
  # Silence  
  options(warn=-1)

  # Initialize empty vector for the clear cut cells 
  cc.cells <- numeric(0)
  
  # Name of the management units.
  units <- sort(unique(subland$MgmtUnit[!is.na(subland$MgmtUnit)]))

  

  # For each management unit compute available land to cut (burnt and unburnt), then:
  unit=units[17] # for testing
  for(unit in units){  
    
    # Identify locations that can be harvested from those that cannot due to environmental or social constraints.
    # Some excluded areas are identified directly on the map based on local knowledge.
    # We need to consider excluded cells in some calculations because they contribute to
    # biodiversity objectives (even if they cannot be harvested).
    # Differentiate also between young and mature stands.
    s.inc <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 & is.na(subland$Exclus)])
    s.ex <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 & !is.na(subland$Exclus)])
    s.inc.mat <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= subland$age.mat & is.na(subland$Exclus)])
    s.ex.mat <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= subland$age.mat & !is.na(subland$Exclus)])
    
    # categories of burnt area
    s.inc.burnt     <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$TSF == 0 & is.na(subland$Exclus)])
    s.inc.mat.burnt <- length(subland$cell.indx[subland$MgmtUnit == unit &  subland$TSF == 0 & is.na(subland$Exclus)&  subland$TSD >= subland$age.mat])

    # Extract the portion that is managed through even-aged silviculture (clearcutting) based 
    # on species dominance.Some species are mostly managed through even aged silviculture (EPN,
    # SAB, PET, other), the rest through unevenaged silviculture.
    subland.ea1 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("EPN","PET","SAB","other") & is.na(subland$Exclus),]
    subland.ea2 <- subland[subland$MgmtUnit == unit &  subland$TSD >= 0 &  subland$SppGrp %in% c("BOJ","ERS") & is.na(subland$Exclus),]
    subland.ea3 <- subland.ea2[runif(nrow(subland.ea2))<0.05,]
    subland.ea4 <- subland.ea1[runif(nrow(subland.ea1))<0.95,]
    subland.ea <- rbind(subland.ea4,subland.ea3)
    
    # Get the area managed under an even-aged regime
    s.ea <- length(subland.ea$cell.indx)    
    
    # Area in mature (old) forests that should be maintained in the FMUs in order to meet the target
    target.old.ha  <- target.old.pct * (s.inc + s.ex)
    target.old.ha.ea <- max(0, target.old.ha - s.ex.mat)
    target.old.pct.ea <- target.old.ha.ea/s.ea        
    
    # Subset of harvestable (mature even-aged) cells
    subland.rec <- subland.ea[subland.ea$TSD >= subland.ea$age.mat,]
    s.mat <- nrow(subland.rec)
    
    
    #### Determine the sustained yield level 
    
    # Number of strata corresponding to the number of different maturity classes present in
    # each FMU. Only one in the current version
    strates <- sort((unique(subland.ea$age.mat)))
    
    # Calculation of the expected abundance of harvestable stands during future planning periods, 
    # as the stands that are currently young will age and become harvestable
    
    recoltable <- matrix(0,length(strates), hor.plan)
    recoltable2 <- matrix(0,length(strates), hor.plan)
    for (j in 1:length(strates)) { # j=1
      age.mat.stra <- strates[j]
      TSD_strate <- subland.ea$TSD[subland.ea$age.mat==strates[j]]
      # superficie maximale théorique récoltable par période pour chaque strate
      recoltable2[j,] <- length(TSD_strate)/(age.mat.stra/5) * (1:hor.plan)   
      # calculer le moment ou les superficies arriveront à maturité dans le futur
      for (per in 0:(hor.plan-1))  # per=0  
        recoltable[j,per+1] <- sum(TSD_strate >= (age.mat.stra-(per*5)))
      for (per in (age.mat.stra/5): hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }

    # Total harvestable area, all strata combined, minus what has to be kept to satisfy 
    # the ecosystem management target
    recoltable.s <- colSums(recoltable)
    recoltable.s1 <- pmax(0,recoltable.s-target.old.ha.ea)
    recoltable.s2 <- recoltable.s1/(1:hor.plan)
    
    # Define sustainable harvest level at time t=5 (initial conditions). Corresponds to 
    # the  period with the lowest mature forest availability. This harvest level is used in 
    # scenarios where the replanning option is not selected.
    
    if(t==5) {ref.harv.level[unit] <- min(recoltable.s2) * a.priori}
    
    # If the replanning option is not selected, harvest level is the same for all periods. If there is
    # replanning, the adjusted harvest level is used.
    
    if (replanif == 1) {
        recoltable.s3 <- recoltable.s2 * a.priori
    } else {
        recoltable.s3 <- ref.harv.level[unit] # *a.priori
    }
    
    # Number of cells to harvest (sustained yield level) 
    n.cc.cells <-  max(0, round(min(recoltable.s3)*1))
    
    n.cc.cells <- min(recoltable.s1[1],n.cc.cells)
    
    # If the economic crisis variable is TRUE, only 80% of the sustained yield level can be harvested
    if(period.crisis)
    n.cc.cells   <- 0.8*n.cc.cells

    #### Now that the harvest level has been determined for the current period, 
    #### the following procedure selects the cells to be harvested
    #### STRATA ARE NOT TAKEN INTO ACCOUNT DURING this selection    

    
    # Identify mature forests that have burned during the current period, and the proportion that is truly
    # harvestable - they will be harvested in priority (salvage logging)
    
    subland.burnt.mature <- subland.ea[(subland.ea$TSD >= (subland.ea$age.mat-diff.prematurite)) & subland.ea$TSF==0, ]
    cell.burnt.available <- sample(subland.burnt.mature$cell.indx, round(salvage.rate.event*nrow(subland.burnt.mature)), replace=FALSE)

    # Randomly select cells among the even-aged mature cells present in non-protected areas
    # Prioritize clear cuts in burned areas
    if (s.ea!=0) {
      
      # When the number harvestable disturbed cells >= sustained yield level, harvest as much as you can
      if(length(cell.burnt.available) >= round(n.cc.cells*salvage.rate.FMU)) 
        cc.cells.salv <- sample(cell.burnt.available, size=(round(n.cc.cells*salvage.rate.FMU)), replace=FALSE)                             
      # But when the number harvestable disturbed cells  < sustained yield level, harvest them all
      else 
        cc.cells.salv <- cell.burnt.available
      
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
    
    # cc.cells per species group, for the outputs
    vec.cc.compo<- subland[subland$MgmtUnit == unit & (subland$cell.indx %in% cc.cells),]$SppGrp
    harv.EPN <- sum(vec.cc.compo=="EPN")*km2.pixel
    harv.BOJ <- sum(vec.cc.compo=="BOJ")*km2.pixel
    harv.PET <- sum(vec.cc.compo=="PET")*km2.pixel
    harv.SAB <- sum(vec.cc.compo=="SAB")*km2.pixel
    harv.ERS <- sum(vec.cc.compo=="ERS")*km2.pixel
    harv.other <- sum(vec.cc.compo=="other")*km2.pixel
    
    ### mature cells that burned during the period
  
    burn.EPN.m <- sum(subland.burnt.mature$SppGrp=="EPN")*km2.pixel
    burn.BOJ.m <- sum(subland.burnt.mature$SppGrp=="BOJ")*km2.pixel
    burn.PET.m <- sum(subland.burnt.mature$SppGrp=="PET")*km2.pixel
    burn.SAB.m <- sum(subland.burnt.mature$SppGrp=="SAB")*km2.pixel
    burn.ERS.m <- sum(subland.burnt.mature$SppGrp=="ERS")*km2.pixel
    burn.other.m <- sum(subland.burnt.mature$SppGrp=="other")*km2.pixel    
    ### cellules brulees non matures

    subland.burnt.nonmature <- subland.ea[(subland.ea$TSD < (subland.ea$age.mat-diff.prematurite)) & subland.ea$TSF==0, ]
    burn.EPN.y <- sum(subland.burnt.nonmature$SppGrp=="EPN")*km2.pixel
    burn.BOJ.y <- sum(subland.burnt.nonmature$SppGrp=="BOJ")*km2.pixel
    burn.PET.y <- sum(subland.burnt.nonmature$SppGrp=="PET")*km2.pixel
    burn.SAB.y <- sum(subland.burnt.nonmature$SppGrp=="SAB")*km2.pixel
    burn.ERS.y <- sum(subland.burnt.nonmature$SppGrp=="ERS")*km2.pixel
    burn.other.y <- sum(subland.burnt.nonmature$SppGrp=="other")*km2.pixel
        
    # Write the results (clear cut by unit) in an output text file
    if(write.tbl.outputs) 
      write.table(data.frame(run=irun, time=t, UA=unit, tot.inc = s.inc *km2.pixel, even.age=s.ea*km2.pixel, 
                             sup.mat=s.mat*km2.pixel, s.inc.burnt= s.inc.burnt*km2.pixel ,s.inc.mat.burnt=s.inc.mat.burnt*km2.pixel,
                             cc.area.unaff=length(cc.cells.unaff)*km2.pixel, cc.area.salvaged=length(cc.cells.salv)*km2.pixel, prop.salvaged=length(cc.cells.salv)/s.inc.burnt,
                             harv.EPN = harv.EPN,harv.BOJ = harv.BOJ,harv.PET = harv.PET,harv.SAB = harv.SAB,
                             harv.ERS = harv.ERS,harv.other = harv.other,
                             burn.EPN.m=burn.EPN.m,burn.BOJ.m=burn.BOJ.m,burn.PET.m=burn.PET.m,burn.SAB.m=burn.SAB.m,burn.ERS.m=burn.ERS.m,burn.other.m=burn.other.m,
                             burn.EPN.y=burn.EPN.y,burn.BOJ.y=burn.BOJ.y,burn.PET.y=burn.PET.y,burn.SAB.y=burn.SAB.y,burn.ERS.y=burn.ERS.y,burn.other.y=burn.other.y) ,
                  file=paste0(out.path, "/ClearCut.txt"), quote=FALSE, sep="\t",
                  append=(!out.overwrite | unit != "11161"), 
                  row.names=FALSE, col.names=(out.overwrite & unit == "11161")) 
  } # mgmt unit

  # Return the cell.indx of the clear cuts, for all management units together for period t
  return(cc.cells)  

}
