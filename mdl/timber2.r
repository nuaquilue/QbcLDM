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

timber2 <- function(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, harv.level, km2.pixel, t){  

  cat("Timber supply even aged stands - area", "\n" )
             
  # Initialize empty vector for the clear cut cells 
  cc.cells <- numeric(0)
  n.cc.cells<- numeric(0)
  
  # Name of the management units.
  land2 <- land[!is.na(land$mgmt.unit),]
  land2$even[land2$tsfire==0 & land2$spp!="NonFor"] <- 1 ## but only for forest spp. f
  
  units <- sort(unique(land2$mgmt.unit[!is.na(land2$mgmt.unit)]))
  
  # Harvest rates have to be calculated separately for each management unit:
  unit=units[4] # for testing  unit="2662"
  for(unit in units){  
    # cat(unit, "\n")
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
    
    s.inc.burnt     <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$tsfire==0 & is.na(land2$exclus)])
    s.inc.mat.burnt <- length(land2$cell.id[land2$mgmt.unit == unit &  (land2$age >= land2$age.matu) &  land2$tsfire==0 & is.na(land2$exclus)])
    s.inc.kill     <- length(land2$cell.id[land2$mgmt.unit == unit &  land2$age >= 0 &  land2$tssbw %in% c(0,5) & is.na(land2$exclus)])
    s.inc.mat.kill <- length(land2$cell.id[land2$mgmt.unit == unit &  (land2$age >= land2$age.matu) &  land2$tssbw %in% c(0,5) & is.na(land2$exclus)])
    
    #print(paste("tordeuse",s.inc.kill,s.inc.mat.kill))
    
    # Extract the portion that is managed through even-aged silviculture (clearcutting) based 
    # on species dominance. Some species are mostly managed through even aged silviculture (EPN,
    # SAB, PET, others), the rest through unevenaged silviculture.

   # even <- land2$mgmt.unit == unit & land2$SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB") & is.na(land2$exclus) & land2$rndm<=0.95
  #  sum(even) 
  #  even[land2$mgmt.unit == unit & land2$SppGrp %in% c("BOJ", "ERS", "OthDT")& is.na(land2$exclus) & land2$rndm>0.95] <- 1

    
    land.ea <- land2[land2$mgmt.unit == unit & land2$even==1,] 
    dim(land.ea)
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
    
    # Number of strata corresponding to the number of different ages of maturity present in
    # each FMU. Only one stratum is used in the current version
    strates <- sort((unique(land.ea$age.matu)))
      # table(land.ea$age.matu)
    
    # Calculation of the expected abundance of harvestable stands during future planning periods, 
    # as the stands that are currently young will age and become harvestable
    recoltable <- matrix(0,length(strates), hor.plan)
    recoltable2 <- matrix(0,length(strates), hor.plan)
    for(j in 1:length(strates)) { # j=1
      age.mat.stra <- strates[j]
      TSD_strate <- land.ea$age[land.ea$age.matu==strates[j]]
      # maximum theoretical harvestable area per period for each stratum
      recoltable2[j,] <- length(TSD_strate)/(age.mat.stra/5) * (1:hor.plan)   
      # Determine the period when maturity will be reached for the different age classes
      for(per in 0:(hor.plan-1))  # per=0  
        recoltable[j,per+1] <- sum(TSD_strate >= (age.mat.stra-(per*5)))
      for(per in (age.mat.stra/5):hor.plan)
        recoltable[j,per] <- recoltable2[j,per]
    }
    
    # Total harvestable area, all strata combined, minus what has to be kept to satisfy 
    # the old forest target
    recoltable.s <- colSums(recoltable)
    recoltable.s1 <- pmax(0,recoltable.s-target.old.ha.ea)
    recoltable.s2 <- recoltable.s1/(1:hor.plan)
    # a priori reduction in maximal allowable harvest level to buffer fire impacts    
    recoltable.s3 <- recoltable.s2 * a.priori
    # Number of cells to harvest (sustained yield level) 
    n.cc.cells.UA <-  max(0, round(min(recoltable.s3)*1))
    n.cc.cells <- c(n.cc.cells,n.cc.cells.UA)
    
  }
  
  ## Return number of cells to cut per mgmt unit (with actual names of management units)
  n.cc.cells <- data.frame(mgmt.unit=units, x=n.cc.cells)
  return(n.cc.cells)  
  
}
