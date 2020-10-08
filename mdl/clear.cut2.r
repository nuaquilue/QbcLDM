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
###
###  Details > For each management unit and each period, calculate a theoretically sustainable harvesting rate, 
###            and harvest it in burnt and non-burnt stands
###
###  Value >  A vector of the indexes of the harvested cells.
######################################################################################

clear.cut2 <- function(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                      salvage.rate.event, salvage.rate.FMU, harv.level, km2.pixel, fire.id, sbw.id, t){  

   
  #harv.level <- ref.harv.level
  cat("Clearcutting", "\n" )
  
  land2 <- land[!is.na(land$MgmtUnit),]
  
  units <- as.character(sort(unique(land2$MgmtUnit[!is.na(land2$MgmtUnit)])))
  
  s.inc <- filter(land, !is.na(MgmtUnit) & is.na(Exclus)) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat <- filter(land, !is.na(MgmtUnit) & is.na(Exclus) & Age>AgeMatu) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))

  
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
  #target.old.ha  <- s.inc %>% mutate(x=target.old.pct * (s.inc$x + s.ex$x))
  #target.old.ha.ea <- target.old.ha %>% mutate(x=pmax(0, target.old.ha$x - s.ex.mat$x))
  
  cc.cells.salv.tot <- cc.cells.unaff.tot <- cc.cells <- numeric(0)
  
  unit=units[18] # for testing
  for(unit in units){
    harv.level.u <- harv.level[harv.level$MgmtUnit == as.numeric(unit),2]
    
    land.ea.u <- land.ea[land.ea$MgmtUnit==unit,]
    
    s.ea.u <- length(land.ea.u$cell.id)  
    
    # Subset of harvestable (mature even-aged) cells
    land.ea.mat.u <- land.ea.u[land.ea.u$Age >= land.ea.u$AgeMatu,]

    subland.salv.mature.burn <- land.ea.u[(land.ea.u$Age >= (land.ea.u$AgeMatu-diff.prematurite)) & 
                                            land.ea.u$DistType == fire.id & land.ea.u$TSDist ==0, ]
    subland.salv.mature.sbw <- land.ea.u[(land.ea.u$Age >= (land.ea.u$AgeMatu-diff.prematurite)) & 
                                           land.ea.u$DistType == sbw.id & land.ea.u$TSDist %in% c(0,5), ]  
    subland.salv.mature <- rbind(subland.salv.mature.burn,subland.salv.mature.sbw)

    land.ea.u[land.ea.u$DistType==1,]
    
    cell.salv.available <- sample(subland.salv.mature$cell.id, round(salvage.rate.event*nrow(subland.salv.mature)), replace=FALSE)
  
    # Randomly select cells among the even-aged mature cells present in non-protected areas
    # Prioritize clear cuts in disturbed areas (salvage logging)
    if (s.ea.u!=0) {
    
      # When the number harvestable disturbed cells >= sustained yield level, select as many as you can
      if(length(cell.salv.available) >= round(harv.level.u*salvage.rate.FMU)) {
        cc.cells.salv <- sample(cell.salv.available, size=(round(harv.level.u*salvage.rate.FMU)), replace=FALSE)           
        }      else {
        # But when the number harvestable disturbed cells  < sustained yield level, select them all
        cc.cells.salv <- cell.salv.available
        }
    
      # When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
      # forests unaffected by disturbances (cc.cells.unaff).
    
      if (length(cc.cells.salv) < harv.level.u) {
        subland.non.pertu <- land.ea.mat.u[land.ea.mat.u$TSDist!=0, ]
        size.n <- min((harv.level.u-length(cc.cells.salv)), nrow(subland.non.pertu)) # number of harvesting cells
        cc.cells.unaff <- sample( subland.non.pertu$cell.id, size=size.n, replace=FALSE)
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
    cc.cells.salv.tot <- c(cc.cells.salv.tot, cc.cells.salv)
    cc.cells.unaff.tot <- c(cc.cells.unaff.tot, cc.cells.unaff)
    
  }
  length(cc.cells.unaff.tot)
 
  
  ################################################# TRACKING #################################################
  ## Area salvaged logged and area harvested
  s.salv <- filter(land, cell.id %in% cc.cells.salv.tot) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  s.unaff <- filter(land, cell.id %in% cc.cells.unaff.tot) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  
  ## Species cut by management unit
  spp.ccut <- filter(land, cell.id %in% c(cc.cells.salv.tot, cc.cells.unaff.tot)) %>%
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
