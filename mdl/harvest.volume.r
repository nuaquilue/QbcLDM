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

harvest.vol <- function(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                      salvage.rate.event, salvage.rate.FMU, harv.level, km2.pixel, fire.id, sbw.id, t){  

   
  #harv.level <- ref.harv.level
  cat("Select clearcut and partial cut cells - volume based", "\n" )
  
  land2 <- land[!is.na(land$MgmtUnit),]
    source("mdl/volume.vec.r") 
  land2$vol <- (volume.vec(land2)*km2.pixel*100)
  land2 <- land2[order(-land2$vol),]

  units <- as.character(sort(unique(land2$MgmtUnit[!is.na(land2$MgmtUnit)])))

  
  s.inc <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus)) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus) & Age>AgeMatu) %>% group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))

  
  ## For those locations that can be harvested (included), differentiate those that have been burnt or killed
  ## by an outbreak, and then count the young (cannot be salvaged) vs the mature (can be salvaged)
  s.inc.burnt <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus) & TSDist==0 & DistType==fire.id) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat.burnt <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus) & TSDist==0 & DistType==fire.id & Age>AgeMatu) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.kill <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus) & TSDist%in%c(0,5) & DistType==sbw.id) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  s.inc.mat.kill <- filter(land2, !is.na(MgmtUnit) & is.na(Exclus) & TSDist%in%c(0,5) & DistType==sbw.id & Age>AgeMatu) %>% 
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  
  ## Also, look for zones at defforestation risk, both included and excluded
  reg.fail.ex <- filter(land2, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "SAB", "OthCB"), 
                        TSDist==0, DistType==fire.id, Age<=50, !is.na(Exclus)) %>%
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))
  reg.fail.inc <- filter(land2, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "SAB", "OthCB"), 
                         TSDist==0, DistType==fire.id, Age<=50, is.na(Exclus)) %>%
    group_by(MgmtUnit) %>% summarise(x=length(MgmtUnit))

  
  land2 <- mutate(land2, rndm=runif(nrow(land2)))
  land.coniferes <- filter(land2, !is.na(MgmtUnit) & SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB")
                         & is.na(Exclus) & rndm<=0.95) 
  land.feuillu.tol <- filter(land2, !is.na(MgmtUnit) & SppGrp %in% c("BOJ", "ERS", "OthDT") 
                           & is.na(Exclus) & rndm<=0.05) 
  
  land.ea <- rbind(land.coniferes, land.feuillu.tol)
  
  ### possibilité
  poss.init <-  read.table("InitialVolume.txt", header=T)  

  ## Subset the mature even-aged cells from those that are harvestable
  land.rec <- filter(land.ea, Age>=AgeMatu)
  
  # initialisation des variables
  cc.cells.salv.tot <- cc.cells.unaff.tot <- cc.cells <- numeric(0)
  
  unit=2371 #units[18] # for testing
  for(unit in units){
    #harv.level.u <- harv.level[harv.level$MgmtUnit == as.numeric(unit),2]
    harv.level.u <- poss.init[poss.init$unit == as.numeric(unit),2]
    
    land.ea.u <- land.ea[land.ea$MgmtUnit==unit,]
    
    s.ea.u <- length(land.ea.u$cell.id)  
    
    # Subset of harvestable (mature even-aged) cells
    land.ea.mat.u <- land.ea.u[land.ea.u$Age >= land.ea.u$AgeMatu,]

    subland.salv.mature.burn <- land.ea.u[(land.ea.u$Age >= (land.ea.u$AgeMatu-diff.prematurite)) & 
                                            land.ea.u$DistType == fire.id & land.ea.u$TSDist ==0, ]
    subland.salv.mature.sbw <- land.ea.u[(land.ea.u$Age >= (land.ea.u$AgeMatu-diff.prematurite)) & 
                                           land.ea.u$DistType == sbw.id & land.ea.u$TSDist %in% c(0,5), ]  
    subland.salv.mature <- rbind(subland.salv.mature.burn,subland.salv.mature.sbw)
    
    # sélection de cellules récupérables en tenant compte
    # des contraintes a priori (maximum salvage rate, etc.)
    cell.salv.available <- sample(subland.salv.mature$cell.id, round(salvage.rate.event*nrow(subland.salv.mature)), replace=FALSE)
     
    #############################################
    # Randomly select cells among the even-aged mature cells present in non-protected areas
    # Prioritize clear cuts in disturbed areas (salvage logging)
    # VOLUME: CELLS SELECTED FROM THE HIGHEST TO LOWEST VOLUMES, UNTIL CONDITION MET
    x <- sum(subland.salv.mature[subland.salv.mature$cell.id %in%cell.salv.available,]$vol )
    cc.cells.salv <-  numeric(0)
    xx <-0
    
    while (x > 0 & length(cell.salv.available)>0 & xx < harv.level.u)    {
      paquet <- ifelse((x > 550000) & (harv.level.u-xx > 550000),5,1 ) # pour accélérer le calcul, paquets de 5 cellules
      cc.cells.salv.x <- cell.salv.available[1:paquet]  #sample(cell.salv.available,1)
      cc.cells.salv <- c(cc.cells.salv,cc.cells.salv.x)
      cell.salv.available <- cell.salv.available[-which(cell.salv.available%in%cc.cells.salv.x)]
      x <- sum(subland.salv.mature[subland.salv.mature$cell.id %in%cell.salv.available,]$vol )
      xx <- sum(land.ea[land.ea$cell.id %in%cc.cells.salv,]$vol )
    }
    
    # When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
    # forests unaffected by disturbances (cc.cells.unaff).

    subland.non.pertu <- land.ea.mat.u[land.ea.mat.u$TSDist!=0, ]
    x <- sum(subland.non.pertu$vol )
    cc.cells.unaff <- numeric(0)
    #  arrête la récolte lorsqu'on est rendu à < 40000m3 du but, il y aura parfois des dépassements
    while(x > 0 & length(subland.non.pertu$cell.id)>0 & xx < (harv.level.u-40000)) {
       paquet <- ifelse((x > 550000) & (harv.level.u-xx > 550000),5,1 )
       cc.cells.unaff.x <- subland.non.pertu[1:paquet,]$cell.id #sample( subland.non.pertu$cell.id, 1)
       cc.cells.unaff   <- c(cc.cells.unaff,cc.cells.unaff.x)
       subland.non.pertu <- subland.non.pertu[-which(subland.non.pertu$cell.id%in%cc.cells.unaff.x),]
       x <- sum(subland.non.pertu$vol,na.rm=T )
       xx <- sum(land.ea[land.ea$cell.id %in%c(cc.cells.salv,cc.cells.unaff),]$vol )
     }
       

    # Combine all types of harvested cells with those already harvested in other FMUs during the same period
    cc.cells <- c(cc.cells, cc.cells.salv, cc.cells.unaff)
    cc.cells.salv.tot <- c(cc.cells.salv.tot, cc.cells.salv)
    cc.cells.unaff.tot <- c(cc.cells.unaff.tot, cc.cells.unaff)
    
  }
  length(cc.cells.unaff.tot)
 
  ############################
  ################## partial cuts
  
  land.uea <- land2[land2$cell.id %in% land.ea$cell.id ,]

  # volume par cellule. Moitié du volume accessible
  land.uea$vol <- land.uea$vol/2
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$AgeMatuPC <- round(land.uea$AgeMatu,-1)/2
  
  ## Subset of harvestable (i.e. mature uneven-aged, ot recently partial cut) cells
  land.rec.pc <- filter(land.uea, Age>=(AgeMatu-15) & TSDist >=(AgeMatu-15) & TSPCut >=AgeMatuPC )
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  s.mat <- group_by(land.rec.pc, MgmtUnit) %>% summarise(x=length(MgmtUnit))    
  
  harv.level.pc <- read.table("InitialVolumePC.txt", header=T)
  pc.cells <- 0
  for(unit in unique(land.uea$MgmtUnit)){  #unit=9351

  
    poss.cp.ua <- harv.level.pc$x[harv.level.pc$MgmtUnit==unit]
    cell.dispo.ua <- land.rec.pc[land.rec.pc$MgmtUnit==unit, ]
    x <- sum(cell.dispo$vol )
    pc.cells.ua <- numeric(0)
    xx <- 0
    #  arrête la récolte lorsqu'on est rendu à < 20000m3 du but, il y aura parfois des dépassements
    while(x > 0 & length(cell.dispo.ua$cell.id)>0 & xx < (poss.cp.ua-20000) ) {
      paquet <- ifelse((x > 260000) & (poss.cp.ua-xx > 260000),5,1 )
      pc.cells.ua.x <- cell.dispo.ua[1:paquet,]$cell.id #sample( subland.non.pertu$cell.id, 1)
      pc.cells.ua   <- c(pc.cells.ua,pc.cells.ua.x)
      cell.dispo.ua <- cell.dispo.ua[-which(cell.dispo.ua$cell.id%in%pc.cells.ua.x),]
      x <- sum(cell.dispo.ua$vol,na.rm=T ) # volume restant
      xx <- sum(land.uea[land.uea$cell.id %in%c(pc.cells.ua),]$vol ) # volume récolté
    }
    
      ## Add the cells partially cut in this UA:
      pc.cells <- c(pc.cells, pc.cells.ua)   
  }    
  
  length(pc.cells)
  ################################################# TRACKING #################################################
  ## Area salvaged and non-salvaged, clearcut
  ## areas are in cells, volumes are in m3
  
  
  a.salv <- filter(land2, cell.id %in% cc.cells.salv.tot) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  a.unaff <- filter(land2, cell.id %in% cc.cells.unaff.tot) %>% group_by(MgmtUnit) %>% summarize(x=length(MgmtUnit))
  
  ### Volume logged, clearcut
  v.salv <- filter(land.ea, cell.id %in% cc.cells.salv.tot) %>% group_by(MgmtUnit) %>% summarize(x=sum(vol))
  v.unaff <- filter(land.ea, cell.id %in% cc.cells.unaff.tot) %>% group_by(MgmtUnit) %>% summarize(x=sum(vol))
  
  ## Area clearcut per species per management unit (clearcut)
  spp.ccut <- filter(land2, cell.id %in% c(cc.cells.salv.tot, cc.cells.unaff.tot)) %>%
              group_by(MgmtUnit, SppGrp) %>% summarize(x=length(MgmtUnit)) 
  ## Volume clearcut per species per management unit (clearcut)
  spp.ccut.vol <- filter(land2, cell.id %in% c(cc.cells.salv.tot, cc.cells.unaff.tot)) %>%
    group_by(MgmtUnit, SppGrp) %>% summarize(x=sum(vol)) 
  
  ## Area partial cut per management unit
  a.pcut  <- filter(land2, cell.id %in% pc.cells) %>% group_by(MgmtUnit) %>% 
    summarize(x=length(MgmtUnit)) 
  ## Volume partial cut per management unit
  v.pcut  <- filter(land2, cell.id %in% pc.cells) %>% group_by(MgmtUnit) %>% 
    summarize(x=sum(vol)) 

  ## Area clearcut per species per management unit (clearcut)
  spp.pcut <- filter(land2, cell.id %in% c(pc.cells)) %>%
    group_by(MgmtUnit, SppGrp) %>% summarize(x=length(MgmtUnit)) 
  ## Volume clearcut per species per management unit (clearcut)
  spp.pcut.vol <- filter(land2, cell.id %in% c(pc.cells)) %>%
    group_by(MgmtUnit, SppGrp) %>% summarize(x=sum(vol))
     
  ## Merge all the info, FMU level
  track <- left_join(s.inc, s.ea, by="MgmtUnit") %>% left_join(s.mat, by="MgmtUnit") %>% 
           left_join(s.inc.burnt, by="MgmtUnit") %>% left_join(s.inc.mat.burnt, by="MgmtUnit") %>%
           left_join(s.inc.kill, by="MgmtUnit") %>% left_join(s.inc.mat.kill, by="MgmtUnit") %>%
           left_join(reg.fail.ex, by="MgmtUnit") %>% left_join(reg.fail.inc, by="MgmtUnit") %>%
           left_join(a.salv, by="MgmtUnit") %>% left_join(a.unaff, by="MgmtUnit") %>%
           left_join(v.salv, by="MgmtUnit") %>% left_join(v.unaff, by="MgmtUnit") %>%  
           left_join(a.pcut, by="MgmtUnit") %>% left_join(v.pcut, by="MgmtUnit")
  names(track)[2:ncol(track)] <- c("tot.inc", "even.age", "a.mat", "a.inc.burnt", "a.inc.mat.burnt",
     "a.inc.kill", "a.inc.mat.kill", "a.reg.fail.ex", "a.reg.fail.in", "area.salvaged", "area.unaff","v.salv",
     "a.salv","a.pcut","v.pcut")
  track[is.na(track)] <- 0
  
  #### merge, species level
  
  spp.track <- left_join(spp.ccut, spp.ccut.vol, by=c("MgmtUnit", "SppGrp")) %>% 
                           left_join(spp.pcut, by=c("MgmtUnit", "SppGrp")) %>%
                                       left_join(spp.ccut.vol, by=c("MgmtUnit", "SppGrp"))
  names(spp.track)[3:ncol(spp.track)] <- c("spp.ccut","spp.ccut.vol",
                                   "spp.pcut","spp.ccut.vol")
  ## Return the cell.id of the cut locations and the tracking info
  return(list(cc.cells=cc.cells, pc.cells=pc.cells, track.cut=track, spp.track=spp.track))  
  
}
