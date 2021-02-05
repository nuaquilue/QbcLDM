######################################################################################
###  harvest.area()
######################################################################################

harvest.area <- function(land, cc.step, diff.prematurite, hor.plan, TS.CC.area, TS.PC.area, salvage.rate.FMU,
                         salvage.rate.event, km2.pixel, t, p.failure, age.seed, courbes){  

   
  #harv.level <- ref.harv.level
  cat("Select clearcut and partial cut cells - area based", "\n" )
  
  land2 <- land[!is.na(land$mgmt.unit) & !land$spp=="NonFor",]
  land2$vol <- (volume.vec(land2,courbes)*km2.pixel*100)
 # land2 <- land2[order(-land2$vol),]

  units <- as.character(sort(unique(land2$mgmt.unit[!is.na(land2$mgmt.unit)])))

  
  s.inc <- filter(land2, !is.na(mgmt.unit) & is.na(exclus)) %>% group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  s.inc.mat <- filter(land2, !is.na(mgmt.unit) & is.na(exclus) & age>age.matu) %>% group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))

  
  ## For those locations that can be harvested (included), differentiate those that have been burnt or killed
  ## by an outbreak, and then count the young (cannot be salvaged) vs the mature (can be salvaged)
  s.inc.burnt <- filter(land2, !is.na(mgmt.unit) & is.na(exclus) & tsfire==0) %>% 
    group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  s.inc.mat.burnt <- filter(land2, !is.na(mgmt.unit) & is.na(exclus) & tsfire==0 & age>age.matu) %>% 
    group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  s.inc.kill <- filter(land2, !is.na(mgmt.unit) & is.na(exclus) & tssbw%in%c(0,5)) %>% 
    group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  s.inc.mat.kill <- filter(land2, !is.na(mgmt.unit) & is.na(exclus) & tssbw%in%c(0,5) & age>age.matu) %>% 
    group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  
  ## Also, look for zones at defforestation risk, both included and excluded
  reg.fail.ex <- filter(land2, !is.na(mgmt.unit) & spp %in% c("EPN", "SAB", "OTH.RES.N"), 
                        tsfire==0, age<=age.seed, temp < 1.5, runif(length(land2$temp))<p.failure, !is.na(exclus)) %>%
    group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))
  reg.fail.inc <- filter(land2, !is.na(mgmt.unit) & spp %in% c("EPN", "SAB", "OTH.RES.N"), 
                         tsfire==0, age<=age.seed, temp < 1.5, runif(length(land2$temp))<p.failure, is.na(exclus)) %>%
                          group_by(mgmt.unit) %>% summarise(x=length(mgmt.unit))

  
  #land2 <- mutate(land2, rndm=runif(nrow(land2)))
  #even <- land2$spp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB") & is.na(land2$exclus) & land2$rndm<=0.95
  #sum(even) 
  #even[land2$spp %in% c("BOJ", "ERS", "OthDT")& is.na(land2$exclus) & land2$rndm>0.95] <- 1
  land2$even[land2$tsfire==0] <- 1
  land.coniferes <- land2[land2$even==1,] 
  land.feuillu.tol <- land2[land2$even==0,] 
  
  land.ea <- land.coniferes
  s.ea <- group_by(land.ea, mgmt.unit) %>% summarise(x=length(mgmt.unit)) 
  ### possibilité
  poss.init <-  TS.CC.area # read.table("InitialVolume.txt", header=T)  

  ## Subset the mature even-aged cells from those that are harvestable
  land.rec <- filter(land.ea, age>=age.matu)
  
  # initialisation des variables
  cc.cells.salv.tot <- cc.cells.unaff.tot <- cc.cells <- numeric(0)
  
  unit=2571 #units[18] # for testing
  for(unit in units){

    harv.level.u <- poss.init[poss.init$mgmt.unit == unit,2]
    land.ea.u <- land.ea[land.ea$mgmt.unit==unit,]
    s.ea.u <- length(land.ea.u$cell.id)  
    
    # Subset of harvestable (mature even-aged) cells
    land.ea.mat.u <- land.ea.u[land.ea.u$age >= land.ea.u$age.matu,]

    subland.salv.mature.burn <- land.ea.u[(land.ea.u$age >= (land.ea.u$age.matu-diff.prematurite)) & 
                                             land.ea.u$tsfire ==0, ]
    # table(land.ea.u$tsfire)
    liste.recuperables.FMU <- sample(subland.salv.mature.burn$cell.id, round(salvage.rate.FMU*nrow(subland.salv.mature.burn)))

    subland.salv.mature.burn2 <- subland.salv.mature.burn[subland.salv.mature.burn$cell.id %in% liste.recuperables.FMU,]

    

    
    if(length(subland.salv.mature.burn2$cell.id) > round(salvage.rate.event* harv.level.u)) {
          liste.2 <- sample(subland.salv.mature.burn2$cell.id,round((salvage.rate.event* harv.level.u)), replace=FALSE)
           subland.salv.mature.burn3 <- subland.salv.mature.burn2[subland.salv.mature.burn2$cell.id %in% liste.2,]
    } else {
      subland.salv.mature.burn3 <- subland.salv.mature.burn2
    }
        
    subland.salv.mature.sbw <- land.ea.u[(land.ea.u$age >= (land.ea.u$age.matu-diff.prematurite)) & 
                                           land.ea.u$tssbw %in% c(0,5), ]  
    subland.salv.mature <- rbind(subland.salv.mature.burn3,subland.salv.mature.sbw)
    
    # sélection de cellules récupérables en tenant compte
    # des contraintes a priori (maximum salvage rate, etc.)
    cell.salv.available <- subland.salv.mature$cell.id #sample(subland.salv.mature$cell.id, round(salvage.rate.event*nrow(subland.salv.mature)), replace=FALSE)
    #table(subland.salv.mature$spp)        
     
    #############################################
    # Randomly select cells among the even-aged mature cells present in non-protected areas
    # Prioritize clear cuts in disturbed areas (salvage logging)

    x <- length(cell.salv.available )# x = disponible
    cc.cells.salv <-  numeric(0)
    xx <-0 # récolté
    
    while (x > 0 & length(cell.salv.available)>0 & xx < harv.level.u)    {
      paquet <- ifelse((x <= harv.level.u) ,x,harv.level.u ) # pour accélérer le calcul, paquets de 5 cellules
      cc.cells.salv.x <- sample(cell.salv.available,paquet)#cell.salv.available[1:paquet]  #
      cc.cells.salv <- c(cc.cells.salv,cc.cells.salv.x)
      cell.salv.available <- cell.salv.available[-which(cell.salv.available%in%cc.cells.salv.x)]
      x <- sum(subland.salv.mature[subland.salv.mature$cell.id %in%cell.salv.available,]$vol )
      xx <- length(cc.cells.salv )
    }
    
    # When salvaged cells were not enough to satisfy sustained yield level, then harvest some mature 
    # forests unaffected by disturbances (cc.cells.unaff).

    subland.non.pertu <- land.ea.mat.u[land.ea.mat.u$tsfire!=0, ]

    x <- length(subland.non.pertu$cell.id ) # x = disponible
    cc.cells.unaff <- numeric(0)
    #  arrête la récolte lorsqu'on est rendu à < 40000m3 du but, il y aura parfois des dépassements
    while(x > 0 & length(subland.non.pertu$cell.id)>0 & xx < (harv.level.u)) {
       paquet <- ifelse(harv.level.u-xx >= 0 ,min(x,harv.level.u-xx) ,0 )
       cc.cells.unaff.x <- sample( subland.non.pertu$cell.id, paquet)
       cc.cells.unaff   <- c(cc.cells.unaff,cc.cells.unaff.x)
       subland.non.pertu <- subland.non.pertu[-which(subland.non.pertu$cell.id%in%cc.cells.unaff.x),]
       xx <- length(c(cc.cells.unaff,cc.cells.salv ))
     }
       

    # Combine all types of harvested cells with those already harvested in other FMUs during the same period
    cc.cells <- c(cc.cells, cc.cells.salv, cc.cells.unaff)
    cc.cells.salv.tot <- c(cc.cells.salv.tot, cc.cells.salv)
    cc.cells.unaff.tot <- c(cc.cells.unaff.tot, cc.cells.unaff)
    
  }
  length(cc.cells)
 
  ############################
  ################## partial cuts
  
  land.uea <- land.feuillu.tol
  s.uea <- group_by(land.uea, mgmt.unit) %>% summarise(x=length(mgmt.unit)) 
  
  # volume par cellule. Moitié du volume accessible
  land.uea$vol <- land.uea$vol/2
  
  ## The maturity age for partial cuts is half the maturity age for a clear cut
  land.uea$age.matu.pc <- round(land.uea$age.matu,-1)/2
  
  ## Subset of harvestable (i.e. mature uneven-aged, ot recently partial cut) cells
  land.rec.pc <- filter(land.uea, tspcut >=age.matu.pc )
  
  ## Get the number of cells to be managed under a partial-cut regime
  s.uea <- group_by(land.uea, mgmt.unit) %>% summarise(x=length(mgmt.unit))    
  s.mat.pc <- group_by(land.rec.pc, mgmt.unit) %>% summarise(x=length(mgmt.unit))    
  
  harv.level.pc <- TS.PC.area # read.table("InitialVolumePC.txt", header=T)
  pc.cells <- numeric()
  for(unit in unique(land.uea$mgmt.unit)){  #unit=9351
    poss.cp.ua <- harv.level.pc$x[harv.level.pc$mgmt.unit==unit]
    poss.cp.ua <- ifelse(length(poss.cp.ua) == 0,0,poss.cp.ua)
    cell.dispo.ua <- land.rec.pc[land.rec.pc$mgmt.unit==unit, ]
    x <- length(cell.dispo.ua$cell.id ) #disponible
    pc.cells.ua <- numeric(0)
    xx <- 0
    #  arrête la récolte lorsqu'on est rendu à < 20000m3 du but, il y aura parfois des dépassements
    while(x > 0 & length(cell.dispo.ua$cell.id)>0 & xx < (poss.cp.ua)) {
      paquet <- ifelse(poss.cp.ua-xx >= 0 ,min(x,poss.cp.ua-xx) ,0 )
      pc.cells.ua.x <- sample( cell.dispo.ua$cell.id, paquet)
      pc.cells.ua   <- c(pc.cells.ua,pc.cells.ua.x)
      cell.dispo.ua <- cell.dispo.ua[-which(cell.dispo.ua$cell.id%in%pc.cells.ua.x),]
      #x <- sum(cell.dispo.ua$vol,na.rm=T ) # volume restant
      xx <- length(c(pc.cells.ua )) # aire récoltée
    }
    
      ## Add the cells partially cut in this UA:
      pc.cells <- c(pc.cells, pc.cells.ua)   
  }    
  
  length(pc.cells)
  ################################################# TRACKING #################################################
  ## Area salvaged and non-salvaged, clearcut
  ## areas are in cells, volumes are in m3
  a.salv <- filter(land2, cell.id %in% cc.cells.salv.tot) %>% group_by(mgmt.unit) %>% summarize(x=length(mgmt.unit))
  a.unaff <- filter(land2, cell.id %in% cc.cells.unaff.tot) %>% group_by(mgmt.unit) %>% summarize(x=length(mgmt.unit))
  
  ### Volume logged, clearcut
  v.salv <- filter(land.ea, cell.id %in% cc.cells.salv.tot) %>% group_by(mgmt.unit) %>% summarize(x=sum(vol))
  v.unaff <- filter(land.ea, cell.id %in% cc.cells.unaff.tot) %>% group_by(mgmt.unit) %>% summarize(x=sum(vol))
  
  ## Area clearcut per species per management unit (clearcut)
  spp.ccut <- filter(land2, cell.id %in% c(cc.cells.salv.tot, cc.cells.unaff.tot)) %>%
              group_by(mgmt.unit, spp) %>% summarize(x=length(mgmt.unit)) 
  ## Volume clearcut per species per management unit (clearcut)
  spp.ccut.vol <- filter(land2, cell.id %in% c(cc.cells.salv.tot, cc.cells.unaff.tot)) %>%
                  group_by(mgmt.unit, spp) %>% summarize(x=sum(vol)) 
  
  ## Area partial cut per management unit
  a.pcut  <- filter(land2, cell.id %in% pc.cells) %>% group_by(mgmt.unit) %>% summarize(x=length(mgmt.unit)) 
  ## Volume partial cut per management unit
  v.pcut  <- filter(land2, cell.id %in% pc.cells) %>% group_by(mgmt.unit) %>%  summarize(x=sum(vol)) 

  ## Area partialcut per species per management unit (clearcut)
  spp.pcut <- filter(land2, cell.id %in% pc.cells) %>% group_by(mgmt.unit, spp) %>% summarize(x=length(mgmt.unit)) 
  ## Volume partialcut per species per management unit (clearcut)
  spp.pcut.vol <- filter(land2, cell.id %in% pc.cells) %>% group_by(mgmt.unit, spp) %>% summarize(x=sum(vol))
     
  ## Merge all the info, FMU level
  ## @@@@@@@@ MATHIEU, here the names of the variables area "area" but they are in fact NUMBER OF CELLS!
  ## I don't change anything, but it is confusing ;-)
  track <- left_join(s.inc, s.ea, by="mgmt.unit") %>% left_join(s.mat.pc, by="mgmt.unit") %>% 
           left_join(s.inc.burnt, by="mgmt.unit") %>% left_join(s.inc.mat.burnt, by="mgmt.unit") %>%
           left_join(s.inc.kill, by="mgmt.unit") %>% left_join(s.inc.mat.kill, by="mgmt.unit") %>%
           left_join(reg.fail.ex, by="mgmt.unit") %>% left_join(reg.fail.inc, by="mgmt.unit") %>%
           left_join(a.salv, by="mgmt.unit") %>% left_join(a.unaff, by="mgmt.unit") %>%
           left_join(v.salv, by="mgmt.unit") %>% left_join(v.unaff, by="mgmt.unit") %>%  
           left_join(a.pcut, by="mgmt.unit") %>% left_join(v.pcut, by="mgmt.unit")
  names(track)[2:ncol(track)] <- c("tot.inc", "even.age", "a.mat", "a.inc.burnt", "a.inc.mat.burnt",
     "a.inc.kill", "a.inc.mat.kill", "a.reg.fail.ex", "a.reg.fail.in", "area.salvaged", "area.unaff","v.salv",
     "v.unaff","a.pcut","v.pcut")
  track[is.na(track)] <- 0
  
  #### merge, species level
  spp.track <- left_join(spp.ccut, spp.ccut.vol, by=c("mgmt.unit", "spp")) %>% 
               left_join(spp.pcut, by=c("mgmt.unit", "spp")) %>% left_join(spp.pcut.vol, by=c("mgmt.unit", "spp"))
  names(spp.track)[3:ncol(spp.track)] <- c("spp.ccut","spp.ccut.vol", "spp.pcut","spp.pcut.vol")
  spp.track[is.na(spp.track)] <- 0
  ## Return the cell.id of the cut locations and the tracking info
  return(list(cc.cells=cc.cells, pc.cells=pc.cells, track.cut=track, spp.track=spp.track))  
  
}
