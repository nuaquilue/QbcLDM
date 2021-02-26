######################################################################################
###  disturbance.fire()
###
###  Description >  Simulates fire events in four zones with distinct fire regime.
###                 Called in landscape.dyn
###
###  Arguments >
###   land : appropriate selection fo the data frame of the state variables
###   file.num.fires : data frame of the number of fire per period by fire regime zone
###   file.fire.sizes : data frame of the fire sizes distribution by fire regime zone
###   fire.rate.increase : Rate of increase of number of fire through time (climate change)
###   baseline.fuel : number of years each time step represents
###   fuel.types.modif : number of years each time step represents
###   km2.pixel : number of km2 per pixel on the grid 
###
###  Details > For each fire regime zone, the number of fires and their sizes are derived
###   from the input distributions. Fire spreads from random ignition points until target
###   area is burnt. Re-burn is not allowed in the same time step.
###
###  Value >  A vector of the indexes of the burnt cells.
######################################################################################


wildfires <- function(land, fire.regime, fire.sizes, sep.zones, baseline.fuel, fuel.types.modif, pigni.opt, plot.fires,
                      wwind, wflam, rpb, is.fuel.modifier, is.clima.modifier, gcm.sep, clim.scn, km2.pixel, irun, t, MASK, th.small.fire){
  
  cat("Wildfires", "\n" )
  options(warn=-1)  # to avoid unecessary warnings about "frz" being factor of character when joining
                    # "track" type data.frames
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)

  ## MAP fire.ids and fire.step
  MAP <- MASK
  map <- data.frame(cell.id=land$cell.id, id=NA, step=NA)
  
  ## Generate random probability of ignition or load static pigni
  if(pigni.opt=="rand"){
    pigni <- data.frame(cell.id=land$cell.id, frz=land$frz)
    pigni$p <- runif(nrow(pigni),0.01,1)
  }
  if(pigni.opt=="static.exp")
    load("inputlyrs/rdata/pigni_static.exp.rdata")
  if(pigni.opt=="static.nexp")
    load("inputlyrs/rdata/pigni_static.nexp.rdata")
  if(pigni.opt=="light_static.exp")
    load("inputlyrs/rdata/pigni_light_static.exp.rdata")
  if(pigni.opt=="light_static.nexp")
    load("inputlyrs/rdata/pigni_light_static.nexp.rdata")
  
  
  ## Wind direction between neigbours
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  default.neigh <- data.frame(x=c(-1,1,ncol(MASK),-ncol(MASK),ncol(MASK)-1,-(ncol(MASK)+1),ncol(MASK)+1,-(ncol(MASK)-1)),
                              windir=c(270,90,180,0,225,315,135,45))
  # default.neigh <- data.frame(x=c(-1,1,ncol(MASK),-ncol(MASK)), windir=c(270,90,180,0))
  default.nneigh <- nrow(default.neigh)
  
  ## Create a fuel data frame with types (according to Spp and Age): 1 - low, 2 - medium, and 3 - high
  ## Then assign baseline flammability to each type
  fuels <- fuel.type(land, fuel.types.modif, NA, NA)
  
  ## To determine changes in landscape level fuel load per zone
  current.fuels <- group_by(fuels, frz) %>% summarize(x=mean(flam))
  modif.fuels <- current.fuels
  modif.fuels$x <-  1+(current.fuels$x-baseline.fuel$x)/baseline.fuel$x
   
  ## To modify target area to be burnt according to SEP values (climate change)
  if(!is.na(clim.scn))
    sep.zone.cc <- filter(sep.zone, GCM==gcm.sep, RCP==clim.scn)
  
  ## Initialize empty vector to track burned cells 
  burnt.cells <- visit.cells <-  numeric(0)
  
  ## Reset TrackFires data frame each run
  track.target <- data.frame(frz=NA, br=NA, brvar=NA, brfuel=NA, brclima=NA, target.area=NA)
  track.fire <- data.frame(frz=NA, fire.id=NA, wind=NA, target.size=NA,  burnt.size=NA)
  track.sprd <- data.frame(frz=NA, fire.id=NA, cell.id=NA, step=NA, flam=NA, wind=NA, sr=NA, pb=NA, burn=NA)
  
  ## Create a random permuation of the Fire Regime Zones to not burn FRZ always in the same order
  ## Start burn until target area per fire zone is not reached 
  izone <- "Z3"
  for(izone in paste0("Z", sample(1:6, 6, replace=FALSE))){
    
    ## Track modifications of target area at the zone level
    aux.track <- track.target[1,]
    aux.track$frz <- izone
      
    ## Determine target area to be  burnt per zone
    ## 1. Baseline area derived from MFRI (cells)
    baseline.area <- time.step*sum(land$frz==izone)/fire.regime$fri[fire.regime$frz==izone]
    aux.track$br <- baseline.area
    
    ## 2. Add random inter-period variability 
    zone.target.area <- rnorm(1, unlist(baseline.area), unlist(baseline.area)*0.1)
    aux.track$brvar <- zone.target.area
    
    ## 3. Modify target area based on changes in landscape-level fuel load
    if(is.fuel.modifier) 
      zone.target.area <- zone.target.area*modif.fuels$x[modif.fuels$frz==izone]
    aux.track$brfuel <- zone.target.area
    
    ## 4. Modify target area based on climate projections
    if(is.clima.modifier & !is.na(clim.scn)){
      if(t<=20) # 2020 to 2040
        aux.track$brclima <- aux.track$brvar*1
      if(t>20 & t<=50){ # 2040 to 2070
        zone.target.area <- zone.target.area*sep.zone.cc$rSEP1[sep.zone.cc$frz==izone]
        aux.track$brclima <- aux.track$brvar*sep.zone.cc$rSEP1[sep.zone.cc$frz==izone]
      }
      if(t>50){ # 2070 to 2100
        zone.target.area <- zone.target.area*sep.zone.cc$rSEP2[sep.zone.cc$frz==izone]
        aux.track$brclima <- aux.track$brvar*sep.zone.cc$rSEP2[sep.zone.cc$frz==izone]
      }
    }
    else
      aux.track$brclima <- aux.track$brvar
    
    ## Track target area modifications
    aux.track[2:5] <- round(aux.track[2:5]*km2.pixel)
    aux.track$target.area <- round(zone.target.area*km2.pixel)
    track.target <- rbind(track.target, aux.track)
    
    ## Round it to have entire cells
    zone.target.area <- round(zone.target.area)
    
    ## Record
    cat(paste("Zone:", izone, "- Target area (cells):", zone.target.area), "\n")
    
    
    ## For each fire zone, look for the associated fire size distribution; and 
    ## only keep potential ignitions cells in the fire zone
    fs.dist <- filter(fire.sizes, frz==izone) %>% select(-frz)
    pigni.zone <- filter(pigni, frz==izone)
    
    
    ## Initialize traking variables every zone
    fire.id <- 0
    fire.ncell.burnt <- zone.ncell.burnt <- 0
    fire.target.area <- 0 ## to make the first "if" condition true
    
    while(zone.ncell.burnt < zone.target.area){   ## condition in pixels
    
      ## ID for each fire event
      fire.id <- fire.id+1
      
      ## If previous fire has extinguished to early (burnt.area/target.area<50%), then "reuse" the
      ## target area for a new fire. This tends to occur when potentially big fires start in low-fuel lands
      ## Or in a agro-forest matrix. I may need to add a condition related to the minimum target area
      ## I allow to be reused (something big e.g < 200)
      if(fire.id==1 | (fire.id>1 & fire.ncell.burnt/fire.target.area>=0.5) |
         (fire.id>1 & fire.ncell.burnt/fire.target.area<0.5 & fire.target.area<=50) ){
        ## Determine potential area of the fires (i.e. the target area) in km2
        ## Fire size is drawn from an discrete distribution defined in classes of 50 km2 
        fire.class <- sample(1:nrow(fs.dist), 1, replace=F, p=fs.dist$p) 
        fire.target.size <- min(rdunif(1, fs.dist$lower[fire.class], fs.dist$upper[fire.class]), 
                                fire.regime$max[fire.regime$zone==izone])
        ## Transform fire size (in km2) in fire target area (in pixels), some fires will lose area
        ## while others will gain... on average the difference should be 0
        fire.target.area <- pmax(1, round(fire.target.size/km2.pixel))
      }
      ## Do not target more than what remains to be burnt at the zone level. fire.target.area in pixels
      fire.target.area <- min(fire.target.area, zone.target.area-zone.ncell.burnt)
      fire.target.area
      
      ## For fire front selection
      mn.ncell.ff <- ifelse(fire.target.area<=50, 8, 16) 
      mx.ncell.ff <- ifelse(fire.target.area<=50, 12, 20) 
      
      ## Assign the main wind direction 
      ## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
      # S 10%, SW 30%, W 60%  
      fire.wind <- sample(c(180,225,270), 1, replace=T, p=c(10,30,60))
      
      ## Create a fuel data frame with types (according to Spp and Age): 1 - low, 2 - medium, and 3 - high
      ## Then assign baseline flammability to each type according to target fire size:
      ## Max flammability for all fires with size > th.small.fire
      fuels <- fuel.type(land, fuel.types.modif, fire.target.area, th.small.fire)
      
      ## Select an ignition point according to probability of ignition and initialize tracking variables
      fire.front <- sample(pigni.zone$cell.id, 1, replace=F, pigni.zone$p)
      burnt.cells <- c(burnt.cells, fire.front)
      visit.cells <- c(visit.cells, fire.front)
      zone.ncell.burnt <- zone.ncell.burnt + 1
      fire.ncell.burnt <- 1  
      step <- 1
      track.sprd <- rbind(track.sprd, data.frame(frz=izone, fire.id=fire.id, cell.id=fire.front, step=step,
                          flam=0, wind=0, sr=1, pb=1, burn=TRUE))
      map$id[map$cell.id==fire.front] <- 1 #fire.id
      map$step[map$cell.id==fire.front] <- step
      
      ## Start speading from active cells (i.e. the fire front)
      while(fire.ncell.burnt < fire.target.area){
        
        step <- step+1
        
        ## Build a data frame with the theoretical 4 neighbours of cells in fire.front, 
        ## Add the wind direction and the distance.
        ## Filter neighbours in the study area and that have not been visited yet
        ## Then, look for their default flammability
        neigh.id <- data.frame(cell.id=rep(fire.front, each=default.nneigh) + rep(default.neigh$x, length(fire.front)),
                               source.id=rep(fire.front, each=default.nneigh),
                               windir=rep(default.neigh$windir, length(fire.front)) ) %>%
                    filter(cell.id %in% land$cell.id) %>% filter(cell.id %notin% visit.cells) %>%
                    left_join(fuels, by="cell.id") %>% 
                    mutate(dif.wind=abs(windir-fire.wind),
                           wind=ifelse(dif.wind==0, 0, ifelse(dif.wind %in% c(45,315), 0.25, 
                                ifelse(dif.wind %in% c(90,270), 0.5, ifelse(dif.wind %in% c(135,225), 0.75, 1)))),
                           sr=wwind*wind+wflam*flam, pb=1+rpb*log(sr))  
        neigh.id$pb <- neigh.id$pb + runif(nrow(neigh.id), -0.2, 0.2)  # some randomness to get less geometric fire shapes 
        
        ## Get spread rate andcompute probability of burn and actual burn state (T or F):
        sprd.rate <- group_by(neigh.id, cell.id) %>% summarize(flam=max(flam), wind=max(wind), sr=max(sr), pb=max(pb)) 
        sprd.rate$burn <- sprd.rate$pb >= runif(nrow(sprd.rate), pb.lower.th, pb.upper.th)
        sprd.rate
        if(nrow(sprd.rate)>0)
          track.sprd <- rbind(track.sprd, data.frame(frz=izone, fire.id=fire.id, cell.id=sprd.rate$cell.id,
                              step=step, flam=sprd.rate$flam, wind=sprd.rate$wind,
                              sr=sprd.rate$sr, pb=sprd.rate$pb, burn=sprd.rate$burn))
        
        
        ##Avoid fire overshooting at last iteration: Only burn cells with higher pb
        temp.burnt <- sprd.rate[sprd.rate$burn, c("cell.id", "pb")]
        if(fire.ncell.burnt+nrow(temp.burnt)>fire.target.area){
          max.burnt <- fire.target.area - fire.ncell.burnt
          temp.burnt <- temp.burnt[order(temp.burnt$pb, decreasing = TRUE),]
          def.burnt <- temp.burnt$cell.id[1:max.burnt]
          sprd.rate$burn <- (sprd.rate$cell.id %in% def.burnt)
        }
        
        ## Tring to really get exact fire.id!!
        map$id[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] <- 1 #fire.id
        map$step[map$cell.id %in% sprd.rate$cell.id[sprd.rate$burn]] <- step
        
        ## Mark the burnt and visit cells
        burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burn])
        visit.cells <- c(visit.cells, sprd.rate$cell.id)
        
        ## If at least there's a burn cell, continue, otherwise, stop
        if(!any(sprd.rate$burn))
          break
        
        ## Select the new fire front
        nburn <- sum(sprd.rate$burn)        
        if(nburn<=3)
          fire.front <- sprd.rate$cell.id[sprd.rate$burn]
        else if(nburn<=mn.ncell.ff){
          # fire.front <- sprd.rate$cell.id[sprd.rate$burn]
          fire.front <- sort(sample(sprd.rate$cell.id[sprd.rate$burn], rdunif(1, 3, nburn),
                                    replace=F, prob=sprd.rate$pb[sprd.rate$burn]))
        }
        else{
          z <- rdunif(1,mx.ncell.ff-5,mx.ncell.ff)
          ncell.ff <- min(nburn*runif(1,0.5,0.7), z, na.rm=T)
          fire.front <- sort(sample(sprd.rate$cell.id[sprd.rate$burn], round(ncell.ff), 
                                    replace=F, prob=sprd.rate$pb[sprd.rate$burn]))  
        }
        
        ## Increase area burnt (total and per fire)
        zone.ncell.burnt <- zone.ncell.burnt + sum(sprd.rate$burn)
        fire.ncell.burnt <- fire.ncell.burnt + sum(sprd.rate$burn)
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
      } # while 'fire.target.area'
      
      ## Write info about this fire
      track.fire <- rbind(track.fire, data.frame(frz=izone, fire.id, wind=fire.wind, 
                                                 target.size=fire.target.area*km2.pixel,
                                                 burnt.size=fire.ncell.burnt*km2.pixel))  
      #  cat(paste("Fire:", fire.id, "- TargetSize:", fire.target.area, "- BurntPxls:", fire.ncell.burnt), "\n")
      
    }  # while 'zone.target.area'
    
    # if(print.maps)
    #   save(map, file=paste0(out.path, "/Maps_r", irun, "t", t, "_frz", izones, ".rdata"))
      
  } #for 'zone'
  
    
  ## TRACKING
  track.target <- track.target[-1,]
  track.fire <- track.fire[-1,] 
  # Size (in km2) of each zone
  zone.size <- group_by(land, frz) %>% summarize(area=length(frz)*km2.pixel) 
  # Mean flammability of what has been burnt
  burnt.fuels <- fuel.type(filter(land, cell.id %in% burnt.cells), fuel.types.modif, NA, NA) %>% 
                 group_by(frz) %>%  summarize(indx.combust.burnt=mean(flam))
  # Burnt per zone
  zone.burnt <- group_by(track.fire, frz) %>% summarize(nfires=length(fire.id), burnt.area=sum(burnt.size)) 
  # Fire regime (burnt, MFRI, indx.combust)  
  track.regime <- select(track.target, frz, target.area) %>% left_join(zone.burnt, by="frz") %>% 
                 left_join(zone.size, by="frz") %>% left_join(current.fuels, by="frz") %>%
                 mutate(fire.cycle=round(time.step*area/burnt.area), indx.combust=x) %>% select(-area, -x) %>% 
                 left_join(burnt.fuels, by="frz") 

  if(plot.fires){
    save(map, file=paste0(out.path, "/Maps_r", irun, "t", t, ".rdata"))
    MAP[!is.na(MASK[])] <- map$id
    writeRaster(MAP, paste0(out.path, "/FireIds_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
    # ## fire.step
    # MAP[!is.na(MASK[])] <- map$step
    # writeRaster(MAP, paste0(out.path, "/FireStep_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
  }
  
  ## Return the index of burnt cells and the tracking data.frames
  ## For some reason I still don't know, a few (little) times, there are duplicates in burnt.cells, 
  ## what causes errors in buffer.mig (for example). 
  ## I will need to find the mistake and solve it, but by now I simply retrun unique(burnt.cells).
  ## However, in such cases length(burnt.cells)*km2.pixel < aburnt at the zone level
  return(list(burnt.cells=unique(burnt.cells), track.target=track.target, track.regime=track.regime, 
              track.fire=track.fire, track.sprd=track.sprd[-1,]))  
  
}