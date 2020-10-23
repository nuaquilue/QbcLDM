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
###   area is burnt. Re-burning is not allowed in the same time step.
###
###  Value >  A vector of the indexes of the burnt cells.
######################################################################################


wildfires <- function(land, file.fire.regime, file.fire.sizes, baseline.fuel,
                      fuel.types.modif, pigni.opt, km2.pixel, t,increase.fire){
  
  cat("Wildfires", "\n" )
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Read fire regime and fire size distribution per HFRZ
  fire.regime <- read.table(file.fire.regime, header = T)
  fire.sizes <- read.table(file.fire.sizes, header = T)
  
  # job feu.stable = 1
  #if (feu.stable) {fire.regime[,4:6] <- 0  }
  
    
  ## Generate random probability of ignition or load static pigni
  if(pigni.opt=="rand"){
    pigni <- data.frame(cell.id=land$cell.id, frz=land$FRZone)
    pigni$p <- runif(nrow(pigni),0.01,1)
  }
  if(pigni.opt=="static.exp")
    load("inputlyrs/rdata/pigni_static.exp.rdata")
  if(pigni.opt=="static.nexp")
    load("inputlyrs/rdata/pigni_static.nexp.rdata")
  
  
  ## Wind direction between neigbours
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  # default.neigh <- data.frame(x=c(-1,1,805,-805,804,-806,806,-804),
  #                             windir=c(270,90,180,0,225,315,135,45))
  default.neigh <- data.frame(x=c(-1,1,805,-805), windir=c(270,90,180,0))
  default.nneigh <- nrow(default.neigh)
  
  ## Create a fuel data frame with types (according to Spp and Age): 1 - low, 2 - medium, and 3 - high
  ## Then assign baseline flammability to each type
  fuels <- fuel.type(land, fuel.types.modif, NA)
  current.fuels <- group_by(fuels, zone) %>% summarize(x=mean(baseline))
  modif.fuels <- current.fuels
  modif.fuels$x <-  1+(current.fuels$x-baseline.fuel$x)/baseline.fuel$x
  
  ## Create a random permuation of the Fire Regime Zones to not burning FRZ always in the same order
  fr.zones <- sample(LETTERS[1:8], 8, replace=FALSE)   
  
  ## Initialize empty vector to track burned cells 
  burnt.cells <- visit.cells <-  numeric(0)
  
  ## Reset TrackFires data frame each run
  track.fire <- data.frame(zone=NA, fire.id=NA, wind=NA, atarget=NA,  aburnt=NA)  #atarget.modif=NA,
  # track.sprd <- data.frame(zone=NA, fire.id=NA, cell.id=NA, step=NA, 
  #                          flam=NA, wind=NA, sr=NA, pb=NA, burning=NA)
  
  
  ## Start burning until annual target area per fire zone is not reached 
  izone="E"
  for(izone in fr.zones){
    
    ## Determine annual area burnt per zone, but considering fire rate increase with time 
    baseline.area <- time.step*sum(land$FRZone==izone)/fire.regime$fri[fire.regime$zone==izone]
    if(t>0 & increase.fire){
      for(j in seq(time.step, t, time.step)){
        if(j<=25)
          rate <- fire.regime$rate40[fire.regime$zone==izone]
        else if(j<=55)
          rate <- fire.regime$rate70[fire.regime$zone==izone]
        else
          rate <- fire.regime$rate100[fire.regime$zone==izone]
        baseline.area <-  baseline.area*(1+rate*time.step)  
      }
    }
    target.area <- round(rnorm(1, unlist(baseline.area), unlist(baseline.area)*0.1)) #in km2
#   target.size <- round(target.area/km2.pixel)
    
    ## Record
    cat(paste("Zone:", izone, "- target.area (pxls):", target.area), "\n")
    
    ## For each fire zone, look for the associated fire size distribution; and 
    ## only keep potential ignitions cells in the fire zone
    fs.dist <- filter(fire.sizes, frz==izone) %>% select(-frz)
    pigni.zone <- filter(pigni, frz==izone)
    
    ## Initialize traking variables every zone
    fire.id <- track.burnt <- 0
    pxl.burnt <- fire.size.target <- 0 ## to make the first "if" condition true
    while(track.burnt < target.area){   ## condition in pixels
    
      ## ID for each fire event
      fire.id <- fire.id+1
      
      ## If previous fire has extinguished to early (burnt.area/target.area<50%), then "reuse" the
      ## target area for a new fire. This tends to occur when potentially big fires start in low-fuel lands
      ## Or in a agro-forest matrix. I may need to add a condition related to the minimum target area
      ## I allow to be reused (something big e.g < 200)
      if(fire.id==1 | (fire.id>1 & pxl.burnt/fire.size.target>=0.5) |
         (fire.id>1 & pxl.burnt/fire.size.target<0.5 & fire.size.target<=50) ){
        ## Determine potential area of the fires (i.e. the target area) in km2
        ## Fire size is drawn from an discrete distribution defined in classes of 50 km2 
        fire.class <- sample(1:nrow(fs.dist), 1, replace=F, p=fs.dist$p) 
        fire.area <- min(rdunif(1, fs.dist$lower[fire.class], fs.dist$upper[fire.class]), 
                         fire.regime$max[fire.regime$zone==izone])
        ## Transform fire area (in km2) in fire size target (in pixels), some fires will lose size
        ## while others will gain... on average the difference should be 0
        fire.size.target <- pmax(1, round(fire.area/km2.pixel))
        ## Change fire size to account for changes in landscape-level flammability
        ## VERIFY THE IMPACT OF LANDSCAPE FLAMMABILITY ON FIRE SIZE. I dont' like to much
        # fire.area.modif <- fire.area*modif.fuels$x[modif.fuels$zone==izone]
        # fire.size.target.modif <- pmax(1, round(fire.area.modif/km2.pixel))
      }
      ## Do not target more than what remains to be burnt at the zone level
      fire.size.target <- min(fire.size.target, target.area-track.burnt)
      fire.size.target
      
      ## Assign the main wind direction 
      ## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
      # S 10%, SW 30%, W 60%  
      fire.wind <- sample(c(180,225,270), 1, replace=T, p=c(10,30,60))
      
      ## Create a fuel data frame with types (according to Spp and Age): 1 - low, 2 - medium, and 3 - high
      ## Then assign baseline flammability to each type according to target fire size
      fuels <- fuel.type(land, fuel.types.modif, fire.size.target)
      
      ## Select an ignition point according to probability of ignition and initialize tracking variables
      fire.front <- sample(pigni.zone$cell.id, 1, replace=F, pigni.zone$p)
      burnt.cells <- c(burnt.cells, fire.front)
      visit.cells <- c(visit.cells, fire.front)
      track.burnt <- track.burnt + 1
      pxl.burnt <- 1  
      # track.sprd <- rbind(track.sprd, 
      #                     data.frame(zone=izone, fire.id=fire.id, cell.id=fire.front,
      #                                flam=0, wind=0, sr=1, pb=1, burning=TRUE))
      
      
      ## Start speading from active cells (i.e. the fire front)
      while(pxl.burnt < fire.size.target){
        
        ## Build a data frame with the theoretical 4 neighbours of cells in fire.front, 
        ## Add the wind direction and the distance.
        ## Filter neighbours in the study area and that have not been visited yet
        ## Then, look for their default flammability
        neigh.id <- data.frame(cell.id=rep(fire.front, each=default.nneigh) + rep(default.neigh$x, length(fire.front)),
                               source.id=rep(fire.front, each=default.nneigh),
                               windir=rep(default.neigh$windir, length(fire.front)) ) %>%
                    filter(cell.id %in% land$cell.id) %>% filter(cell.id %notin% visit.cells) %>%
                    left_join(fuels, by="cell.id") %>% 
                    mutate(flam=wflam*baseline, 
                           wind = wwind * (ifelse(abs(windir-fire.wind)>180, 
                                        360-abs(windir-fire.wind), abs(windir-fire.wind)))/180,
                           sr=wind+flam, pb=1+rpb*log(sr))
        neigh.id  
        
        ## Get spread rate andcompute probability of burning and actual burning state (T or F):
        sprd.rate <- group_by(neigh.id, cell.id) %>% 
                     # summarize(flam=max(flam)/wflam, wind=max(wind)/wwind, sr=max(sr), pb=max(pb)) 
                     summarize(sr=max(sr), pb=max(pb)) 
        sprd.rate$burning <-runif(nrow(sprd.rate), 0, pb.upper.th) <= sprd.rate$pb & sprd.rate$pb >= pb.lower.th
            # if(nrow(sprd.rate)>0)
            #   track.sprd <- rbind(track.sprd, data.frame(zone=izone, fire.id=fire.id, cell.id=sprd.rate$cell.id,
            #                                   flam=sprd.rate$flam, wind=sprd.rate$wind,
            #                                   sr=sprd.rate$sr, pb=sprd.rate$pb, burning=sprd.rate$burning))
        sprd.rate
        
        ##Avoid fire overshooting at last iteration: Only burn cells with higher pb
        temp.burnt <- sprd.rate[sprd.rate$burning, c("cell.id", "pb")]
        if(pxl.burnt+nrow(temp.burnt)>fire.size.target){
          max.burnt <- fire.size.target - pxl.burnt
          temp.burnt <- temp.burnt[order(temp.burnt$pb, decreasing = TRUE),]
          def.burnt <- temp.burnt$cell.id[1:max.burnt]
          sprd.rate$burning <- (sprd.rate$cell.id %in% def.burnt)
        }
        
        ## If at least there's a burning cell, continue, otherwise, stop
        if(!any(sprd.rate$burning))
          break
        
        ## Mark the cells burnt and visit, and select the new fire front
        ## 'mad' -> median absolute deviation
        burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burning])
        visit.cells <- c(visit.cells, sprd.rate$cell.id)
        exclude.th <- min(max(sprd.rate$sr)-0.005, 
                           rnorm(1,median(sprd.rate$sr[sprd.rate$burning])-mad(sprd.rate$sr[sprd.rate$burning])*1.5,
                                 mad(sprd.rate$sr[sprd.rate$burning])/2))
        fire.front <- sprd.rate$cell.id[sprd.rate$burning & sprd.rate$sr>=exclude.th]
        
        ## Increase area burnt (total and per fire)
        track.burnt <- track.burnt + sum(sprd.rate$burning)
        pxl.burnt <- pxl.burnt + sum(sprd.rate$burning)
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
      } # while 'fire burns'
      
      ## Write info about this fire
      track.fire <- rbind(track.fire, data.frame(zone=izone, fire.id, wind=fire.wind, atarget=fire.size.target*km2.pixel,
                                                 aburnt=pxl.burnt*km2.pixel))  #atarget.modif=fire.size.target.modif*km2.pixel, 
      #  cat(paste("Fire:", fire.id, "- TargetSize:", fire.size.target, "- BurntPxls:", pxl.burnt), "\n")
      
    }  # while 'zone burns'
  } #for 'zone'
  
    
  ## TRACKING
  track.fire <- track.fire[-1,]; track.fire
  track.regime <- group_by(track.fire, zone) %>% summarize(nfires=length(atarget), atarget=sum(atarget), aburnt=sum(aburnt)) %>%
                  left_join(group_by(land, FRZone) %>% summarize(atot=length(FRZone)*km2.pixel), by=c("zone"="FRZone")) %>%
                  mutate(fire.cycle=round(time.step*atot/aburnt)) %>%
                  left_join(current.fuels, by="zone") %>% mutate(indx.combust=x) %>% select(-atot, -x)
  # fuels
  fuels <- fuel.type(land, fuel.types.modif, NA)
  fuels.burnt <- fuel.type(filter(land, cell.id %in% burnt.cells), fuel.types.modif)
  nb <-  length(unique(fuel.types.modif$baseline))
  nzones <- length(levels(fuels$zone))
  track.fuels <- data.frame(table(fuels$zone, fuels$baseline) / matrix(table(fuels$zone), nrow=nzones, ncol=nb),
                            table(fuels.burnt$zone, fuels.burnt$baseline) / matrix(table(fuels.burnt$zone), nrow=nzones, ncol=nb)) %>%
                select(-Var1.1, -Var2.1)
  names(track.fuels) <- c("zone", "flam", "pctg.zone", "pctg.burnt")
  track.fuels$pctg.burnt[is.na(track.fuels$pctg.burnt)] <- 0
  
  ## Return the index of burnt cells and the tracking data.frames
  ## For some reason I still don't know, a few (little) times, there are duplicates in burnt.cells, 
  ## what causes errors in buffer.mig (for example). 
  ## I will need to find the mistake and solve it, but by now I simply retrun unique(burnt.cells).
  ## However, in such cases length(burnt.cells)*km2.pixel < aburnt at the zone level
  return(list(burnt.cells=unique(burnt.cells), track.regime=track.regime, track.fsire=track.fire, track.fuels=track.fuels))  
  
}