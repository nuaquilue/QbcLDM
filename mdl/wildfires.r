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


wildfires <- function(land, file.num.fires, file.fire.sizes, fire.rate.increase, 
                      baseline.fuel, fuel.types.modif, km2.pixel, t){

  ## Tracking
  cat("Wildfires", "\n"); tic("  t")
  options(warn=-1)
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Read and load input data
  load("inputlyrs/rdata/pigni.rdata")
  dist.num.fires <- read.table(file.num.fires, header = T)
  dist.fire.size <- read.table(file.fire.sizes, header = T)
  
  ## Wind direction between neigbours
  ## Wind direction is coded as 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
  # default.neigh <- data.frame(x=c(-1,1,805,-805,804,-806,806,-804),
  #                             windir=c(270,90,180,0,225,315,135,45),
  #                             dist=1000*c(2,2,2,2,sqrt(8),sqrt(8),sqrt(8),sqrt(8)))
  default.neigh <- data.frame(x=c(-1,1,805,-805), windir=c(270,90,180,0))
  default.nneigh <- nrow(default.neigh)
  
  ## Create a fuel data frame with types (according to Spp and Age): 1 - low, 2 - medium, and 3 - high
  ## Then assign baseline flammability to each type
  fuels <- fuel.type(land, fuel.types.modif)
  current.fuels <- group_by(fuels, zone) %>% summarize(x=mean(baseline))
  modif.fuels <- current.fuels
  modif.fuels$x <- 1+(current.fuels$x-baseline.fuel$x)/baseline.fuel$x
  
  ## Create a random permuation of the Fire Regime Zones to not burning FRZ always in the same order
  fr.zones <- unique(land$FRZone)
  fr.zones <- sample(fr.zones, length(fr.zones), replace=FALSE)   
  
  ## Initialize empty vector to track burned cells 
  burnt.cells <- visit.cells <-  numeric(0)
  
  ## Reset TrackFires data frame each run
  track.fire <- data.frame(zone=NA, fire.id=NA, wind=NA, atarget=NA, aburnt=NA)
  track.sprd <- data.frame(zone=NA, fire.id=NA, cell.id=NA, step=NA, 
                           flam=NA, wind=NA, sr=NA, pb=NA, burning=NA)
  
  
  ## Start burning until annual target area per fire zone is not reached
  fire.id <- 0; izone <- "ZD"; i <- 1
  for(izone in fr.zones){
    
    # Determine number of fires per zone  - considering fire rate increase with time 
    num.fires <- round(rpois(1, dist.num.fires$lambda[dist.num.fires$zone==izone]) * (1+(t*fire.rate.increase)) *
                        modif.fuels$x[modif.fuels$zone==izone])
    
    # Limit number of fires to a predefined range [1, max.nfires]
    num.fires <- pmax(1, pmin(num.fires, dist.num.fires$max.nfires[dist.num.fires$zone==izone]))
    
    pxlburnt <- fire.size.target <- 0
    
    ## Spreading of each fire
    for(i in 1:num.fires){
      
      ## ID for each fire event
      fire.id <- fire.id+1
      
      ## If previous fire has extinguished to early (burnt.area/target.area<50%), then "reuse" the
      ## target area for a new fire. This tends to occur when potentially big fires start in low-fuel lands
      ## Or in a agro-forest matrix. I may need to add a condition related to the minimum target area
      ## I allow to be reused (something big e.g < 200)
      if(i==1 | (i>1 & pxlburnt/fire.size.target>=0.5)){
        num.fires <- num.fires+1
        ## Determine potential area of the fires (i.e. the target area) in km2
        ## Fire size is drawn from an empirical discrete distribution defined in classes of 50 km2 
        ## Change fire sizes to account for changes in landscape-level flammability
        ## VERIFY THE IMPACT OF LANDSCAPE FLAMMABILITY ON FIRE SIZE. I dont' like to much
        fire.area <- (50*sample(1:nrow(dist.fire.size), 1, replace=F,
                                p=dist.fire.size[, which(levels(fr.zones)==izone)+2]) -
                        runif(1,0,50) ) * modif.fuels$x[modif.fuels$zone==izone]
        
        # Transform fire area (in km2) in fire size target (in pixels), some fires will lose size
        # while others will gain... on average the difference should be 0
        fire.size.target <- round(fire.area/km2.pixel)
      }
      
      ## Assign the main wind direction according to the fire spread type
      ## Wind directions: 0-N, 45-NE, 90-E, 135-SE, 180-S, 225-SW, 270-W, 315-NE
      # S 10%, SW 30%, W 60%  
      fire.wind <- sample(c(180,225,270), 1, replace=T, p=c(10,30,60)); fire.wind
      
      ## Select an ignition point according to probability of ignition and initialize tracking variables
      fire.front <- sample(pigni$cell.id, 1, replace=F, pigni$p)
      burnt.cells <- c(burnt.cells, fire.front)
      visit.cells <- c(visit.cells, fire.front)
      pxlburnt <- 1  
      fire.step <- 1
      track.sprd <- rbind(track.sprd, 
                          data.frame(zone=izone, fire.id=fire.id, cell.id=fire.front, step=fire.step, 
                                     flam=0, wind=0, sr=1, pb=1, burning=TRUE))
      
      
      ## Start speading from active cells (i.e. the fire front)
      while(pxlburnt<fire.size.target){
        
        ## Build a data frame with the theoretical 8 neighbours of cells in fire.front, 
        ## Add the wind direction and the distance.
        ## Filter neighbours in the study area and that have not been visited yet
        ## Then, look for their default flammability
        neigh.id <- data.frame(cell.id=rep(fire.front, each=default.nneigh)+ rep(default.neigh$x, length(fire.front)),
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
                     summarize(flam=max(flam)/wflam, wind=max(wind)/wwind, sr=max(sr), pb=max(pb), ) 
        sprd.rate$burning <-runif(nrow(sprd.rate), 0, pb.upper.th) <= sprd.rate$pb & sprd.rate$pb >= pb.lower.th
        if(nrow(sprd.rate)>0)
          track.sprd <- rbind(track.sprd, data.frame(zone=izone, fire.id=fire.id, cell.id=sprd.rate$cell.id,
                                          step=fire.step, flam=sprd.rate$flam, wind=sprd.rate$wind,
                                          sr=sprd.rate$sr, pb=sprd.rate$pb, burning=sprd.rate$burning))
        # 
        ## If at least there's a burning cell, continue, otherwise, stop
        if(!any(sprd.rate$burning))
          break
        
        ## Mark the cells burnt and visit, and select the new fire front
        ## 'mad' -> median absolute deviation
        burnt.cells <- c(burnt.cells, sprd.rate$cell.id[sprd.rate$burning])
        visit.cells <- c(visit.cells, sprd.rate$cell.id)
        exclude.th <- min(max(sprd.rate$sr)-0.005, 
                           rnorm(1,mean(sprd.rate$sr[sprd.rate$burning])-sd(sprd.rate$sr[sprd.rate$burning])/4,
                                 sd(sprd.rate$sr[sprd.rate$burning])/2))
        fire.front <- sprd.rate$cell.id[sprd.rate$burning & sprd.rate$sr>=exclude.th]
        
        ## Increase area burnt and fire.step 
        pxlburnt <- pxlburnt + sum(sprd.rate$burning)
        fire.step <- fire.step+1
        
        ## In the case, there are no cells in the fire front, stop trying to burn.
        ## This happens when no cells have burnt in the current spreading step
        if(length(fire.front)==0)
          break
        
      } # while 'fire'
      
      ## Write info about this fire
      track.fire <- rbind(track.fire, data.frame(zone=izone, fire.id, wind=fire.wind,
                                                 atarget=fire.size.target*km2.pixel, aburnt=pxlburnt*km2.pixel))
      cat(paste("Zone:", izone, "Fire:", fire.id, "- aTarget:", fire.size.target*km2.pixel, "- aBurnt:", pxlburnt*km2.pixel), "\n")
      
    }  #for 'num.fires'
  } #for 'zone'
  
    
  ## TRACKING
  track.fire <- track.fire[-1,]; track.fire
  track.regime <- group_by(track.fire, zone) %>% summarize(nfires=length(atarget), atarget=sum(atarget), aburnt=sum(aburnt)) %>%
                  left_join(group_by(land, FRZone) %>% summarize(atot=length(FRZone)), by=c("zone"="FRZone")) %>%
                  mutate(fire.cycle=round(time.step*atot/aburnt)) %>%
                  left_join(current.fuels, by="zone") %>% mutate(indx.combust=x) %>% select(-atot, -x)
  fuels.burnt <- fuel.type(filter(land, cell.id %in% burnt.cells), fuel.types.modif)
  track.fuels <- data.frame(table(fuels$zone, fuels$baseline) / matrix(table(fuels$zone), nrow=4, ncol=3),
                 table(fuels.burnt$zone, fuels.burnt$baseline) / matrix(table(fuels.burnt$zone), nrow=4, ncol=3)) %>%
                select(-Var1.1, -Var2.1)
  names(track.fuels) <- c("zone", "flam", "pctg.zone", "pctg.burnt")
  
  toc()
  ## Return the index of burnt cells and the tracking data.frames
  return(list(burnt.cells=burnt.cells, track.regime=track.regime, track.fire=track.fire, track.fuels=track.fuels))  
  
}