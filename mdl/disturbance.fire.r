######################################################################################
###  disturbance.fire()
###
###  Description >  Simulates fire events in four zones with distinct fire regime.
###                 Called in landscape.dyn
###
###  Arguments >  
###   SPECIES : raster of the study area with the species groups
###   subland : appropriate selection fo the data frame of the state variables
###   NFdistrib : data frame of the number of fire per period by fire regime zone
###   FSdistrib : data frame of the fire sizes distribution by fire regime zone
###   fire.step : number of years each time step represents
###   write.tbl.outputs : if TRUE
###   fire.rate.increase : Rate of increase of number of fire through time (climate change)
###   km2.pixel : number of km2 per pixel on the grid 
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###   out.path : directory path to save output text files
###   out.overwrite : if TRUE the output text files are overwritten 
###   plot.fires : plot fire perimeters in ascii format
###
###  Details > For each fire regime zone, the number of fires and their sizes are derived
###   from the input distributions. Fire spreads from random ignition points until target
###   area is burnt. Re-burning is not allowed in the same time step.
###
###  Value >  A vector of the indexes of the burnt cells.
######################################################################################

# subland <- subset(land, select=c(cell.indx, FRZone, TSD,SppGrp))

disturbance.fire <- function(SPECIES, subland, NFdistrib, FSdistrib, fire.step,
                             write.tbl.outputs=T,fire.rate.increase, km2.pixel=km2.pixel, irun=1, t, out.path=NULL, 
                             out.overwrite=T, plot.fires=FALSE, avec.combu,fuel.types.baseline,
                             fuel.types.modif){

  # Silence
  options(warn=-1)
  
  # Initialize empty vector for the future burned cells in the current time step
  burnt.cells <- numeric(0)

  # Initialize empty vector to save the id.fires and plot it as a raster
  if(plot.fires)
    val <- vector("integer", ncell(SPECIES))
  
  # Track the fires
  id.fire <- 0
  prob.sprd <- SPECIES[] # creer un vecteur de la bonne longueur
  
  vec.fuels <- fuel.types(subland,fuel.types.modif)  #  créer vecteur des fuel types
  # Define the spreading potential for different forest types - uniform if avec.combu == 0
  # Si avec.combu ==1, les probabilités de brulage tiennent compte de la composition et de l'âge
  # des peuplements
    if (avec.combu) {
       prob.sprd[!is.na(prob.sprd)] <- vec.fuels
      } else {
      prob.sprd[!is.na(prob.sprd)] <- runif(length(!is.na(prob.sprd)),0.1,0.8) # 0.2 ??
       }

  ww <-  aggregate(vec.fuels,  by=list (subland$FRZone), FUN=mean)
  #print(ww)
  
  # Create a random permuation of the Fire Regime Zones 
  fr.zones <- unique(subland$FRZone)
  fr.zones <- sample(fr.zones, length(fr.zones), replace=FALSE)   

  #### landscape level change in flammability, by comparing fuel types at time=t and at baseline (t=0)
  
  modif.fuels <- 1+(ww-fuel.types.baseline)/fuel.types.baseline
  
  # Simulate fires for each Zone   zone = fr.zones[3]
  for(zone in fr.zones){

    # Determine number of fires per zone  - considering fire rate increase with time 

      num.fires <- NFdistrib$lambda[NFdistrib$zone==zone] * fire.step * (1+(t*fire.rate.increase))
      
      if (avec.combu) {
        num.fires <- round((modif.fuels[ww$Group.1 == zone,2]  * num.fires))
      }
      
    # Limit number of fires to a predefined range [min.nfires, max.nfires]
    if(num.fires < NFdistrib$min.nfires[NFdistrib$zone==zone] * fire.step) 
      num.fires <- NFdistrib$min.nfires[NFdistrib$zone==zone] * fire.step
    if(num.fires > NFdistrib$max.nfires[NFdistrib$zone==zone] * fire.step) 
      num.fires <- NFdistrib$max.nfires[NFdistrib$zone==zone] * fire.step
       
    # Determine potential size of these fires (i.e. the target area) in km2
    # Fire size is drawn from an empirical discrete distribution defined by 
    # classes of 50 km2 
    
    fire.sizes <- round(
                  50*sample(1:nrow(FSdistrib), num.fires, replace=T, p=FSdistrib[, which(levels(fr.zones)==zone)+2]) -
                  runif(num.fires,0,50), 0)
  
    ###############################
    #############  Change fire sizes to account for changes in landscape-level flammability

    if (avec.combu) {
      fire.sizes <- (modif.fuels[ww$Group.1 == zone,2]  * fire.sizes)
    }
  
    #################
    
    
    # Transform fire sizes (in km2) in fire extents (in pixels), some fires will lose area
    # while others will gain... on average the difference should be 0
    fire.extents <- round(fire.sizes/km2.pixel)
    # Select num.fires ignitions points within the zone 
    ignis <- sample(subland$cell.indx[subland$FRZone==zone], num.fires)

    SPREADPROB <- raster(val=prob.sprd, ext=extent(SPECIES), crs=NA, res=res(SPECIES))

    # Simulate the spreading of each individual fire
    fires <- fire.spread(SPECIES, loci=ignis, spreadProb=SPREADPROB, mask=NA, 
                         maxSize=fire.extents, directions=8L, iterations=10000L, 
                         spreadProbLater=NA_real_)
    
    
    ################################
    #################################
    
    sub.brul <- subland[subland$cell.indx %in% fires[["perims"]]$cell.indx,]
    sub.brul2 <- table(fuel.types(sub.brul,fuel.types.modif))
    sub.brul3 <-  round(sub.brul2 / sum(sub.brul2[] ),2)
    f.t.z <- fuel.types(subland[subland$FRZone==zone,],fuel.types.modif)
    f.t.z2 <- table(f.t.z)
    f.t.z3<-  round(f.t.z2 / sum(f.t.z2[] ),2)
    #print(paste("zone",zone,"brul.z",f.t.z3,"brulis",sub.brul3))
    ########################################  
    ### REPORT: write the results by fire regime zone: 
    if(write.tbl.outputs){      
      
      # Id of fire, target and burnt area 
      write.table(data.frame(run=irun, time=t, zone=zone, 
                             id.fire=(id.fire+1):(id.fire+nrow(fires$area)), 
                             target.area = (fires[["areas"]]$target)*km2.pixel, 
                             burnt.area = (fires[["areas"]]$burnt)*km2.pixel),
                  file=paste0(out.path, "/Fires.txt"), sep="\t", quote=FALSE,
                  append=(!out.overwrite | (id.fire != 0)),
                  row.names=FALSE, col.names=(out.overwrite & id.fire == 0) )   
      
      # Number of target fires, num of actual fires, target burnt area and effectivele burnt area
      write.table(data.frame(run=irun, time=t, zone=zone, target.n.fires=num.fires, 
                             n.fires=nrow(fires$areas), 
                             target.area=sum(fires[["areas"]]$target)*km2.pixel, 
                             burnt.area=sum(fires[["areas"]]$burnt)*km2.pixel,
                             fire.cycle = round(((sum(subland$FRZone==zone)*5)/(sum(fires[["areas"]]$burnt)))), # cycle
                             ind.combust = round(ww$x[ww$Group.1 == zone],2)),   
                  file=paste0(out.path, "/FireRegime.txt"), sep="\t", quote=FALSE,
                  append=(!out.overwrite | (zone != fr.zones[1])),
                  row.names=FALSE, col.names=(out.overwrite & zone == fr.zones[1]) )
      ### statistique spread prob
      write.table(data.frame(run=irun, time=t, zone=zone, brulcl1=sub.brul3[1], brulcl2=sub.brul3[2]
                             , brulcl3=sub.brul3[3], zon.cl1=f.t.z3[1],zon.cl12=f.t.z3[2], zon.cl3=f.t.z3[3]),   
                  file=paste0(out.path, "/Spread.txt"), sep="\t", quote=FALSE,
                  append=(!out.overwrite | (zone != fr.zones[1])),
                  row.names=FALSE, col.names=(out.overwrite & zone == fr.zones[1]) )
    }

    # Update id.fire
    id.fire <- id.fire + nrow(fires$areas)
    
    # Add the indices of the burnt cells to the global vector
    burnt.cells <- c(burnt.cells, fires[["perims"]]$cell.indx)
    # Update vector to plot fire perimeters
    if(plot.fires)
      val[fires[["perims"]]$cell.indx] <- fires[["perims"]]$fire.id + max(val)
    
  } # zone  
  
  
  
  
  ### Plot fires perimeters
  if(plot.fires){
    IDBURNT <- raster(val=val, ext=extent(SPECIES), res=res(SPECIES), crs=NA)
    out.raster <- paste0(out.path, "/asc/Fires_", irun, "_", t, ".asc")
    writeRaster(IDBURNT, out.raster, format="ascii", overwrite=T)
  }

  ## Return the index of burnt cells
  return(burnt.cells) 
}