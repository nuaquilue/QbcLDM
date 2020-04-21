######################################################################################
###  landscape.dyn()
###
###  Description > Runs the Landscape Dynamics Model. 
###
###  Arguments >  
###
###  Details > The landscape-level processes are fire, clear-cuts and partial-cuts
###            Post-disturbance regeneration and forest succession are based on 
###            state transition matrices.
###
###  Value > Tabular and spatial outputs are written in 'out.path' directory if  
###          'write.tbl.outputs' and 'write.sp.outputs' are TRUE respectively.  
######################################################################################

landscape.dyn <- function(scn.name){
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(tictoc)
    library(raster)
    library(RANN)
    library(tidyverse)
  })
  source("mdl/wildfires.r")
  source("mdl/disturbance.cc.r") 
  source("mdl/sbw.outbreak.r") 
  source("mdl/disturbance.pc.r")   
  source("mdl/buffer.mig.r")            
  source("mdl/forest.transitions.r")  
  source("mdl/suitability.r") 
  source("mdl/fuel.type.r")  
  
  tic("  t")
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
      source(paste0("outputs/", scn.name, "/scn.custom.def.r"))

  
  ## Set the directory for writing spatial outputs (create if it does not exist yet) 
  if(write.sp.outputs){      
    if(!file.exists(paste0(out.path, "/lyr")))
       dir.create(file.path(getwd(), out.path, "/lyr"), showWarnings = F) 
  }

  
  ## Load MASK raster layer of the study area, and compute cell resolution in km2
  load(file="inputlyrs/rdata/mask.rdata")
  km2.pixel <- res(MASK)[1] * res(MASK)[2] / 10^6
  
  
  ## Load temperature and precipitation 5-year predictions according to the climatic scenario.
  ## If climate change is not activated, initial temp and precip will be used for the whole simulation.
  if(!is.na(clim.scn)){
    load(file=paste0("inputlyrs/rdata/temp_", clim.scn, "_ModCan.rdata")) 
    load(file=paste0("inputlyrs/rdata/precip_", clim.scn, "_ModCan.rdata"))  
  }
  

  ## Build the discrete time sequence according to time.step
  ## 0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85
  time.seq <- seq(0, time.horizon, time.step) 
  
  
  ## Tracking data.frames  ( review description ARE WRONG!!)
  ## 1. Area of each species within poor, medium, or good climatic-soil suitability levels
  ## 2. The distribution of Ages Class per domain
  ## 3. The area affected by the disturbances in each Biolcimatic Domain
  track.spp.frzone <- data.frame(run=NA, year=NA, FRZone=NA, SppGrp=NA, Area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, BCDomain=NA, SppGrp=NA, 
                                    C20=NA, C40=NA, C60=NA, C80=NA, C100=NA, Cold=NA)
  track.suit.class <- data.frame(run=NA, year=NA, BCDomain=NA, PotSpp=NA, poor=NA, med=NA, good=NA)
  track.land.fuel <- data.frame(run=NA, year=NA, zone=NA, x=NA)
  track.fire.regime <- data.frame(run=NA, year=NA, zone=NA, nfires=NA, atarget=NA,
                             aburnt=NA, fire.cycle=NA, indx.combust=NA)
  track.fires <- data.frame(run=NA, year=NA, zone=NA, fire.id=NA, wind=NA, atarget=NA, atarget.modif=NA, aburnt=NA)
  track.fuels <- data.frame(run=NA, year=NA, zone=NA, flam=NA, pctg.zone=NA, pctg.burnt=NA)
  
  
  ## Start the simulations
  irun <- 1
  for(irun in 1:nrun){
    
    ## Load dynamic state variables 
    load(file="inputlyrs/rdata/land.rdata")
    
    ## Set the scheduling of the processes
    fire.schedule <- seq(0, time.horizon, fire.step)
    cc.schedule <- seq(0, time.horizon, cc.step)
    pc.schedule <- seq(0, time.horizon, pc.step)
    if(sbw.step.fix)
      sbw.schedule <- seq(sbw.step, time.horizon, sbw.step)
    else
      sbw.schedule <- sample(c(30,35,40), size=floor(time.horizon/30), replace=TRUE)

    
    ## Record initial species distribution per fire zone, and distribution of age classes per BCDomain
    track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=0, 
                              group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
    track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=0, 
                              group_by(land, BCDomain, SppGrp) %>% summarize(C20=sum(Age<=20)*km2.pixel, 
                              C40=sum(Age<=40)*km2.pixel, C60=sum(Age<=60)*km2.pixel, C80=sum(Age<=80)*km2.pixel, 
                              C100=sum(Age<=100)*km2.pixel, Cold=sum(Age>100)*km2.pixel)))
    ## Record initial suitability classes per BCDomain 
    suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
    aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
           group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, 
           med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
    track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=0, aux))
    rm(suitab); rm(aux)
    ## Calculate the baseline fuel at the fire regime zone and track it
    baseline.fuel <- group_by(fuel.type(land, fuel.types.modif), zone) %>% summarize(x=mean(baseline))
    track.land.fuel <- rbind(track.land.fuel, data.frame(run=irun, year=0, baseline.fuel))

    # # pour le cas d'un calcul avec integration a priori du risque de feu, on cr?e une matrice 
    # # qui contiendra le niveau de r?colte ? maintenir sur tout l'horizon
    ref.harv.level <- table(land$MgmtUnit)*0 
    
    
    ## Start 
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      print(paste0("run:", irun, " - time:", t+year.ini))
      
      ## Update climatic variables at each time step if climate change is activated
      ## Column 1 is cell.index, the following columns account for climate in 2000-2004, 2005-2009, 2010-2014, etc.
      ## The last column (temp19) then corresponds to the period 2095-2100
      ## The first time step (t=0) we start at 2010, so the first column to start with is column 4
      if(!is.na(clim.scn)){
        land$Temp <- unlist(cc.temp[3+which(time.seq==t)], use.names=FALSE)
        land$Precip <- unlist(cc.precip[3+which(time.seq==t)], use.names=FALSE)
      }
      
      
      ##################################### PROCESSES OF CHANGE #####################################
      ## 1. FIRE
      burnt.cells <- integer() 
      if(processes[fire.id] & t %in% fire.schedule){
        fire.out <- wildfires(land, file.num.fires, file.fire.sizes, fire.rate.increase, 
                              baseline.fuel, fuel.types.modif, km2.pixel, t)
        burnt.cells <- fire.out[[1]]
        if(nrow(fire.out[[3]])>0){
          track.fire.regime <- rbind(track.fire.regime, data.frame(run=irun, year=t+year.ini, fire.out[[2]]))
          track.fires <- rbind(track.fires, data.frame(run=irun, year=t+year.ini, fire.out[[3]]))
          track.fuels <- rbind(track.fuels, data.frame(run=irun, year=t+year.ini, fire.out[[4]]))
        }
        # Done with fires
        land$TSDist[land$cell.id %in% burnt.cells] <- 0
        land$DistType[land$cell.id %in% burnt.cells] <- fire.id
        fire.schedule <- fire.schedule[-1]          
      }
      
        
      ## 2. SBW (under development)
      kill.cells <- integer()
      if(processes[sbw.id] & t %in% sbw.schedule){
        kill.cells <- sbw.outbreak(land, severity=1, write.tbl.outputs=T,
                                   km2.pixel=1, irun=1, t=0, out.path=NULL, out.overwrite=T)
        # Done with outbreak
        land$TSDist[land$cell.id %in% kill.cells] <- 0
        land$DistType[land$cell.id %in% kill.cells] <- sbw.id
        sbw.schedule <- sbw.schedule[-1]
      } 
      
      
      ## 3. CLEAR CUTING
      cc.cells <- integer()
      if(processes[cc.id] & t %in% cc.schedule){
        cc.cells <- disturbance.cc(subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, 
                                                         TSF,age.mat,TSPC,Temp), (SppGrp!="NonFor" & !is.na(MgmtUnit))), 
                                   cc.step, target.old.pct, diff.prematurite, hor.plan,
                                   a.priori, replanif, salvage.rate.event, salvage.rate.FMU, ref.harv.level, 
                                   write.tbl.outputs, km2.pixel, irun, t, out.path, 
                                   out.overwrite=(irun==1 & t==cc.schedule[1]))
        # Done with clear cuts
        land$TSDist[land$cell.id %in% cc.cells] <- 0
        land$DistType[land$cell.id %in% cc.cells] <- cc.id
        cc.schedule <- cc.schedule[-1]  
      }       
      
      
      ## 4. PARTIAL CUTING (under development)
      ## On consid?re que la rotation de coupe partielle (temps minimal entre deux coupes partielles) 
      ## correspond ? la moiti? de l'?ge d'admissibilit? pour la coupe totale (land$age.mat).
      ## TSPC = temps depuis coupe partielle. Une valuer de 0 est assign?e lorsque le peuplement 
      ## est affect? par une perturbation s?v?re
      # REVIEW land$TSPC[land$TSD < (land$age.mat/2)] <- 0
      pc.cells <- integer()
      if(processes[pc.id] & t %in% pc.schedule){
        pc.cells <- disturbance.pc(subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, TSF,age.mat,TSPC), (SppGrp!="NonFor" & !is.na(MgmtUnit))), 
                                   cc.step, hor.plan,
                                   write.tbl.outputs, km2.pixel, irun, t, out.path, 
                                   out.overwrite=(irun==1 & t==pc.schedule[1]))
        # Done with partial cuts
        land$TSDist[land$cell.id %in% pc.cells] <- 0
        land$DistType[land$cell.id %in% pc.cells] <- pc.id
        pc.schedule <- pc.schedule[-1]  
      }  

      # a priori - en d?veloppement
      # Lorsque l'option 'a priori' est s?lectionn?e, le niveau de r?f?rence est calcul? durant
      # la premi?re p?riode, en appliquant une p?nalit? (a.priori). Ce niveau de r?colte (ref.harv.level)
      # en coupe totale est maintenu tel quel durant les p?riodes subs?quentes.
      if (t==5) {
        ref.harv.level  <- table(land$MgmtUnit[land$cell.id %in% cc.cells])
        if(irun==1) {
          ref.harv.level.cp <- table(land$MgmtUnit[land$cell.id %in% pc.cells])
          ref <- cbind(ref.harv.level,ref.harv.level.cp)*km2.pixel
          write.table(ref, file = paste0(out.path, "/InitialHarvestLevel.txt"),
                          quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)                 
        }
      } 
      else{
        bid <- table(land$MgmtUnit[land$cell.indx %in% cc.cells])
        ref.harv.level <- pmax(ref.harv.level,bid)
      }      
      
      

      ##################################### VEGETATION DYNAMICS #####################################
      
      ## First of all, save a vector containing initial forest composition for comparison at the end of the for loop
      initial.forest.comp <- land$SppGrp
      
      ## Natural regeneration of forest after disturbance depends on the nature of the disturbance, 
      ## the age of the stand at the time the disturbance occurred, and the environmental suitability
      ## according to climate and soils. Compute it:
      suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
      
      
      ## Regeneration after fire
      buffer <- buffer.mig(land, burnt.cells, potential.spp)
      land$SppGrp[land$cell.id %in% burnt.cells] <- forest.trans(filter(land, cell.id %in% burnt.cells), 
                 post.fire.reg, buffer, suitab, potential.spp, dtype="B", p.failure, age.seed, suboptimal, enfeuil)
      

      ## Regeneration after sbw outbreak
      buffer <- buffer.mig(land, kill.cells, potential.spp)
      land$SppGrp[land$cell.id %in% kill.cells] <- forest.trans(filter(land, cell.id %in% kill.cells),
                 post.sbw.reg, buffer, suitab, potential.spp, dtype="S", p.failure, age.seed, suboptimal, enfeuil)
      
      
      ##  Regeneration after clear-cutting
      buffer <- buffer.mig(land, cc.cells, potential.spp)
      land$SppGrp[land$cell.id %in% cc.cells] <- forest.trans(filter(land, cell.id %in% cc.cells),
                  post.harvest.reg, buffer, suitab, potential.spp, dtype="C", p.failure, age.seed, suboptimal, enfeuil)
      
      ## Natural succession of tree spp at every 40 years starting at TSDist = 70
      if(succ.enable){
        cat("Natural succession", "\n")
        chg.comp.cells <- filter(land, (Age-AgeMatu) %in% seq(40,400,40) & Tcomp>=40) %>% select(cell.id)
        buffer <- buffer.mig(land, unlist(chg.comp.cells), potential.spp)
        land$SppGrp[land$cell.id %in% unlist(chg.comp.cells)] <- 
            forest.trans(filter(land, cell.id %in% unlist(chg.comp.cells)), 
            forest.succ, buffer, suitab, potential.spp, dtype="S", p.failure, age.seed, suboptimal, enfeuil)
        ## For those cells that change composition, reset TSD at X years before maturity
        ## to account for the fact that a major change in species dominance is
        ## generaly due to significant mortality in the overstory
        land$Age[(land$SppGrp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] <- 
          land$AgeMatu[(land$SppGrp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] - 
          sample(seq(10,40,5),1)
      }
      
      
      ## Now, for each cell that has changed composition (because of natural succession or regeneration
      ## post-distrubance), re-initialize Tcomp
      land$Tcomp[land$SppGrp != initial.forest.comp] <- 0
      
      
      ## Finally, Aging: I ncrement Time Since Disturbance and Time Last Forest Composition change by time.step 
      land$TSDist <- land$TSDist + time.step
      land$Tcomp <- land$Tcomp + time.step
      
      
      
      ##################################### TRACKING AND SPATIAL OUTS #####################################
      track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=t+year.ini, 
                                group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
      track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=t+year.ini, 
                                  group_by(land, BCDomain, SppGrp) %>% summarize(C20=sum(Age<=20)*km2.pixel, 
                                  C40=sum(Age<=40)*km2.pixel, C60=sum(Age<=60)*km2.pixel, C80=sum(Age<=80)*km2.pixel, 
                                  C100=sum(Age<=100)*km2.pixel, Cold=sum(Age>100)*km2.pixel)))
      suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
      aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
              group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, 
              med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
      track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=t+year.ini, aux))
      aux <- group_by(fuel.type(land, fuel.types.modif), zone) %>% summarize(x=mean(baseline))
      track.land.fuel <- rbind(track.land.fuel, data.frame(run=irun, year=t+year.ini, aux))
      rm(suitab); rm(aux)
      
      ## If required, plot maps of DisturbanceType at each time step 
      if(write.sp.outputs){
        MAP <- MASK
        cat("... writing output layers", "\n")
        # nfire <- sum(track.fire$year==t, na.rm=T)
        # sizes <- filter(track.fire, year==t) %>% group_by(swc, fire.id) %>% summarise(ab=aburnt.highintens+aburnt.lowintens)
        # Ignitions' cell.id 
        # igni.id <- burnt.cells[c(1,cumsum(sizes$ab)[1:(nfire-1)]+1)] 
        MAP[!is.na(MASK[])] <- land$DistType*(land$TSDist==time.step)  ## 0 will be time.step
        # MAP[igni.id] <- 9
        writeRaster(MAP, paste0(out.path, "/lyr/DistType_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
      }
      
    } # t
  } # irun
  
  
  cat("... writing outputs", "\n")
  write.table(track.spp.frzone[-1,], paste0(out.path, "/SppByFRZone.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.age.class[-1,], paste0(out.path, "/SppByAgeClass.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.suit.class[-1,], paste0(out.path, "/SuitabilityClasses.txt"), quote=F, row.names=F, sep="\t")
  track.land.fuel[,4] <- round(track.land.fuel[,4], 3)
  write.table(track.land.fuel[-1,], paste0(out.path, "/FuelLand.txt"), quote=F, row.names=F, sep="\t")
  track.fire.regime[,8] <- round(track.fire.regime[,8], 2) 
  write.table(track.fire.regime[-1,], paste0(out.path, "/FireRegime.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.fires[-1,], paste0(out.path, "/Fires.txt"), quote=F, row.names=F, sep="\t")
  track.fuels[,5:6] <- round(track.fuels[,5:6], 2)
  write.table(track.fuels[-1,], paste0(out.path, "/Fuels.txt"), quote=F, row.names=F, sep="\t")
  
  toc()
} 


