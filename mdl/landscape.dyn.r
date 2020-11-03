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
    require (reshape)
  })
  source("mdl/wildfires.r")
  source("mdl/sbw.outbreak.r") 
  source("mdl/harvest.volume.r") 
  source("mdl/harvest.area.r")
  source("mdl/timber.partial.r") 
  source("mdl/timber.partial.volume.r") 
  source("mdl/timber2.r") 
  source("mdl/timber.volume.r") 
  source("mdl/buffer.mig4.r") 
  source("mdl/forest.transitions.r")  
  source("mdl/suitability.r") 
  source("mdl/fuel.type.r")  
  source("mdl/select.others2.r")
  source("mdl/volume.vec.r")  

  tic("  t")
  options(warn=-1)
  select <- dplyr::select
  
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
    load(file=paste0("inputlyrs/rdata/temp", clim.scn, "_ModCan.rdata")) 
    load(file=paste0("inputlyrs/rdata/precip", clim.scn, "_ModCan.rdata"))  
  }
  

  ## Build the discrete time sequence according to time.step
  ## 0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85
  ## lenght(time.seq) is 18
  time.seq <- seq(0, time.horizon, time.step) 
  
  
  ## Tracking data.frames  ( review description ARE WRONG!!)
  ## 1. Area of each species within poor, medium, or good climatic-soil suitability levels
  ## 2. The distribution of Ages Class per domain
  ## 3. The area affected by the disturbances in each Biolcimatic Domain
  track.spp.frzone <- data.frame(run=NA, year=NA, FRZone=NA, SppGrp=NA, Area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, MgmtUnit=NA, SppGrp=NA, AgeClass=NA, n=NA)
  track.suit.class <- data.frame(run=NA, year=NA, BCDomain=NA, PotSpp=NA, poor=NA, med=NA, good=NA)
  track.fire.regime <- data.frame(run=NA, year=NA, zone=NA, nfires=NA, atarget=NA,
                                  aburnt=NA, fire.cycle=NA, indx.combust=NA)
  track.fires <- data.frame(run=NA, year=NA, zone=NA, fire.id=NA, wind=NA, atarget=NA, aburnt=NA)  # atarget.modif=NA,
  track.fuels <- data.frame(run=NA, year=NA, zone=NA, pctg.zone=NA, pctg.burnt=NA)
  track.ccut <- data.frame(run=NA, year=NA,  MgmtUnit=NA, tot.inc=NA, even.age=NA, a.mat=NA, a.inc.burnt=NA, 
                           a.inc.mat.burnt=NA, a.inc.kill=NA, a.inc.mat.kill=NA, a.reg.fail.ex=NA, a.reg.fail.in=NA,
                           area.salvaged=NA, area.unaff=NA, v.salv=NA, v.unaff=NA,
                           a.pcut=NA, v.pcut=NA)
  track.spp.ccut <- data.frame(run=NA, year=NA,  MgmtUnit=NA, SppGrp=NA, spp.ccut=NA, spp.ccut.vol=NA,
                               spp.pcut=NA, spp.pcut.vol=NA)
  track.pcut <- data.frame(run=NA, year=NA,  MgmtUnit=NA, uneven.age=NA, s.mat=NA, s.rec.pc=NA)
  track.spp.pcut <- data.frame(run=NA, year=NA,  MgmtUnit=NA, SppGrp=NA, x=NA)
  track.vol <- data.frame(run=NA, year=NA,  MgmtUnit=NA, SppGrp=NA, DistType=NA, x=NA)
    
  potential.spp$persist <- persist
  
  ## Start the simulations
  irun <- 1
  for(irun in 1:nrun){
    
    ## Load dynamic state variables 
    load(file="inputlyrs/rdata/land.rdata")
    
    ## Set the scheduling of the processes
    fire.schedule <- seq(0, time.horizon, fire.step)
    cc.schedule <- seq(0, time.horizon, cc.step)
    pc.schedule <- seq(0, time.horizon, pc.step)
    sbw.schedule <- c(0,35,70)
    #if(sbw.step.fix) {
    #  sbw.schedule <- seq(sbw.step, time.horizon, sbw.step)
    #}    else {
    #        sbw.schedule <- sample(c(30,35,40), size=floor(time.horizon/30), replace=TRUE)
    #}
    
    ## Record initial species distribution per fire zone, and distribution of age classes per BCDomain
    track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=0, 
                              group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
    breaks <- c(0,20,40,60,80,100,999)
    tags <- c("C10","C30", "C50", "C70", "C90", "OLD")
    land$AgeClass <- cut(land$Age, 
                      breaks=breaks, 
                      include.lowest=TRUE, 
                      right=TRUE, labels=tags)
    track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=year.ini-5, 
                              group_by(land, MgmtUnit, SppGrp) %>% count(AgeClass))) #MgmtUnit 
 


    
    ## Record initial suitability classes per BCDomain 
    suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
    aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
           group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, 
           med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
    track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=0, aux))
    rm(suitab); rm(aux)
    
    ## Matrix to save the sustained yield level at time t = 0, after clear.cut has happeened
    ## It will be used when the option "replan" is not activated, 
    ## so recalculation of AAC level is only calculated once, during the first period
    ref.harv.level <- table(land$MgmtUnit)*NA
    
    # variables temporaires
    land$TSF <- 100
    land$TSSBW <- 100
    land$TSCC <- land$Age
    land$TSPCut <- land$Age - (land$AgeMatu/2)
    
    # determiner le regime de coupe - even aged (1), uneven aged(0), autres(2)
    land <- mutate(land, rndm=runif(nrow(land)))
    land$even <-2
    land$even[land$SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB") & is.na(land$Exclus) & land$rndm<=0.95] <- 1
    land$even[land$SppGrp %in% c("EPN", "PET", "SAB", "OthCB", "OthCT", "OthDB") & is.na(land$Exclus) & land$rndm>0.95] <- 0
    land$even[land$SppGrp %in% c("BOJ", "ERS", "OthDT")& is.na(land$Exclus) & land$rndm>0.95] <- 1
    land$even[land$SppGrp %in% c("BOJ", "ERS", "OthDT")& is.na(land$Exclus) & land$rndm<=0.95] <- 0    

    
    fuels <- fuel.type(land, fuel.types.modif, NA)
    baseline.fuel <- group_by(fuels, zone) %>% summarize(x=mean(baseline))
    
    ## Start 
    t <- 0  #t<-5
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      print(paste0("scn:", scn.name, " - run:", irun, "/", nrun, " - time:", t+year.ini))
      
      ## Update climatic variables at each time step if climate change is activated
      ## Column 1 is cell.index, the following columns account for climate in 2000-2004, 2005-2009, 2010-2014, etc.
      ## The last column (temp19) then corresponds to the period 2095-2100
      ## The first time step (t=0) we start at 2010, so the first column to start with is column 4
      if(!is.na(clim.scn) & t < 90){
        aux <- cc.temp[,c(1,3+which(time.seq==t))]
        names(aux) <- c("cell.id", "Temp")
        land <- select(land, -Temp) %>% left_join(aux, by="cell.id")
        aux <- cc.precip[,c(1,3+which(time.seq==t))]
        names(aux) <- c("cell.id", "Precip")
        land <- select(land, -Precip) %>% left_join(aux, by="cell.id")
     }

      
      ##################################### PROCESSES OF CHANGE #####################################
      ## 1. FIRE
      burnt.cells <- integer() 
      if(processes[fire.id] & t %in% fire.schedule){
        fire.out <- wildfires(land, file.fire.regime, file.fire.sizes, baseline.fuel, 
                              fuel.types.modif, pigni.opt, km2.pixel, t, increase.fire, avec.combu,zone.fuel.load)
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
      land$TSF[land$cell.id %in% burnt.cells] <- 0
        
      ## 2. SBW (under development)
      kill.cells <- integer()
      if(processes[sbw.id] & t %in% sbw.schedule){
        kill.cells <- sbw.outbreak(land, severity=1, km2.pixel)
        # Done with outbreak
        land$TSSBW[land$cell.id %in% kill.cells] <- 0
        land$TSDist[land$cell.id %in% kill.cells] <- 0
        land$DistType[land$cell.id %in% kill.cells] <- sbw.id
        sbw.schedule <- sbw.schedule[-1]
      }

      ###################################################
      ################ 3 and 4 : harvesting 
      ########## Timber supply calculation - only during first period if replanning is not selected
      # AREA BASED
      if((t == 0 | replanif==1)& timber.supply == "area.based") {
              # even-aged
              harv.level <- timber2(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                      salvage.rate.event, salvage.rate.FMU, ref.harv.level, km2.pixel, fire.id, sbw.id, t)
              TS.CC.area <- harv.level
              # uneven-aged
              harv.level.pc <- timber.partial(land, hor.plan, km2.pixel, pc.step)  
              TS.PC.area <- harv.level.pc
      }

      # VOLUME BASED - in development
      if(timber.supply == "volume.based" & (processes[cc.id] & t %in% cc.schedule)
         &(t == 0 | replanif==1)) {
      #source("mdl/timber.partial.volume.r")
      #source("mdl/timber.volume.r")
      TS.CC.vol <- timber.volume(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                      salvage.rate.event, salvage.rate.FMU, harv.level, km2.pixel, fire.id, sbw.id, t)
      TS.PC.vol <-timber.partial.volume(land, hor.plan, km2.pixel, pc.step)
      }
      
      ## Selection of harvested cells based on timber supply
 
      if(timber.supply == "volume.based" & (processes[cc.id] & t %in% cc.schedule)) {
          cc.cells <- integer()
          #source("mdl/harvest.volume.r") 
          harv.out <- harvest.vol(land, cc.step, diff.prematurite, hor.plan, TS.CC.vol,TS.PC.vol,
                              salvage.rate.event, harv.level, km2.pixel, fire.id, sbw.id, t)
          cc.cells <- harv.out[[1]]
          pc.cells <- harv.out[[2]]
          if(nrow(harv.out[[3]])>0){
            track.ccut <- rbind(track.ccut, data.frame(run=irun, year=t+year.ini, harv.out[[3]]))
            track.spp.ccut <- rbind(track.spp.ccut, data.frame(run=irun, year=t+year.ini, harv.out[[4]]))
          }
          # Done with clear cuts
          land$TSDist[land$cell.id %in% cc.cells] <- 0
          #land$TSDist[land$cell.id %in% pc.cells] <- 0
          land$TSPCut[land$cell.id %in% pc.cells] <- 0
          land$TSCC[land$cell.id %in% cc.cells] <- 0
          land$DistType[land$cell.id %in% cc.cells] <- cc.id
          land$DistType[land$cell.id %in% pc.cells] <- pc.id
          cc.schedule <- cc.schedule[-1]  
          pc.schedule <- pc.schedule[-1]  
                 }
      
      if(timber.supply == "area.based" & (processes[cc.id] & t %in% cc.schedule)){      
          #source("mdl/harvest.area.r")
          cc.cells <- integer()
          harv.out <- harvest.area(land, cc.step, diff.prematurite, hor.plan, TS.CC.area,TS.PC.area,salvage.rate.FMU,
                          salvage.rate.event, harv.level, km2.pixel, t, p.failure, age.seed)
          cc.cells <- harv.out[[1]]
          pc.cells <- harv.out[[2]]
          if(nrow(harv.out[[3]])>0){
            track.ccut <- rbind(track.ccut, data.frame(run=irun, year=t+year.ini, harv.out[[3]]))
            track.spp.ccut <- rbind(track.spp.ccut, data.frame(run=irun, year=t+year.ini, harv.out[[4]]))
           }
          # Done with clear cuts
          land$TSDist[land$cell.id %in% cc.cells] <- 0
          #land$TSDist[land$cell.id %in% pc.cells] <- 0
          land$TSPCut[land$cell.id %in% pc.cells] <- 0
          land$TSCC[land$cell.id %in% cc.cells] <- 0
          land$DistType[land$cell.id %in% cc.cells] <- cc.id
          land$DistType[land$cell.id %in% pc.cells] <- pc.id
          cc.schedule <- cc.schedule[-1]  
          pc.schedule <- pc.schedule[-1]  
      }

      ##################################### VEGETATION DYNAMICS #####################################
      
      ## First of all, save a vector containing initial forest composition for comparison at the end of the for loop
      initial.forest.comp <- land$SppGrp
      if(enable.succ){  
         
      ## Natural regeneration of forest after disturbance depends on the nature of the disturbance, 
      ## the age of the stand at the time the disturbance occurred, and the environmental suitability
      ## according to climate and soils. Compute it:
      suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal)
      
      ## Regeneration after fire
      if(length(burnt.cells)>0) {
      buffer <- buffer.mig4(land, burnt.cells, potential.spp)
      land$SppGrp[land$cell.id %in% burnt.cells] <- forest.trans(land, burnt.cells, post.fire.reg, buffer, 
                 suitab, potential.spp, dtype="B", p.failure, age.seed, suboptimal, enfeuil)
      }

      ## Regeneration after sbw outbreak
      if(length(kill.cells)>0) {
      buffer <- buffer.mig4(land, kill.cells, potential.spp)
      land$SppGrp[land$cell.id %in% kill.cells] <- forest.trans(land, kill.cells, post.sbw.reg, buffer, 
                 suitab, potential.spp, dtype="O", p.failure, age.seed, suboptimal, enfeuil)
              
      }

      ##  Regeneration after clear-cutting
      if(length(cc.cells)>0) {
      buffer <- buffer.mig4(land, cc.cells, potential.spp)
      land$SppGrp[land$cell.id %in% cc.cells] <- forest.trans(land, cc.cells,post.harvest.reg, buffer, 
                  suitab, potential.spp, dtype="C", p.failure, age.seed, suboptimal, enfeuil)
      }
      
      #######
      # maintien forcé de la composition forestière (plantation)
      if(lutte ==1) {
        territ <- !is.na(land$MgmtUnit) 
        # superficie qui passe de feu à res et l'inverse
        plant.1 <- initial.forest.comp[territ] %in% c("SAB","EPN") & land$SppGrp[territ] %in% c("PET","BOJ","ERS") 
        plant.2 <- initial.forest.comp[territ] %in% c("PET","BOJ","ERS") & land$SppGrp[territ] %in% c("SAB","EPN")
        #print(c(sum(plant.1),sum(plant.2)))
        plant.1a <- plant.1[land$MgmtUnit[territ] == "2751"]
        plant.2a <- plant.2[land$MgmtUnit[territ] == "2751"]
        #print(c(sum(job1b),sum(job2b))) 
        land$SppGrp[initial.forest.comp%in% c("SAB","EPN")] <- initial.forest.comp[initial.forest.comp%in% c("SAB","EPN")]
        }
      #
      ## Natural succession of tree spp at every 40 years starting at Tcomp = 70

        chg.comp.cells <- filter(land, (Age-AgeMatu) %in% seq(40,400,40) & Tcomp>=70) %>% select(cell.id)
        if(length(unlist(chg.comp.cells))>0) {
  #        target.cells <- land[land$cell.id %in% unlist(chg.comp.cells), c("cell.id", "x", "y")]
          buffer <- buffer.mig4(land, unlist(chg.comp.cells), potential.spp)
  #       buffer <- buffer.mig(land, unlist(chg.comp.cells), potential.spp)
          land$SppGrp[land$cell.id %in% unlist(chg.comp.cells)] <- 
            forest.trans(land, unlist(chg.comp.cells), forest.succ, buffer, 
                         suitab, potential.spp, dtype="S", p.failure, age.seed, suboptimal, enfeuil)          
        }

        ## Before August 2020
        ## For those cells that change composition, reset Age at X years before maturity
        ## to account for the fact that a major change in species dominance is
        ## generaly due to significant mortality in the overstory
        # land$Age[(land$SppGrp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] <- 
        #   land$AgeMatu[(land$SppGrp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] - 
        #   sample(seq(10,40,5),1)
      }
      
      ## Now, for each cell that has changed composition (because of natural succession or regeneration
      ## post-distrubance), re-initialize Tcomp
      land$Tcomp[land$SppGrp != initial.forest.comp] <- 0
      land$Age[land$cell.id %in% burnt.cells] <- 0
      land$Age[land$cell.id %in% kill.cells] <- 0
      land$Age[land$cell.id %in% cc.cells] <- 0
      
      land$TSPCut[land$cell.id %in% c(cc.cells,kill.cells,burnt.cells)] <- -(land$AgeMatu/2)
      

      ## Finally, Aging: Increment Time Since Disturbance and Time Last Forest Composition change by time.step 
      land$Age <- land$Age + time.step
      land$TSDist <- land$TSDist + time.step
      land$Tcomp <- land$Tcomp + time.step
      land$TSPCut <- land$TSPCut + time.step
      land$TSF <- land$TSF + time.step      
      land$TSSBW <- land$TSSBW + time.step     
      
      ##################################### TRACKING AND SPATIAL OUTS #####################################
      track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=t+year.ini, 
                                group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
      land$AgeClass <- cut(land$Age, 
                           breaks=breaks, 
                           include.lowest=TRUE, 
                           right=TRUE, labels=tags)
      track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=(t)+year.ini, 
                                                                   group_by(land, MgmtUnit, SppGrp) %>% count(AgeClass)))
      suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
      aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
              group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, 
              med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
      track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=t+year.ini, aux))
      aux <- group_by(fuel.type(land, fuel.types.modif), zone) %>% summarize(x=mean(baseline))
      rm(suitab); rm(aux)

      ## If required, plot maps of DisturbanceType at each time step 
      if(write.sp.outputs){
        MAP <- MASK
        cat("... writing output layers", "\n")
        # nfire <- sum(track.fire$year==t, na.rm=T)
        # sizes <- filter(track.fire, year==t) %>% group_by(swc, fire.id) %>% summarise(ab=aburnt.highintens+aburnt.lowintens)
        # Ignitions' cell.id 
        # igni.id <- burnt.cells[c(1,cumsum(sizes$ab)[1:(nfire-1)]+1)] 
        MAP[!is.na(MASK[])] <- land$DistType*(land$TSDist==time.step)  
        # MAP[igni.id] <- 9
        writeRaster(MAP, paste0(out.path, "/lyr/DistType_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
      }
    } # t
  } # irun
  
  
  cat("... writing outputs", "\n")
  if (processes[1]==1) {
  track.fire.regime[,8] <- round(track.fire.regime[,8], 2)     
  write.table(track.fire.regime[-1,], paste0(out.path, "/FireRegime.txt"), quote=F, row.names=F, sep="\t")  
  track.fires$rem <- track.fires$atarget-track.fires$aburnt
  write.table(track.fires[-1,], paste0(out.path, "/Fires.txt"), quote=F, row.names=F, sep="\t")
  track.fuels[,4:5] <- round(track.fuels[,4:5], 2)
  write.table(track.fuels[-1,], paste0(out.path, "/Fuels.txt"), quote=F, row.names=F, sep="\t")
  }
 
  write.table(track.vol[-1,], paste0(out.path, "/Volume.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.frzone[-1,], paste0(out.path, "/SppByFRZone.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.age.class[-1,], paste0(out.path, "/SppByAgeClass.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.suit.class[-1,], paste0(out.path, "/SuitabilityClasses.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.ccut[-1,], paste0(out.path, "/ClearCut.txt"), quote=F, row.names=F, sep="\t")
  track.spp.ccut <- track.spp.ccut[-1,] 
  write.table(track.spp.ccut, paste0(out.path, "/ClearCutSpp.txt"), quote=F, row.names=F, sep="\t")
  toc()
} 


