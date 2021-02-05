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
###          'write.tbl.outputs' and 'write.maps' are TRUE respectively.  
######################################################################################

landscape.dyn <- function(scn.name){
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(tictoc)
    library(raster)
    library(RANN)
    # library(dplyr)
    library(tidyverse)
    # options(dplyr.summarise.inform = FALSE)
    require (reshape)
  })
  source("mdl/buffer.mig4.r") 
  source("mdl/forest.transitions.r")  
  source("mdl/fuel.type.r")  
  source("mdl/harvest.area.r")
  source("mdl/harvest.volume.r") 
  source("mdl/sbw.outbreak.r") 
  source("mdl/select.others2.r")
  source("mdl/suitability.r") 
  source("mdl/timber.partial.r") 
  source("mdl/timber.partial.volume.r") 
  source("mdl/timber.volume.r") 
  source("mdl/timber2.r") 
  source("mdl/volume.vec.r")  
  source("mdl/wildfires.r")
  
  tic("  t")
  select <- dplyr::select
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
      source(paste0("outputs/", scn.name, "/scn.custom.def.r"))

  
  ## Set the directory for writing spatial outputs (create if it does not exist yet) 
  if(write.maps){      
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
  ## 0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85  # MATHIEU
  ## 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90  # NÚRIA
  ## lenght(time.seq) is 18
  time.seq <- seq(time.step, time.horizon, time.step) 
  
  
  ## Tracking data.frames 
  breaks <- c(0,20,40,60,80,100,999)
  tags <- c("C10","C30", "C50", "C70", "C90", "OLD")
  track.spp.firezone <- data.frame(run=NA, year=NA, frz=NA, spp=NA, area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, mgmt.unit=NA, spp=NA, age.class=NA, n=NA, area=NA)
  track.suit.class <- data.frame(run=NA, year=NA, bioclim.domain=NA, potential.spp=NA, poor=NA, med=NA, good=NA)
  track.fire.regime <- data.frame(run=NA, year=NA, frz=NA,  target.area=NA, nfires=NA, burnt.area=NA, 
                                  fire.cycle=NA, indx.combust=NA, indx.combust.burnt=NA)
  track.fires <- data.frame(run=NA, year=NA, frz=NA, fire.id=NA, wind=NA, target.size=NA, burnt.size=NA)
  track.target <- data.frame(run=NA, year=NA, frz=NA, br=NA, brvar=NA, brfuel=NA, brclima=NA, target.area=NA)
  track.fuel <- data.frame(run=NA, year=NA, frz=NA, type=NA, pct=NA)
  track.cut <- data.frame(run=NA, year=NA,  mgmt.unit=NA, tot.inc=NA, even.age=NA, a.mat=NA, a.inc.burnt=NA, 
                           a.inc.mat.burnt=NA, a.inc.kill=NA, a.inc.mat.kill=NA, a.reg.fail.ex=NA, a.reg.fail.in=NA,
                           area.salvaged=NA, area.unaff=NA, v.salv=NA, v.unaff=NA, a.pcut=NA, v.pcut=NA)
  track.spp.cut <- data.frame(run=NA, year=NA,  mgmt.unit=NA, spp=NA, spp.ccut=NA, spp.ccut.vol=NA,
                                  spp.pcut=NA, spp.pcut.vol=NA)
  track.vol <- data.frame(run=NA, year=NA,  mgmt.unit=NA, spp=NA, DistType=NA, x=NA)
    
  
  ## Start the simulations
  irun <- 1
  for(irun in 1:nrun){
    
    ## Load dynamic state variables 
    load(file="inputlyrs/rdata/land.rdata")
    
    
    ## Set the scheduling of the processes
    fire.schedule <- seq(time.step, time.horizon, fire.step)
    cc.schedule <- seq(time.step, time.horizon, cc.step)
    pc.schedule <- seq(time.step, time.horizon, pc.step)
    # if(sbw.step.fix) 
    sbw.schedule <- c(5,35,70)
    # else 
    #   sbw.schedule <- sample(c(30,35,40), size=floor(time.horizon/30), replace=TRUE)
    
    
    ## Matrix to save the sustained yield level at time t = 0, after clear.cut has happeened
    ## It will be used when the option "replan" is not activated, 
    ## so recalculation of AAC level is only calculated once, during the first period
    ref.harv.level <- table(land$mgmt.unit)*NA
    
    
    ## Determine the harvest regime for each species:
    ## even aged (1): 95% all conifers, PET, DB
    ## uneven aged (0): 95% deciduous
    ## other (2)
    land <- mutate(land, rndm=runif(nrow(land)))
    land$even <- 2
    land$even[land$spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N") & is.na(land$exclus) & land$rndm<=0.95] <- 1
    land$even[land$spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N") & is.na(land$exclus) & land$rndm>0.95] <- 0
    land$even[land$spp %in% c("BOJ", "ERS", "OTH.FEU.S") & is.na(land$exclus) & land$rndm>0.95] <- 1
    land$even[land$spp %in% c("BOJ", "ERS", "OTH.FEU.S") & is.na(land$exclus) & land$rndm<=0.95] <- 0    

    
    ## Compute the baseline fuel at the fire zone level
    fuels <- fuel.type(land, fuel.types.modif, NA)
    baseline.fuel <- group_by(fuels, frz) %>% summarize(x=mean(baseline))
    
    
    ## Record initial distributions:
    ## Species distribution per fire zone
    track.spp.firezone <- rbind(track.spp.firezone, 
      data.frame(run=irun, year=year.ini, group_by(land, frz, spp) %>% summarize(area=length(cell.id)*km2.pixel)))
    ## Fuel type distribution per fire zone
    zone.size <- group_by(land, frz) %>% summarize(x=length(frz))
    aux <- group_by(fuels, frz, type) %>% summarize(n=length(frz)) %>% 
           left_join(zone.size, by="frz") %>% mutate(pct=n/x) %>% select(-n, -x)
    track.fuel <- rbind(track.fuel, data.frame(run=irun, year=year.ini, aux))
    ## Age classes distribution per species and management unit
    land$age.class <- cut(land$age, breaks=breaks, include.lowest=TRUE, right=TRUE, labels=tags)
    track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=year.ini, 
                           group_by(land, mgmt.unit, spp) %>% count(age.class)) %>% mutate(area=n*km2.pixel))
    ## Suitability classes distribution per bioclim.domain 
    suitab <- suitability(land, temp.suitability, prec.suitability, soil.suitability, suboptimal) 
    aux <- left_join(suitab, select(land, cell.id, bioclim.domain), by="cell.id") %>%
           group_by(bioclim.domain, potential.spp) %>% 
           summarize(poor=sum(suit.clim==0)*km2.pixel, med=sum(suit.clim==0.5)*km2.pixel, good=sum(suit.clim==1)*km2.pixel) 
    track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=0, aux))
    rm(suitab); rm(aux)
    
    ## Start 
    t <- 5  
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      print(paste0("scn: ", scn.name, " - run: ", irun, "/", nrun, " - time: ", year.ini+t-time.step, " to ", t+year.ini))
      
      ## Update climatic variables at each time step if climate change is activated
      ## Column 1 is cell.index, the following columns account for climate in 2000-2004, 2005-2009, 2010-2014, etc.
      ## The last column (temp19) then corresponds to the period 2095-2100
      ## The first time step (t=0) we start at 2010, so the first column to start with is column 4 (temp2)
      if(!is.na(clim.scn) & t < time.horizon){
        # Temp
        aux <- cc.temp[,c(1,3+which(time.seq==t))]
        names(aux) <- c("cell.id", "temp")
        land <- select(land, -temp) %>% left_join(aux, by="cell.id")
        # Clean NAs
        r <- 5
        zcells <- filter(land, !is.na(spp) & is.na(temp) & spp!="NonFor")
        neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
        values <- matrix(land$temp[neighs$nn.idx], ncol=r^2)
        land$temp[!is.na(land$spp) & is.na(land$temp) & land$spp!="NonFor"] <- apply(values, 1, mean, na.rm=T)
        # Precip
        aux <- cc.precip[,c(1,3+which(time.seq==t))]
        names(aux) <- c("cell.id", "prec")
        land <- select(land, -prec) %>% left_join(aux, by="cell.id")
        # Clean NAs
        zcells <- filter(land, !is.na(spp) & is.na(precip) & spp!="NonFor")
        neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
        values <- matrix(land$precip[neighs$nn.idx], ncol=r^2)
        land$precip[!is.na(land$spp) & is.na(land$precip) & land$spp!="NonFor"] <- apply(values, 1, mean, na.rm=T)
      }

      
      ##################################### PROCESSES OF CHANGE #####################################
      ## 1. FIRE
      burnt.cells <- integer() 
      if(is.wildfires & t %in% fire.schedule){
        fire.out <- wildfires(land, fire.regime, fire.sizes, sep.zone, baseline.fuel, fuel.types.modif, pigni.opt, 
                              is.fuel.modifier, is.clima.modifier, is.fuel.firesprd, gcm.sep, clim.scn, km2.pixel, t, ncol(MASK))
        burnt.cells <- fire.out[[1]]
        if(nrow(fire.out[[4]])>0){
          track.target <- rbind(track.target, data.frame(run=irun, year=t+year.ini, fire.out[[2]]))
          track.fire.regime <- rbind(track.fire.regime, data.frame(run=irun, year=t+year.ini, fire.out[[3]]))
          track.fires <- rbind(track.fires, data.frame(run=irun, year=t+year.ini, fire.out[[4]]))
        }
        # Done with fires
        land$tsfire[land$cell.id %in% burnt.cells] <- 0
        fire.schedule <- fire.schedule[-1]          
      }
      
        
      ## 2. SBW (under development)
      kill.cells <- integer()
      if(is.sbw & t %in% sbw.schedule){
        kill.cells <- sbw.outbreak(land, severity=1, km2.pixel)
        # Done with outbreak
        land$tssbw[land$cell.id %in% kill.cells] <- 0
        sbw.schedule <- sbw.schedule[-1]
      }

      ## 3. HARVESTING
      ## 3.1. TIMBER SUPPLY CALCULATION
      ## It is only done during the first period if replanning is not selected, otherwise, each time step
      ## AREA BASED
      if((t==time.seq[1] | replanif==1) & timber.supply=="area.based" & is.clearcut & t %in% cc.schedule){
          TS.CC.area <- timber2(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                                salvage.rate.event, salvage.rate.FMU, ref.harv.level, km2.pixel, t)
          TS.PC.area <- timber.partial(land, hor.plan, km2.pixel, pc.step)  
      }

      ## VOLUME BASED - in development
      if((t==time.seq[1] | replanif==1) & timber.supply=="volume.based" & is.clearcut & t %in% cc.schedule){
        TS.CC.vol <- timber.volume(land, cc.step, target.old.pct, diff.prematurite, hor.plan, a.priori, replan, 
                                   salvage.rate.event, salvage.rate.FMU, harv.level, km2.pixel, t, courbes)
        TS.PC.vol <- timber.partial.volume(land, hor.plan, km2.pixel, pc.step, courbes)
      }
      
      ## 3.2. SELECTION OF HARVESTED CELLS BASED ON TIMBER SUPPLY
      cc.cells <- pc.cells <- integer()
      ## AREA BASED
      if(timber.supply == "area.based" & is.clearcut & t %in% cc.schedule){      
        harv.out <- harvest.area(land, cc.step, diff.prematurite, hor.plan, TS.CC.area, TS.PC.area, salvage.rate.FMU,
                                 salvage.rate.event, km2.pixel, t, p.failure, age.seed, courbes)
        cc.cells <- harv.out[[1]]
        pc.cells <- harv.out[[2]]
        if(nrow(harv.out[[3]])>0){
          track.cut <- rbind(track.cut, data.frame(run=irun, year=t+year.ini, harv.out[[3]]))
          track.spp.cut <- rbind(track.spp.cut, data.frame(run=irun, year=t+year.ini, harv.out[[4]]))
        }
        # Done with clear cuts
        land$tspcut[land$cell.id %in% pc.cells] <- 0
        land$tsccut[land$cell.id %in% cc.cells] <- 0
        cc.schedule <- cc.schedule[-1]  
        pc.schedule <- pc.schedule[-1]  
      }
      
      ## VOLUME BASED 
      if(timber.supply == "volume.based" & is.clearcut & t %in% cc.schedule){
        harv.out <- harvest.vol(land, cc.step, diff.prematurite, hor.plan, TS.CC.vol,TS.PC.vol,
                                salvage.rate.event, harv.level, km2.pixel, t, courbes)
        cc.cells <- harv.out[[1]]
        pc.cells <- harv.out[[2]]
        if(nrow(harv.out[[3]])>0){
          track.cut <- rbind(track.cut, data.frame(run=irun, year=t+year.ini, harv.out[[3]]))
          track.spp.cut <- rbind(track.spp.cut, data.frame(run=irun, year=t+year.ini, harv.out[[4]]))
        }
        # Done with clear cuts
        land$tspcut[land$cell.id %in% pc.cells] <- 0
        land$tsccut[land$cell.id %in% cc.cells] <- 0
        cc.schedule <- cc.schedule[-1]  
        pc.schedule <- pc.schedule[-1]  
      }
      

      ##################################### VEGETATION DYNAMICS #####################################
      ## First of all, save a vector containing initial forest composition for comparison at the end of the for loop
      initial.forest.comp <- land$spp
      if(enable.succ){  
         
        ## Natural regeneration of forest after disturbance depends on the nature of the disturbance, 
        ## the age of the stand at the time the disturbance occurred, and the environmental suitability
        ## according to climate and soils. Compute it:
        suitab <- suitability(land, temp.suitability, prec.suitability, soil.suitability, suboptimal)
      
        ## Regeneration after fire
        if(length(burnt.cells)>0){
          buffer <- buffer.mig4(land, burnt.cells, spp.colonize.persist)
          land$spp[land$cell.id %in% burnt.cells] <- forest.trans(land, burnt.cells, post.fire.reg, buffer, 
                     suitab, spp.colonize.persist, dtype="B", p.failure, age.seed, suboptimal, enfeuil)
        }

        ## Regeneration after sbw outbreak
        if(length(kill.cells)>0){
          buffer <- buffer.mig4(land, kill.cells, spp.colonize.persist)
          land$spp[land$cell.id %in% kill.cells] <- forest.trans(land, kill.cells, post.sbw.reg, buffer, 
                     suitab, spp.colonize.persist, dtype="O", p.failure, age.seed, suboptimal, enfeuil)
        }

        ##  Regeneration after clear-cutting
        if(length(cc.cells)>0){
          buffer <- buffer.mig4(land, cc.cells, spp.colonize.persist)
          land$spp[land$cell.id %in% cc.cells] <- forest.trans(land, cc.cells,post.harvest.reg, buffer, 
                      suitab, spp.colonize.persist, dtype="C", p.failure, age.seed, suboptimal, enfeuil)
        }
      
        ######## maintien forcé de la composition forestière (plantation)
        if(lutte ==1) {
          territ <- !is.na(land$mgmt.unit) 
          # superficie qui passe de feu à res et l'inverse
          plant.1 <- initial.forest.comp[territ] %in% c("SAB","EPN") & land$spp[territ] %in% c("PET","BOJ","ERS") 
          plant.2 <- initial.forest.comp[territ] %in% c("PET","BOJ","ERS") & land$spp[territ] %in% c("SAB","EPN")
          #print(c(sum(plant.1),sum(plant.2)))
          plant.1a <- plant.1[land$mgmt.unit[territ] == "2751"]
          plant.2a <- plant.2[land$mgmt.unit[territ] == "2751"]
          #print(c(sum(job1b),sum(job2b))) 
          land$spp[initial.forest.comp%in% c("SAB","EPN")] <- initial.forest.comp[initial.forest.comp%in% c("SAB","EPN")]
        }
      
        ## Natural succession of tree spp at every 40 years starting at Tcomp = 70
        chg.comp.cells <- filter(land, (age-age.matu) %in% seq(40,400,40) & tscomp>=70) %>% select(cell.id)
        if(length(unlist(chg.comp.cells))>0){
  #        target.cells <- land[land$cell.id %in% unlist(chg.comp.cells), c("cell.id", "x", "y")]
          buffer <- buffer.mig4(land, unlist(chg.comp.cells), spp.colonize.persist)
  #       buffer <- buffer.mig(land, unlist(chg.comp.cells), potential.spp)
          land$spp[land$cell.id %in% unlist(chg.comp.cells)] <- 
            forest.trans(land, unlist(chg.comp.cells), forest.succ, buffer, suitab, 
                         spp.colonize.persist, dtype="S", p.failure, age.seed, suboptimal, enfeuil)          
        }

        ## Before August 2020
        ## For those cells that change composition, reset Age at X years before maturity
        ## to account for the fact that a major change in species dominance is
        ## generaly due to significant mortality in the overstory
        # land$age[(land$spp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] <- 
        #   land$ageMatu[(land$spp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] - 
        #   sample(seq(10,40,5),1)
      }
      
      
      ## Now, for each cell that has changed composition (because of natural succession or regeneration
      ## post-distrubance), re-initialize Tcomp
      land$tscomp[land$spp != initial.forest.comp] <- 0
      land$age[land$cell.id %in% burnt.cells] <- 0
      land$age[land$cell.id %in% kill.cells] <- 0
      land$age[land$cell.id %in% cc.cells] <- 0
      land$tspcut[land$cell.id %in% c(cc.cells,kill.cells,burnt.cells)] <- -(land$age.matu/2)  ## ¿?
      

      ## Finally, Aging: Increment Time Since Disturbance and Time Last Forest Composition change by time.step 
      land$age <- land$age + time.step
      land$tscomp <- land$tscomp + time.step
      land$tsfire <- land$tsfire + time.step      
      land$tssbw <- land$tssbw + time.step     
      land$tsccut <- land$tsccut + time.step
      land$tspcut <- land$tspcut + time.step
      
      
      
      ##################################### TRACKING AND SPATIAL OUTS #####################################
      ## Species distribution per fire zone
      track.spp.firezone <- rbind(track.spp.firezone, data.frame(run=irun, year=t+year.ini, 
                                group_by(land, frz, spp) %>% summarize(area=length(cell.id)*km2.pixel)))
      ## Fuel type distribution per fire zone
      fuels <- fuel.type(land, fuel.types.modif, NA)
      aux <- group_by(fuels, frz, type) %>% summarize(n=length(frz)) %>% 
             left_join(zone.size, by="frz") %>% mutate(pct=n/x) %>% select(-n, -x)
      track.fuel <- rbind(track.fuel, data.frame(run=irun, year=t+year.ini, aux))
      ## Age classes distribution per species and management unit      
      land$ageClass <- cut(land$age, breaks=breaks, include.lowest=TRUE, right=TRUE, labels=tags)
      track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=t+year.ini, 
                              group_by(land, mgmt.unit, spp) %>% count(age.class)) %>%  mutate(area=n*km2.pixel))
      ## Suitability classes distribution per bioclim.domain   
      suitab <- suitability(land, temp.suitability, prec.suitability, soil.suitability, suboptimal) 
      aux <- left_join(suitab, select(land, cell.id, bioclim.domain), by="cell.id") %>%
              group_by(bioclim.domain, potential.spp) %>% summarize(poor=sum(suit.clim==0)*km2.pixel, 
              med=sum(suit.clim==0.5)*km2.pixel, good=sum(suit.clim==1)*km2.pixel) 
      track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=t+year.ini, aux))
      aux <- group_by(fuel.type(land, fuel.types.modif), frz) %>% summarize(x=mean(baseline))
      rm(suitab); rm(aux)
      

      ## If required, plot maps at each time step 
      if(write.maps){
        MAP <- MASK
        cat("... writing output layers", "\n")
        MAP[!is.na(MASK[])] <- land$spp
        writeRaster(MAP, paste0(out.path, "/lyr/spp_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        MAP[!is.na(MASK[])] <- land$age
        writeRaster(MAP, paste0(out.path, "/lyr/Age_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        if(is.clearcut){
          MAP[!is.na(MASK[])] <- land$tsccut
          writeRaster(MAP, paste0(out.path, "/lyr/TSCcut_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)  
        }
        if(is.wildfires){
          MAP[!is.na(MASK[])] <- land$tsfire
          writeRaster(MAP, paste0(out.path, "/lyr/TSF_r", irun, "t", t, ".tif"), format="GTiff", overwrite=T)
        }
      }
      
    } # t
  } # irun
  
  
  cat("... writing outputs", "\n")
  if(is.wildfires){
    write.table(track.target[-1,], paste0(out.path, "/BurntRates.txt"), quote=F, row.names=F, sep="\t")
    track.fire.regime[,8:9] <- round(track.fire.regime[,8:9], 2)     
    write.table(track.fire.regime[-1,], paste0(out.path, "/FireRegime.txt"), quote=F, row.names=F, sep="\t")  
    track.fires$rem <- track.fires$target.size-track.fires$burnt.size
    write.table(track.fires[-1,], paste0(out.path, "/Fires.txt"), quote=F, row.names=F, sep="\t")
  }
  write.table(track.spp.firezone[-1,], paste0(out.path, "/SppByFireZone.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.fuel[-1,], paste0(out.path, "/FuelByFireZone.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.age.class[-1,], paste0(out.path, "/SppByAgeClass.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.suit.class[-1,], paste0(out.path, "/SuitabilityClasses.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.vol[-1,], paste0(out.path, "/Volume.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.cut[-1,], paste0(out.path, "/Cuts.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.cut[-1,], paste0(out.path, "/SppCut.txt"), quote=F, row.names=F, sep="\t")
  toc()
} 

