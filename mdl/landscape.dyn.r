######################################################################################
###  landscape.dyn()
###
###  Description > Runs the Landscape Dynamics Model. 
###
###  Arguments >
###
###  Details > The current version of the model simulates 1 types of natural disturbance:
###            fire. Spruce budworm outbreaks and windthrow are not simulated 
###            in the current version.
###            Post-disturbance regeneration and forest succession are based on 
###            state transition matrices.
###
###  Value > Tabular and spatial outputs are written in 'out.path' directory if  
###          'write.tbl.outputs' and 'write.sp.outputs' are TRUE respectively.  
######################################################################################

landscape.dyn <- function(scn.name){
  
  ## Load required packages and functions 
  library(raster)  
  library(plyr)
  library(RANN)
  library(reshape2)
  library(SpaDES)  # for 'adj' function
  source("mdl/fire.spread3.r")
  source("mdl/disturbance.fire3.r")
  source("mdl/disturbance.cc.r") 
  source("mdl/disturbance.sbw.r") 
  source("mdl/disturbance.pc.r")   
  source("mdl/buffer.mig4.r")            
  source("mdl/forest.transitions2.r")  
  source("mdl/suitability.r") 
  source("mdl/age.by.spp.r")
  source("mdl/age.by.spp2.r")
  source("mdl/fuel.types.r")
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0("outputs/", scn.name, "/scn.def.r"))
  if(file.exists(paste0("outputs/", scn.name, "/scn.custom.def.r")))
    source(paste0("outputs/", scn.name, "/scn.custom.def.r"))

  ## Set the directory for writing spatial outputs (create if it does not exist yet) 
  if(write.sp.outputs){      
    if(!file.exists(paste0(out.path, "/asc")))
       dir.create(file.path(getwd(), out.path, "/asc"), showWarnings = F) 
  }

  ## Load static spatial variables: climate change scenarios for temperature 
  ## and precipitation at the cell level. When is.climate.change
  ## is false, initial values will be used for the whole simulation.
  if(is.climate.change>0){
    if(is.climate.change ==45) {
     load(file=paste0("inputlyrs/rdata/temp45_ModCan", name.resol, ".rdata"))  
     load(file=paste0("inputlyrs/rdata/precip45_ModCan", name.resol, ".rdata"))    
    } else {
      load(file=paste0("inputlyrs/rdata/temp85_ModCan", name.resol, ".rdata"))  
      load(file=paste0("inputlyrs/rdata/precip85_ModCan", name.resol, ".rdata"))       
    }
    
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
  irun=1   # for testing
  for(irun in 1:nrun){
    ## Load initial dynamic spatial variables
    load(file=paste0("inputlyrs/rdata/sp.input", name.resol, ".rdata"))
    SPECIES <- sp.input[["SppGrp"]]
    rm(sp.input); gc()

        ## Load dynamic state variables at each run
    ## 1. "MASK" raster layer of the study area,
    ## 2. "land" data frame with the state variables, and
    load(file=paste0("inputlyrs/rdata/mask", name.resol, ".rdata"))  
    load(file=paste0("inputlyrs/rdata/land", name.resol, ".rdata"))
     land$MATU[is.na(land$MATU)& land$Temp < -1] <- 80
     land$MATU[is.na(land$MATU)& land$Temp > 1]  <- 60
     land$MATU[is.na(land$MATU)] <- 70
     land$MATU[land$MATU < 60] <- 60
     land$MATU[land$MATU > 100 ] <- 100    
      
    #  "IQS" changé pour "MATU"
     
    land$age.mat <- land$MATU

    # pour le cas d'un calcul avec integration a priori du risque de feu, on crée une matrice 
    # qui contiendra le niveau de récolte à maintenir sur tout l'horizon
     
    ref.harv.level <- table(land$MgmtUnit)*0 
      
    ## Make sure that the age classes are presented in 5-year increments
    land$TSD <- round(land$TSD/min.time.step)*min.time.step
    
    ## Re-equilibrate the age class distribution of locations with age <= 20 years
    ## to compensate for a lack of precision in the initial values
    ## for regenerating stands (due to the state of forest inventories in Québec)
    land$TSD[land$TSD<=20] <- sample(c(5,10,15,20), size=sum(land$TSD<=20), replace=TRUE)

    ## Initalize the TSF, TSW, TSE, TSC, TSPC state variables at -1,
    ## to signal that disturbance of origin is not known for these stands at time t=0
    land$TSF <- land$TSW <- land$TSE <- land$TSC <- -1
    
    ## Initialize time since last partial cut. Partial cuts occur with a 40-year rotation. 
    ## This means that all stands are considered to be available for partial cuts at time t=0.
    ## NOTE: partial cuts are not activated currently, so this variable and the associated 
    ## function have no influence on the results.
    land$TSPC <- 40
    
    ## Time since the last change in forest composition (transition to another dominant forest type) 
    ## The cell can be  considered potential "source" population for migration and range expansion 
    ## if this period is >= 50 years.
    ## This information is not available in current forest inventories, and set at 50 years at t=0.
    
    ## Matrix to save the sustained yield level at time t = 0, after clear.cut has happeened
    ## It will be used when the option "replan" is not activated, 
    ## so recalculation of AAC level is only calculated once, during the first period
    ref.harv.level <- table(land$MgmtUnit)*NA
    
    ## Set the cell resolution in km2
    km2.pixel <- res(MASK)[1] * res(MASK)[2] / 10^6

    ## Copy the schedulings in auxiliar vectors
    aux.fire.schedule <- fire.schedule
    aux.cc.schedule <- cc.schedule
    aux.pc.schedule <- pc.schedule
    aux.sbw.schedule <- sbw.schedule
    
    ### outputs baseline temps = 0
    if(write.tbl.outputs){
      t=0
      suitab <- suitability(subset(land, select=c(cell.indx, Temp, Precip, SoilType)),
                            ThMeanTemp, ThAnnualPrecip, ThSoil,Subopt) 
      cc <- join(land[,c("cell.indx","BCDomain")], suitab, by=c("cell.indx"), type="left", match="all")
      count.suit <- table(list(cc$BCDomain, cc$PotSpp, cc$SuitClim))
      spp.suit <- data.frame(melt(count.suit[,,1]), melt(count.suit[,,2])[,3], melt(count.suit[,,3])[,3])
      names(spp.suit) <- c("BCDomain","Spp","poor","med","good")
      spp.suit[,3:5] <- spp.suit[,3:5]*km2.pixel
      write.table(data.frame(run=irun, time=t, spp.suit), 
                  file = paste0(out.path, "/SuitabilityClasses.txt"),
                  append=!(irun==1 & t==0), quote=FALSE, sep="\t", row.names=FALSE, col.names=(irun==1 & t==0))  
      write.table(age.by.spp(land, km2.pixel, min.time.step, irun, t),
                  file = paste0(out.path, "/AgeBySpp.txt"),
                  append=!(irun==1 & t==0), quote=FALSE, sep="\t", row.names=FALSE, col.names=(irun==1 & t==0)) 
      write.table(age.by.spp2(land, km2.pixel, min.time.step, irun, t),
                  file = paste0(out.path, "/AgeBySpp2.txt"),
                  append=!(irun==1 & t==0), quote=FALSE, sep="\t", row.names=FALSE, col.names=(irun==1 & t==0)) 
    }

    
    fuels <- fuel.type(land, fuel.types.modif, NA)
    baseline.fuel <- group_by(fuels, zone) %>% summarize(x=mean(baseline))
    
    ## Start 
    t <- 0  #t<-5
    for(t in time.seq){
      
      ## Track scenario, replicate and time step
      print(paste0("scn: ", scn.name," - run: ", irun, " - time: ", t, " SysTime: ",    Sys.time()))
      
      ## Update climatic variables at each time step
      ## Column 1 is cell.index, the following columns account for climate in 2000-2004, 2005-2009, 2010-2014, etc.
      ## At t=1 we start at 2010, so the first column to start with is column 4 and then increase at every time.step
      if(is.climate.change  & t < 95){
        land$Temp <- unlist(cc.temp[3+which(time.seq==t)], use.names=FALSE)
        land$Precip <- unlist(cc.precip[3+which(time.seq==t)], use.names=FALSE)
      }
      vec_temp <-land$Temp[land$Temp > -100]
      print(mean(vec_temp))
      vec_prec <-land$Precip[land$Precip > -100]

      ###################################### DISTURBANCES #####################################
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
      
      ## 2. SBW - en développement
      
      if(disturb[2] & t %in% aux.sbw.schedule){
        kill.cells <- disturbance.sbw(land, severity=1, write.tbl.outputs=T,
                                      km2.pixel=1, irun=1, t=0, out.path=NULL, out.overwrite=T)
        # update TSF and mark that this distrubance is done
        land$TSE[land$cell.indx %in% kill.cells] <- 0    
        aux.sbw.schedule <- aux.sbw.schedule[-1]
      } else {
        kill.cells <- integer()
        }
      
      
      ## 3. WIND  - windthrow not included in this version
      wind.cells <- integer() 
      
      ## 4. CLEAR CUTING
      #if(ecocrisis) {
      #  period.crisis <- (runif(1)<ecocrisis.freq)
      #}      else {
      #  period.crisis   <- FALSE
      #}
        
      if(disturb[4] & t %in% aux.cc.schedule){
        cc.cells <- disturbance.cc(subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, 
                                                         TSF,age.mat,TSPC,Temp), (SppGrp!="NonFor" & !is.na(MgmtUnit))), 
                                   cc.step, target.old.pct, diff.prematurite, hor.plan,
                                   a.priori, replanif, salvage.rate.event, salvage.rate.FMU, ref.harv.level, 
                                   write.tbl.outputs, km2.pixel, irun, t, out.path, 
                                   out.overwrite=(irun==1 & t==cc.schedule[1]))
        # update TSC and mark that this distrubance is done  
        land$TSC[land$cell.indx %in% cc.cells] <- 0     
        aux.cc.schedule <- aux.cc.schedule[-1]  
      }       else {
        cc.cells <- integer() 
      }     
      length(cc.cells)
      

 
      # 
      ## 5. PARTIAL CUTING - en développement
      
      ## On considère que la rotation de coupe partielle (temps minimal entre deux coupes partielles) 
      ## correspond à la moitié de l'âge d'admissibilité pour la coupe totale (land$age.mat).
      
      ## TSPC = temps depuis coupe partielle. Une valuer de 0 est assignée lorsque le peuplement 
      ## est affecté par une perturbation sévère
      
      land$TSPC[land$TSD < (land$age.mat/2)] <- 0
      
      if(disturb[5] & t %in% aux.pc.schedule){
        pc.cells <- disturbance.pc(subset(land, select=c(cell.indx, MgmtUnit, SppGrp, Exclus, TSD, TSE, TSF,age.mat,TSPC), (SppGrp!="NonFor" & !is.na(MgmtUnit))), 
                                   cc.step, hor.plan,
                                   write.tbl.outputs, km2.pixel, irun, t, out.path, 
                                   out.overwrite=(irun==1 & t==pc.schedule[1]))
        # update TSPC and mark that this distrubance is done  
        land$TSPC[land$cell.indx %in% pc.cells] <- 0   
        aux.pc.schedule <- aux.pc.schedule[-1] 
      }       else {
        pc.cells <- integer() 
      }  

      # a priori - en développement
      # Lorsque l'option 'a priori' est sélectionnée, le niveau de référence est calculé durant
      # la première période, en appliquant une pénalité (a.priori). Ce niveau de récolte (ref.harv.level)
      # en coupe totale est maintenu tel quel durant les périodes subséquentes.
      
          if (t==5) {
              ref.harv.level    <- table(land$MgmtUnit[land$cell.indx %in% cc.cells])
              if(irun==1) {
                ref.harv.level.cp <- table(land$MgmtUnit[land$cell.indx %in% pc.cells])
                ref <- cbind(ref.harv.level,ref.harv.level.cp)*km2.pixel
                write.table(ref, 
                          file = paste0(out.path, "/InitialHarvestLevel.txt"),
                          quote=FALSE, sep="\t", row.names=TRUE, col.names=TRUE)                 
                }
            } else {
              bid <- table(land$MgmtUnit[land$cell.indx %in% cc.cells])
              ref.harv.level <- pmax(ref.harv.level,bid)
            }      

      ############################# POST-DISTURBANCE REGENERATION #############################
      ## Natural regeneration of forest after disturbance depends on the nature of the disturbance 
      ## and on the age of the stand at the time the disturbance occurred  
      
      ## Assess cell suitability according to climate, soils, and tree species
      suitab <- suitability(subset(land, select=c(cell.indx, Temp, Precip, SoilType)),
                                   ThMeanTemp, ThAnnualPrecip, ThSoil,Subopt) 
      
      # save a vector containing initial forest composition, for comparison at the end of for loop
      vec_compo_init <- land$SppGrp
      
      ## 1. FIRE
      if(disturb[1] & sum(burnt.cells)>0 & succ.enable ){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY", "TSD","Tcomp")], 
                             land[land$cell.indx %in% burnt.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% burnt.cells]  <-  forest.trans(subset(land, select=c(cell.indx, SppGrp), cell.indx %in% burnt.cells), 
                       subset(post.fire.reg, select=-age.class, age.class=="adult"), buffer, suitab, dtype="B",persist, p.failure, age.seed, Subopt,enfeuil)
      }

      ## 2. SBW

      if(disturb[2] & (length(kill.cells)!=0) & succ.enable){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY","TSD", "Tcomp")],
                             land[land$cell.indx %in% kill.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% kill.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp), land$cell.indx %in% kill.cells), 
                                                                  post.sbw.reg, buffer, suitab, dtype="S",persist, p.failure, age.seed, Subopt,enfeuil)
      }

      ## 3. WIND

      ## 4. CLEAR CUTTING
      if(disturb[4] & (length(cc.cells)!=0)& succ.enable){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY","TSD", "Tcomp")],
                             land[land$cell.indx %in% cc.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% cc.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp), land$cell.indx %in% cc.cells), 
                       post.harvest.reg, buffer, suitab, dtype="C",persist, p.failure, age.seed, Subopt,enfeuil)
      }

      
      ############################## AGING and FOREST SUCCESSION ##############################
      ## First update the age of all disturbed cells in this time step (set to 0 TSD):
      ## We don't do that at the end of each disturbance because we need to know the original 
      ## TSD for certain processes, no matter what disturbance has happened that time step
      
      #### POUR LA TORDEUSE: on laisse une période de 10 ans pour la récupération 
      #### (pré-récupération en fait)
      
      kill.cells <- land$cell.indx[land$TSE == 5]  
      
      land$TSD[land$cell.indx %in% c(burnt.cells, kill.cells, wind.cells, cc.cells)] <- 0
          
      ## Aging = increment Time Since Disturbances by min.time.step but upper truncated 
      land$TSD[land$TSD > -1] <- pmin.int(land$TSD[land$TSD > -1] + min.time.step, 250)
      land$TSF[land$TSF > -1] <- pmin.int(land$TSF[land$TSF > -1] + min.time.step, 250)
      land$TSC[land$TSC > -1] <- pmin.int(land$TSC[land$TSC > -1] + min.time.step, 250) 
      land$TSE[land$TSE > -1] <- pmin.int(land$TSE[land$TSE > -1] + min.time.step, 250) 
      land$TSPC[land$TSPC > -1] <- pmin.int(land$TSPC[land$TSPC > -1] + min.time.step, 250) 

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


