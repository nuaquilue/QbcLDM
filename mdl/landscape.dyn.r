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

landscape.dyn <- function(){
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(raster)  
    library(tidyverse)
  })
  source("mdl/fire.spread.r")
  source("mdl/disturbance.fire.r")
  source("mdl/disturbance.cc.r") 
  source("mdl/disturbance.sbw.r") 
  source("mdl/disturbance.pc.r")   
  source("mdl/buffer.mig.r")            
  source("mdl/forest.transitions.r")  
  source("mdl/suitability.r") 
  
  
  ## Load scenario definition (global variables and scenario parameters)
  ## and customized scenario parameters
  source(paste0(out.path, "/scn.def.r"))
  if(file.exists(paste0(out.path, "/scn.custom.def.r")))
      source(paste0(out.path, "/scn.custom.def.r"))

  
  ## Set the directory for writing spatial outputs (create if it does not exist yet) 
  if(write.sp.outputs){      
    if(!file.exists(paste0(out.path, "/asc")))
       dir.create(file.path(getwd(), out.path, "/asc"), showWarnings = F) 
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
  
  
  ## Add baseline flammability for each FuelType
  # fuel.types.baseline <- aggregate(fuel.types(land,fuel.types.modif),  by=list(land$FRZone), FUN=mean)
  land <- left_join(land, fuel.types.modif, by="FuelType")
  
  
  ## Tracking data.frames
  track.spp.frzone <- data.frame(run=NA, year=NA, FRZone=NA, SppGrp=NA, Area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, BCDomain=NA, SppGrp=NA, 
                                    C20=NA, C40=NA, C60=NA, C80=NA, C100=NA, Cold=NA)
  track.suit.class <- data.frame(run=NA, year=NA, BCDomain=NA, PotSpp=NA, poor=NA, med=NA, good=NA)
  
  
  ## Start the simulations
  for(irun in 1:nrun){
    
    ## Load dynamic state variables 
    load(file="inputlyrs/rdata/land.rdata")
    
    ## Set the scehdule of each process
    fire.schedule <- seq(0, time.horizon, fire.step)
    cc.schedule <- seq(0, time.horizon, cc.step)
    pc.schedule <- seq(0, time.horizon, pc.step)
    if(sbw.step.fix)
      sbw.schedule <- seq(sbw.step, time.horizon, sbw.step)
    else
      sbw.schedule <- sample(c(30,35,40), size=floor(time.horizon/30), replace=TRUE)

    
    ## Record species distribution at time = 0 per fire zone, and distribution of age classes per BCDomain
    track.spp.frzone <- rbind(track.spp.frzone, data.frame(run=irun, year=year.ini, 
                              group_by(land, FRZone, SppGrp) %>% summarize(Area=length(cell.id)*km2.pixel)))
    track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=year.ini, 
                              group_by(land, BCDomain, SppGrp) %>% summarize(C20=sum(Age<=20)*km2.pixel, 
                              C40=sum(Age<=40)*km2.pixel, C60=sum(Age<=60)*km2.pixel, C80=sum(Age<=80)*km2.pixel, 
                              C100=sum(Age<=100)*km2.pixel, Cold=sum(Age>100)*km2.pixel)))
    ## Record suitability classes per BCDomain at time = 0
    suitab <- suitability(land, temp.suitability, precip.suitability, soil.suitability, suboptimal) 
    aux <- left_join(suitab, select(land, cell.id, BCDomain), by="cell.id") %>%
           group_by(BCDomain, PotSpp) %>% summarize(poor=sum(SuitClim==0)*km2.pixel, 
                                                    med=sum(SuitClim==0.5)*km2.pixel, good=sum(SuitClim==1)*km2.pixel) 
    track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=year.ini, aux))
      

    # # pour le cas d'un calcul avec integration a priori du risque de feu, on cr?e une matrice 
    # # qui contiendra le niveau de r?colte ? maintenir sur tout l'horizon
    # ref.harv.level <- table(land$MgmtUnit)*0 
    
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
      
      
      ## 1. FIRE
      burnt.cells <- integer() 
      if(disturb[fire.id] & t %in% fire.schedule){
        burnt.cells <- disturbance.fire(land, NFdistrib, FSdistrib, fire.step, write.tbl.outputs,
                                        fire.rate.increase, km2.pixel, irun, t, out.path, 
                                        out.overwrite = (irun==1 & t==fire.schedule[1]), plot.fires, 
                                        avec.combu, fuel.types.baseline, fuel.types.modif)
        # Done with fires
        land$TSDist[land$cell.id %in% burnt.cells] <- 0
        land$DistType[land$cell.id %in% burnt.cells] <- fire.id
        fire.schedule <- fire.schedule[-1]          
      }
      
        
      ## 2. SBW (under development)
      kill.cells <- integer()
      if(disturb[sbw.id] & t %in% sbw.schedule){
        kill.cells <- disturbance.sbw(land, severity=1, write.tbl.outputs=T,
                                      km2.pixel=1, irun=1, t=0, out.path=NULL, out.overwrite=T)
        # Done with outbreak
        land$TSDist[land$cell.id %in% kill.cells] <- 0
        land$DistType[land$cell.id %in% kill.cells] <- sbw.id
        sbw.schedule <- sbw.schedule[-1]
      } 
      
      
      ## 3. CLEAR CUTING
      cc.cells <- integer()
      if(disturb[cc.id] & t %in% cc.schedule){
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
      land$TSPC[land$TSD < (land$age.mat/2)] <- 0
      pc.cells <- integer()
      if(disturb[pc.id] & t %in% pc.schedule){
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
                                   ThMeanTemp, ThAnnualPrecip, ThSoil,suboptimal) 
      
      # save a vector containing initial forest composition, for comparison at the end of for loop
      vec_compo_init <- land$SppGrp
      
      ## 1. FIRE
      if(disturb[fire.id] & !is_empty(burnt.cells)){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY", "TSD","Tcomp")], 
                             land[land$cell.indx %in% burnt.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% burnt.cells]  <-  forest.trans(subset(land, select=c(cell.indx, SppGrp), cell.indx %in% burnt.cells), 
                       subset(post.fire.reg, select=-age.class, age.class=="adult"), buffer, suitab, dtype="B",persist, p.failure, age.seed, suboptimal,enfeuil)
      }

      ## 2. SBW
      if(disturb[sbw.ikd] & !is_empty(kill.cells)){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY","TSD", "Tcomp")],
                             land[land$cell.indx %in% kill.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% kill.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp), land$cell.indx %in% kill.cells), 
                                                                  post.sbw.reg, buffer, suitab, dtype="S",persist, p.failure, age.seed, suboptimal,enfeuil)
      }

      ## 4. CLEAR CUTTING
      if(disturb[cc.id] & !is_empty(cc.cells)){
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY","TSD", "Tcomp")],
                             land[land$cell.indx %in% cc.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[land$cell.indx %in% cc.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp), land$cell.indx %in% cc.cells), 
                       post.harvest.reg, buffer, suitab, dtype="C",persist, p.failure, age.seed, suboptimal,enfeuil)
      }

      
      ############################## AGING and FOREST SUCCESSION ##############################
      ## First update the age of all disturbed cells in this time step (set to 0 TSD):
      ## We don't do that at the end of each disturbance because we need to know the original 
      ## TSD for certain processes, no matter what disturbance has happened that time step
      
      #### POUR LA TORDEUSE: on laisse une p?riode de 10 ans pour la r?cup?ration 
      #### (pr?-r?cup?ration en fait)
      
      kill.cells <- land$cell.indx[land$TSE == 5]  
      
      land$TSD[land$cell.indx %in% c(burnt.cells, kill.cells, wind.cells, cc.cells)] <- 0
          
      ## Aging = increment Time Since Disturbances by time.step but upper truncated 
      land$TSD[land$TSD > -1] <- pmin.int(land$TSD[land$TSD > -1] + time.step, 250)
      land$TSF[land$TSF > -1] <- pmin.int(land$TSF[land$TSF > -1] + time.step, 250)
      land$TSC[land$TSC > -1] <- pmin.int(land$TSC[land$TSC > -1] + time.step, 250) 
      land$TSE[land$TSE > -1] <- pmin.int(land$TSE[land$TSE > -1] + time.step, 250) 
      land$TSPC[land$TSPC > -1] <- pmin.int(land$TSPC[land$TSPC > -1] + time.step, 250) 

      ###################
      ## Succession of tree spp at every 40 years starting at TSD = 70
      
      # id cells undergoing succession during time step t
      
      com.chan.cells <- ((land$TSD - land$age.mat) %in% seq(0,300,40)) & (land$Tcomp >= 40)
      
      if(sum(com.chan.cells)>0 & succ.enable) {
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY", "TSD", "Tcomp")],
                             land[com.chan.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[com.chan.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp),
                              com.chan.cells), forest.succ, buffer, suitab, dtype="S", 
                              persist, p.failure, age.seed, suboptimal,enfeuil)
      }
      
      ## For each cell that changed composition, re-initializeTcomp
      land$Tcomp[land$SppGrp != vec_compo_init] <- 0
      land$Tcomp <- land$Tcomp + time.step
      
      ## For each cell that changes composition, reset TSD at x years before maturity
      ## to account for the fact that a major change in species dominance is
      ## generaly due to significant mortality in the overstory
      
      new.age <- land$TSD[com.chan.cells & (land$SppGrp != vec_compo_init)] - 10
      land$TSD[com.chan.cells & (land$SppGrp != vec_compo_init)] <- new.age
      
      ## Update spatial variable accounting for species distribution
      SPECIES[land$cell.indx] <- land$SppGrp
      
        
      ## Spatial outputs
      if(write.sp.outputs &  irun==1  ){ # 
        par(mfrow=c(1,1), oma=c(4, 2, 2, 2),mar=c(1,2,4,0.3))
        #MASK[!is.na(MASK)] <- land$SppGrp
        #writeRaster(MASK, paste0(out.path, "/asc/Spp_", irun, "_", t, ".asc"), format="ascii", overwrite=T)
        #pdf(paste0(out.path, "/asc/JPEG", irun, "_", t, ".pdf"))
        #image(MASK, col=c("blue","darkgreen","red","grey","grey","yellow","palegreen"),
        #         axes=FALSE, ylab='',xlab='') 
        #dev.off()
        #MASK[!is.na(MASK)] <- as.factor(ifelse(land$TSF< 10,2))
        #image(MASK,col=c("grey","red"))
        #writeRaster(MASK, paste0(out.path, "/asc/TSD_", irun, "_", t, ".asc"), format="ascii", overwrite=T)      
        #MASK[!is.na(MASK)] <- as.factor(ifelse(land$TSF == 5,"R","V"))
        #image(MASK,col=c("red","grey"),axes=FALSE, ylab='',xlab='')
        #writeRaster(MASK, paste0(out.path, "/asc/TSD_", irun, "_", t, ".asc"), format="ascii", overwrite=T)  
        #text(2010+t,x=450000,y=200000,cex=2.5)
        #dev.print(pdf, paste0(out.path, "/asc/gif_feu", t/5, ".pdf"))
      }  
      
    } # t
  } # irun
  
  
  cat("... writing outputs", "\n")
  write.table(track.spp.frzone[-1,], paste0(out.path, "/SppByFRZone.txt"), quote=F, row.names=F, sep="\t")
  write.table(track.spp.age.class[-1,], paste0(out.path, "/SppByAgeClass.txt"), quote=F, row.names=F, sep="\t")
  
    # cc <- join(land[,c("cell.indx","BCDomain")], suitab, by=c("cell.indx"), type="left", match="all")
    # count.suit <- table(list(cc$BCDomain, cc$PotSpp, cc$SuitClim))
    # spp.suit <- data.frame(melt(count.suit[,,1]), melt(count.suit[,,2])[,3], melt(count.suit[,,3])[,3])
    # names(spp.suit) <- c("BCDomain","Spp","poor","med","good")
    # spp.suit[,3:5] <- spp.suit[,3:5]*km2.pixel
    # write.table(data.frame(run=irun, time=t, spp.suit), 
    #             file = paste0(out.path, "/SuitabilityClasses.txt"),
    #             append=!(irun==1 & t==0), quote=FALSE, sep="\t", row.names=FALSE, col.names=(irun==1 & t==0))  
    # 
  ####################################### REPORTING #######################################
  ## 1. Area of each species within poor, medium, or good climatic-soil suitability levels
  ## 2. The distribution of Ages Class per domain
  ## 3. The area affected by the disturbances in each Biolcimatic Domain
  
} 


