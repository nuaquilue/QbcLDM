######################################################################################
###  landscape.dyn()
###
###  Description > Runs the Landscape Dynamics Model. 
###
###  Arguments >  
###   scn.name : identificative name of the scenario (string)
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
  

  ## Build the time sequence of the processes (shared for all runs). 
  ## Partial cuts, spruce budworm and windthrow are deactivated in the current version.
  time.seq <- seq(min.time.step, max(time.horizon, min.time.step), min.time.step)
  fire.schedule <- seq(fire.step, time.horizon, fire.step)
  cc.schedule <- seq(cc.step, time.horizon, cc.step)
  pc.schedule <- seq(cc.step, time.horizon, cc.step)
  sbw.schedule <- c(5,35,75)
  
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
    
    land$Tcomp <- 50
    
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

    fuel.types.baseline <- aggregate(fuel.types(land,fuel.types.modif),  by=list (land$FRZone), FUN=mean)
    
    ## The discrete time sequence is defined by the min.time.step, 5 years by default    
    t=5  # for testing  t=0
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
      
      if(disturb[1] & t %in% aux.fire.schedule){
        burnt.cells <- disturbance.fire(SPECIES, subset(land, select=c(cell.indx, FRZone,TSD,SppGrp)), 
                                        NFdistrib, FSdistrib, fire.step, write.tbl.outputs,
                                        fire.rate.increase, km2.pixel, irun, t, out.path, 
                                        out.overwrite = (irun==1 & t==fire.schedule[1]), plot.fires, avec.combu,
                                        fuel.types.baseline,fuel.types.modif)
        # update TSF and mark that this distrubance is done
        land$TSF[land$cell.indx %in% burnt.cells] <- 0                      
        aux.fire.schedule <- aux.fire.schedule[-1]          
      }   else {
        burnt.cells <- integer() 
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

      ###################
      ## Succession of tree spp at every 40 years starting at TSD = 70
      
      # id cells undergoing succession during time step t
      
      com.chan.cells <- ((land$TSD - land$age.mat) %in% seq(0,300,40)) & (land$Tcomp >= 40)
      
      if(sum(com.chan.cells)>0 & succ.enable) {
        buffer <- buffer.mig(land[, c("cell.indx", "SppGrp", "CoordX", "CoordY", "TSD", "Tcomp")],
                             land[com.chan.cells, c("cell.indx", "CoordX", "CoordY")], radius.buff, nb.buff)
        land$SppGrp[com.chan.cells] <- forest.trans(subset(land, select=c(cell.indx, SppGrp),
                              com.chan.cells), forest.succ, buffer, suitab, dtype="S", 
                              persist, p.failure, age.seed, Subopt,enfeuil)
      }
      
      ## For each cell that changed composition, re-initializeTcomp
      land$Tcomp[land$SppGrp != vec_compo_init] <- 0
      land$Tcomp <- land$Tcomp + min.time.step
      
      ## For each cell that changes composition, reset TSD at x years before maturity
      ## to account for the fact that a major change in species dominance is
      ## generaly due to significant mortality in the overstory
      
      new.age <- land$TSD[com.chan.cells & (land$SppGrp != vec_compo_init)] - 10
      land$TSD[com.chan.cells & (land$SppGrp != vec_compo_init)] <- new.age
      
      ## Update spatial variable accounting for species distribution
      SPECIES[land$cell.indx] <- land$SppGrp
      
      ####################################### REPORTING #######################################
      ## 1. Area of each species within poor, medium, or good climatic-soil suitability levels
      ## 2. The distribution of Ages Class per domain
      ## 3. The area affected by the disturbances in each Biolcimatic Domain
      if(write.tbl.outputs){
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
} 


