######################################################################################
###  define.scenario()
###
###  Description >  Initialize the scenario parameters and the global variables of the 
###   Natural Variability Model.
###
###  Arguments >  
###   scn.name : identificative name of the scenario (string)
###
###  Details > By default, the output directory is ..\outputs\scn.name\ and all the
###   objects are saved in the file scn.def.r.
###
###  Value >  An R script.
######################################################################################

define.scenario <- function(scn.name){

  ## Output directory (do not never change this, please!)
  out.path <- paste0("outputs/", scn.name)

  ## Time lenght in years of a model simulation
  ## 18 time steps of 5 years, it covers the period 2010-2100.
  time.horizon <- 2100-2010  
  year.ini <- 2010
  
  ## Time step
  time.step <- 5
  
  ## Number of runs 
  nrun <- 1
  
  ## Flags to write spatial and tabular output data
  write.maps <- TRUE
  plot.fires <- FALSE
  
  ## Processes of the model included (TRUE-IN our FALSE-OUT),
  is.wildfires <- TRUE
  is.sbw <- FALSE
  is.clearcut <- TRUE
  is.partialcut <- TRUE
                 
  ## Processes  recurrence (in years) 
  fire.step <- cc.step <- pc.step <- time.step
  
  
  ## CLIMATE CHANGE parameters
  clim.scn <- NA # or "rcp45" or "rcp85"
  gcm.sep <- "GCM4_ESM2_10km"
  
  
  ## FIRE parameters
  ## In fire.regime, MFRI and max fire size (in km2); fire size (in km2) distribution per fire zone; 
  ## SEP values and ratios per fire zone; and flammability of fuels per zone and fire size
  fire.regime <-  read.table("inputfiles/FireRegime_Z6.txt", header=T)
  fire.sizes <- read.table("inputfiles/FireSizes_Z6.txt", header=T)
  sep.zone <- read.table("inputfiles/SEPzone_Z6.txt", header=T)
  fuel.types.modif <- read.table("inputfiles/FuelTypeModif_Z6.txt", header=T)
  pigni.opt <- "rand" # Can be "rand" for random fire ignition probability, "static" for a spatial but static 
                      # probability of fire ignition, or "dyn" for a dynamic probability of ignition derived from
                      # a empiric relation between ignitions and environmental / anthropic drivers.
  is.fuel.modifier <- T     # if T, fuels flammability modifies target burnt area
  is.clima.modifier <- T    # if T, SEP ratio modifies target burnt area
  is.fuel.firesprd <- T
  wflam <- 0.7 # Weight spp flammability in fire spread rate
  wwind <- 0.3 # Weight wind in fire spread rate
  rpb <- 0.3
  pb.upper.th <- 0.75 # prob.burning >= th --> cell always burns
  pb.lower.th <- -1 # prob.burning < th --> cell never burns
  
  
  ## SPRUCE BUDWORM parameters:  
  # sbw.step.fix <- T
  
  
  ## FOREST MANAGEMENT parameters:
  target.old.pct <- 0.2    # default target proportion of old (>= mature) forests to maintain inside management units
  diff.prematurite <- 10   # for stands salvaged even if not mature (number of years before maturity)
  salvage.rate.event <- 1  # maximal proportion of burnt mature forests that can be salvaged realistically 
                           # in a given fire event [0,1]
  salvage.rate.FMU <- 1    # maximum proportion of salvaged burnt wood allowed in the timber supply in each FMU [0,1]
  # ecocrisis <- FALSE       # presence of economic crises during simulations
  # ecocrisis.freq <- 0.0    # proportion of years affected by an economic crisis (between 0 and 1)
  # hor.plan <- 22           # time horizon for timber supply calculations (periods of 5 years, 5*22==110)
  hor.plan <- 24           # time horizon for timber supply calculations (periods of 5 years, 5*24==120)
  ## Replanning options facing natural disturbances
  a.priori <- 1  # proportion of AAC to harvest (between 0 and 1). Allows the constitution of a buffer 
                 # for attenuation of natural disturbance impacts on timber supply fluctuations.
  replan <- TRUE  # recalculation of AAC level at each time step (TRUE, FALSE). If no, it is calculated only
                 # once, during the first period
  replanif <- 0  # when 1, timber supply calculation is done at each time step to readjust harvest level
  # to consider changes in FMU age structure (caused by fire) (a posteriori approach)
  timber.supply <- "area.based"
  lutte <- FALSE # TRUE = reboisement systématique des peuplements conifériens qui deviennent feuillus
                 # suite a une perturbation
  
  
  ## VEGETATION DYNAMICS parameters:
  enable.succ <- TRUE # enable natural succession every 40 years (if FLASE, composition remains the same)
  enfeuil <- 0.0
  age.seed <- 40     # below this stand age, seed production is very low, and regeneration failures are more likely
  p.failure <- 0     # probability of regeneration failure in young (< 50 years) burned stands
  suboptimal <- 0.5  # tolerance for sub optimal conditions
  post.fire.reg <- read.table("inputfiles/PostFireRege.txt", header=T)
  post.sbw.reg <- read.table("inputfiles/PostSBWRege.txt", header=T)
  post.harvest.reg <- read.table("inputfiles/PostCutRege.txt", header=T)
  forest.succ <- read.table("inputfiles/ForestSucc.txt", header=T)
  spp.colonize.persist <- read.table("inputfiles/SppColonizePersist.txt", header=T)
      # rad.buff is the estimated maximum colonization distance (in m)
      # nb.buff is the minimum number of source cells within the colonization distance to enable colonization
      # persist indicates whether we allow the transition probability to remain high locally
      # (cell level) when the species is outside its optimal climatic condition (1=yes, 0=no)
  temp.suitability <- read.table("inputfiles/ThMeanTemp.txt", header=T)  
  prec.suitability <- read.table("inputfiles/ThAnnualPrecip.txt", header=T)  
  soil.suitability <- read.table("inputfiles/ThSoil.txt", header=T)  
  courbes <-  read.table("inputfiles/courbes_SI2.txt", header=T) # call yield curves (three site indexes)
  
  # Save all these variables in a .r file to be further loaded by landscape.dyn.r
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}