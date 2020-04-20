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
  time.horizon <- 2095-2010  # 18 time steps of 5 years, it covers the period 2010-2099.
  year.ini <- 2010
  
  ## Time step
  time.step <- 5
  
  ## Number of runs 
  nrun <- 1
  
  ## Flags to write spatial and tabular output data
  write.sp.outputs <- TRUE
  plot.fires <- FALSE
  
  ## Processes of the model included (TRUE-IN our FALSE-OUT),
  ## Partial cuts and spruce budworm are deactivated in the current version.
  processes <- c(TRUE,  # 1. Fire
                 FALSE, # 2. Spruce budworm
                 FALSE,  # 4. Clear cut
                 FALSE)  # 5. Partial cut
                 
  ## Processes' identificator and recurrence (in years) 
  fire.id <- 1; fire.step <- 5
  sbw.id <- 2; sbw.step <- 30
  cc.id <- 3; cc.step <- 5
  pc.id <- 4; pc.step <- 5
  
  
  ## CLIMATE CHANGE parameters
  clim.scn <- NA # or "rcp45" or "rcp85"
  
  
  ## FIRE parameters: distributions of fire regime per FRZone
  file.num.fires <- "inputfiles/NumFires.txt"
  file.fire.sizes <- "inputfiles/FireSizesEmpiric.txt" 
  fire.rate.increase <- 0.1     # Increase in fire frequency over the planning horizon (climate change)
  fuel.types.modif <- data.frame(type=1:3, baseline=c(0.1, 0.4, 0.95)) 
                     # Flammability (that modify the burnt probability) of the fuel types
                     # If baseline=c(1,1,1) no effect of fuel.types on fire spreading/burning
  wflam <- 0.6 # Weight spp flammability in fire spread rate
  wwind <- 0.4 # Weight wind in fire spread rate
  rpb <- 0.3
  pb.upper.th <- 0.8 # prob.burning >= th --> cell always burns
  pb.lower.th <- 0.1 # prob.burning < th --> cell never burns
  
  ## SPRUCE BUDWORM parameters:  
  sbw.step.fix <- T
  
  
  ## FOREST MANAGEMENT parameters:
  target.old.pct <- 0.2    # default target proportion of old (>= mature) forests to maintain inside management units
  diff.prematurite <- 10   # for stands salvaged even if not mature (number of years before maturity)
  salvage.rate.event <- 1  # maximal proportion of burnt mature forests that can be salvaged realistically 
                           # in a given fire event [0,1]
  salvage.rate.FMU <- 1    # maximum proportion of salvaged burnt wood allowed in the timber supply in each FMU [0,1]
  # ecocrisis <- FALSE       # presence of economic crises during simulations
  # ecocrisis.freq <- 0.0    # proportion of years affected by an economic crisis (between 0 and 1)
  hor.plan <- 22           # time horizon for timber supply calculations (periods of 5 years)
  ## Replanning options facing natural disturbances
  a.priori <- 1  # proportion of AAC to harvest (between 0 and 1). Allows the constitution nof a buffer 
                 #for attenuation of nat. disturbance impacts on timber supply fluctuations.
  replanif <- 1  # recalculation of AAC level at each time step (1=yes, 0=no). If no, it is calculated only
                 # once, during the first period
  
  
  ## VEGETATION DYNAMICS parameters:
  succ.enable <- 1 # 1 =enable succession after disturbance, 0=composition remains the same
  age.seed <- 50     # below this stand age, seed production is very low, and regeneration failures are more likely
  p.failure <- 0     # probability of regeneration failure in young (< 50 years) burned stands
  suboptimal <- 0.5  # tolerance for sub optimal conditions
  post.fire.reg <- read.table("inputfiles/PostFireReg.txt", header=T)
  post.sbw.reg <- read.table("inputfiles/PostSBWReg.txt", header=T)
  post.harvest.reg <- read.table("inputfiles/PostHarvestReg.txt", header=T)
  forest.succ <- read.table("inputfiles/ForestSucc.txt", header=T)
  potential.spp <- read.table("inputfiles/PotentialSpp.txt", header=T)
      # rad.buff is the estimated maximum colonization distance (in m)
      # nb.buff is the minimum number of source cells within the colonization distance to enable colonization
      # persist indicates whether we allow the transition probability to remain high locally
      # (cell level) when the species is outside its optimal climatic condition (1=yes, 0=no)
  temp.suitability <- read.table("inputfiles/ThMeanTemp.txt", header=T)  
  precip.suitability <- read.table("inputfiles/ThAnnualPrecip.txt", header=T)  
  soil.suitability <- read.table("inputfiles/ThSoil.txt", header=T)  
  
  
  # Save all these variables in a .r file to be further loaded by landscape.dyn.r
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}