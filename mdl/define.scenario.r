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

  ## Output directory (do not never change that, please!)
  out.path <- paste0("outputs/", scn.name)
  
  ## Name of the input data according to the Spatial resolution 
  name.resol <- ""
  
  ## Flags to write spatial and tabular output data
  write.sp.outputs <- TRUE
  write.tbl.outputs <- TRUE
  plot.fires <- FALSE
  
  ## Time lenght (in years) of a model simulation
  time.horizon <- 100
  
  ## Number of runs (i.e. replicas)
  nrun <- 1
  
  ## Whether Climate Change applies
  is.climate.change <- TRUE

  ## Select the natural disturbances affecting the area: 
  # fire, sbw, windthrow, clear cut, partial cut 
  disturb <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  
  ## Fix the recurrence (in years) of each disturbance  
  ## 1. Fire
  fire.step <- 5
  ## 2. SBW
  # sbw.fix <- TRUE     # if FALSE, both recurrence and severity are stochastic (set in landscape.dyn.r)
  # sbw.schedule <- sample(c(30,35,40), size=floor(time.horizon/30), replace=TRUE)
  # sbw.severity <- rep(NULL, length(sbw.schedule))
  ## 3. Wind
  # wind.step <- 20
  ## 4.Clearcut
  cc.step <- 5
  ## 5. Partialcut
  # pc.step <- 5
  
  ## The minimum time step is the lowest common denominator of the above time steps
  min.time.step <- 5
  
  ## FIRE disturbance definitions: distributions of fire regime per FRZone
  fire.regime.file <- "inputfiles/NumFires.txt"
  fire.sizes.file <- "inputfiles/FireSizesEmpiric.txt" 
  NFdistrib <- read.table(fire.regime.file, header = T)
  FSdistrib <- read.table(fire.sizes.file, header = T)
  fire.rate.increase <- 0 # increase in fire frequency over the planning horizon (climate change)
  avec.combu <- 0  # Prise en compte des combustibles lors de la propagation des feux
  fuel.types.modif <- c(0.1,0.4,0.95) # modificateur des probabilités de brulage
                                     # pour les combustibles mauvais, moyens et élevés
  
  ## SPRUCE BUDWORM disturbance definitions: probability of tree mortality following SBW defoliation
  # sbw.mortality <- read.table("inputfiles/SBWmortality.txt", header=T)

  ## WIND disturbance definitions
  # wind.cycle <- read.table("inputfiles/WindCycle.txt", header=T)
  # wind.cycle$target.land <- wind.step/wind.cycle$cycle
  
  ## FOREST MANAGEMENT disturbance definitions
  #age.mat <- 80           # default number of years for a stand to reach maturity (harvestable)
  #age.mat.pc <- 30        # default number of years after a partial cut for a stand to become harvestable again
  target.old.pct <- 0.2    # default target proportion of old (>= mature) forests to maintain inside management units
  diff.prematurite <- 10    # for stands salvaged even if not mature (number of years before maturity)
  salvage.rate.event <- 1  # maximal proportion of burnt mature forests that can be salvaged realistically 
                           # in a given fire event [0,1]
  salvage.rate.FMU <- 1  # maximum proportion of salvaged burnt wood allowed in the timber supply in each FMU [0,1]
  ecocrisis <- FALSE     # presence of economic crises during simulations
  ecocrisis.freq <- 0.0  # proportion of years affected by an economic crisis (between 0 and 1)
  hor.plan <- 22 # time horizon for timber supply calculations (periods of 5 years)
  
  ## Replanning options facing natural disturbances
  a.priori <- 1  # proportion of AAC to harvest (between 0 and 1). Allows the constitution nof a buffer 
                 #for attenuation of nat. disturbance impacts on timber supply fluctuations.
  replanif <- 1  # recalculation of AAC level at each time step (1=yes, 0=no). If no, it is calculated only
                 # once, during the first period
  
  ## VEGETATION DYNAMICS: post-disturbance regeneration and forest succession
  ## Species are PET - Trembling aspen, BOJ - Yellow birch, ERS - Sugar maple, SAB - Balsam fir, EPN - Black spruce
  succ.enable <- 1 # 1 =enable succession after disturbance, 0=composition remains the same
  radius.buff <-  c(75000, 60000, 50000, 50000, 50000) # estimated maximum colonization distance (in m)
  #radius.buff <-  c(15000,12000,10000,10000,10000)  # hypothèse pessimiste
  nb.buff <- c(1,1,1,1,1)     # minimum number of source cells within the colonization distance for
                              # to enable colonization
  persist <- c(1,1,1,1,1)     # it indicates whether we allow the transition probability to remain high locally
                              # (cell level) when the species is outside its optimal climatic condition (1=yes, 0=no)
  age.seed <- 50     # below this stand age, seed production is very low, and regeneration failures are more likely
  p.failure <- 0     # probability of regeneration failure in young (< 50 years) burned stands
  post.fire.reg <- read.table("inputfiles/PostFireReg.txt", header=T)
  post.sbw.reg <- read.table("inputfiles/PostSBWReg.txt", header=T)
  # post.wind.reg <- read.table("inputfiles/PostWindReg.txt", header=T)
  post.harvest.reg <- read.table("inputfiles/PostHarvestReg.txt", header=T)
  forest.succ <- read.table("inputfiles/ForestSucc.txt", header=T)
  ThMeanTemp <- read.table("inputfiles/ThMeanTemp.txt", header=T)  
  ThAnnualPrecip <- read.table("inputfiles/ThAnnualPrecip.txt", header=T)  
  ThSoil <- read.table("inputfiles/ThSoil.txt", header=T)  
  
  #### tolerance for sub optimal conditions
  Subopt <- 0.5
  
  
  # Save all the variables in .r file to be further loaded by landscape.dyn.r
  if(!file.exists(out.path))
    dir.create(file.path(getwd(), out.path), showWarnings = T) 
  dump(ls(), paste0(out.path, "/scn.def.r"))
  
}