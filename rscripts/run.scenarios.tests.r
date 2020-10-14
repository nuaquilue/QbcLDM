########################################################## 
## Run the LANDSCAPE DYNAMIC MODEL for QU?BEC for multiple
## scenarios with customized parameters
########################################################## 

# Clean the working space
rm(list = ls())

# Set the working directory 

#setwd("C:/Users/Mathieu/Desktop/LDM/DevelopMB")

setwd("C:/Users/boumav/Desktop/QbcLDM")


# Load the model
source("mdl/define.scenario.r")  
source("mdl/landscape.dyn.r")


################################
##################################
########### 8 SCENARIOS + 2 baselines

#S1 - With plantation of hardwoods, A posteriori ajustement of harvest level,
#     salvage 20% of mature stands (low rate)
#S2 - 

############################# SCN 01 #############################
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
rm(list = c(toremove, 'toremove'))
# Create a scenario with customized parameters
scn.name <- "test1"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
processes <- c(T, T , TRUE, TRUE)  # feux, TBE, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
#is.climate.change <- 0
clim.scn <- NA #45

time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori - fonds de r?serve
replanif <- 1  # when 1, timber supply calculation is done at each time step to readjust harvest level
               # to consider changes in FMU age structure (caused by fire) (a posteriori approach)
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1 # minimum proportion of mature forest that are set apart 
                      # for biodiversity conservation purposes
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
                # also a modifyer of the landcape-level fire regime
enfeuil=0.7 # Corresponds to the proportion of black spruce stands that will be converted to hardwood 
            # after fire (plantation), in order to reduce the fire risk
salvage.rate.FMU <- 0.7  # maximum proportion of mature burned stands that will be salvaged when burned
write.sp.outputs <- 0
enable.succ <- 1
#timber.supply <- "area.based"

# Write the name of any updated parameter in the following call
dump(c("processes", "time.horizon","clim.scn",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","write.sp.outputs",
       "enable.succ","target.old.pct"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))


############################# SCN 02 #############################
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
rm(list = c(toremove, 'toremove'))
# Create a scenario with customized parameters
scn.name <- "test2"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
processes <- c(FALSE, FALSE , TRUE, TRUE)  # feux, TBE, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
#is.climate.change <- 0

time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori - fonds de r?serve
replanif <- 1  # when 1, timber supply calculation is done at each time step to readjust harvest level
# to consider changes in FMU age structure (caused by fire) (a posteriori approach)
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1 # minimum proportion of mature forest that are set apart 
# for biodiversity conservation purposes
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
# also a modifyer of the landcape-level fire regime
enfeuil=0.7 # Corresponds to the proportion of black spruce stands that will be converted to hardwood 
# after fire (plantation), in order to reduce the fire risk
salvage.rate.FMU <- 0.2  # maximum proportion of mature burned stands that will be salvaged when burned
write.sp.outputs <- 0
enable.succ <- 0
timber.supply <- "volume.based"

# Write the name of any updated parameter in the following call
dump(c("processes", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","write.sp.outputs",
       "enable.succ","target.old.pct","timber.supply"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))
