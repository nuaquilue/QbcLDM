+########################################################## 
## Run the LANDSCAPE DYNAMIC MODEL for QUÉBEC for multiple
## scenarios with customized parameters
########################################################## 

# Clean the working space
rm(list = ls())

# Set the working directory 

setwd("C:/Users/boumav/Desktop/QLD_V2")



# Load the model
source("mdl/define.scenario.r")  
source("mdl/landscape.dyn8.r")


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
scn.name <- "SC1.WPlant.Post.Lsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 10
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori - fonds de réserve
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

# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))

#############################################################
# second scenario
scn.name <- "SC2.Wplant.Prio.Lsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 10
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 0.8  # 0.8 = baisse de 20% a priori
replanif <- 0
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.7
salvage.rate.FMU <- 0.2

# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))

#############################################################
# troisième scenario
scn.name <- "SC3.NoPlant.Post.Lsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 25
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori
replanif <- 1
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.0
salvage.rate.FMU <- 0.2
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))

#############################################################
# quatrième scenario
scn.name <- "SC4.NoPlant.Prio.Lsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 25
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 0.8  # 0.8 = baisse de 20% a priori
replanif <- 0
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.0
salvage.rate.FMU <- 0.2
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))

##########################################################
# cinquième scenario
scn.name <- "SC5.WPlant.Post.Hsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 5
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori
replanif <- 1
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.7
salvage.rate.FMU <- 0.8

# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))


#############################################################
# sixième scenario
scn.name <- "SC6.WPlant.Prio.Hsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 5
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 0.8  # 0.8 = baisse de 20% a priori
replanif <- 0
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.7
salvage.rate.FMU <- 0.8

# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))


#############################################################
# septième scenario
scn.name <- "SC7.NoPlant.Post.Hsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 5
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori
replanif <- 1
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.0
salvage.rate.FMU <- 0.8
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","succ.enable","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
system.time(landscape.dyn(scn.name))

#############################################################
# huitième scenario
scn.name <- "SC8.NoPlant.Prio.Hsalv"
define.scenario(scn.name)
# New parameters values 
nrun <- 5
disturb <- c(TRUE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 0.8  # 0.8 = baisse de 20% a priori
replanif <- 0
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
enfeuil=0.0
salvage.rate.FMU <- 0.8
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))

#############################################################
# BASELINE 1 - TOTAL PROTECTION
scn.name <- "SC9.TotProt"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
disturb <- c(FALSE, FALSE , FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori
replanif <- 1
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
salvage.rate.FMU <- 0.8
enfeuil <- 0.0
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","enfeuil"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))

#############################################################
# BASELINE 2 - No HARVEST
scn.name <- "SC10.NoHarvProt"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
disturb <- c(TRUE, FALSE , FALSE, FALSE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
is.climate.change <- 1
time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori
replanif <- 1
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
salvage.rate.FMU <- 0.8
enfeuil <- 0.0
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "time.horizon",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","enfeuil"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))