########################################################## 
## Run the LANDSCAPE DYNAMIC MODEL for QUÉBEC for multiple
## scenarios with customized parameters
########################################################## 

# Clean the working space
rm(list = ls())

# Set the working directory 

setwd("C:/Users/boumav/Desktop/QLandscapeDynamics")

# Load the model
source("mdl/define.scenario.r")  
source("mdl/landscape.dyn5.r")

scens <- c("fireFHarvFMigSpersF","fireFHarvFMigSpersT","fireFHarvFMigLpersF","fireFHarvFMigLpersT",
           "fireFHarvTMigSpersF","fireFHarvTMigSpersT","fireFHarvTMigLpersF","fireFHarvTMigLpersT",
           "fireTHarvFMigSpersF","fireTHarvFMigSpersT","fireTHarvFMigLpersF","fireTHarvFMigLpersT",
           "fireTHarvTMigSpersF","fireTHarvTMigSpersT","fireTHarvTMigLpersF","fireTHarvTMigLpersT")
fire_sc <- c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1)
harv_sc <- c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1)
mig_dist <- c("s","s","l","l","s","s","l","l","s","s","l","l","s","s","l","l")
pers_sc <- c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1)


for(i in 1:16) {

############################# SCN 01 #############################
# Clean all data (but not functions) of the workspace
#fun <- unclass(lsf.str())
#toremove <- setdiff(ls(), fun)
#rm(list = c(toremove, 'toremove'))
# Create a scenario with customized parameters
scn.name <- scens[i]# "fireFHarvFMigSpersF"
define.scenario(scn.name)
# New parameters values 
is.climate.change <- TRUE
time.horizon <- 90 # starting in 2010, stable after 2100
 
nrun <- 10
disturb <- c(fire_sc[i], FALSE, FALSE, harv_sc[i], FALSE)  # feux, TBE, chablis, coupe totale, coupe partielle 

# parametres recupération: pourcentage du feu récupérable(recup_feu), et pourcentage maximal
# de l'approvisionnement qui peut être constitué de bois de feu (recup_UA)
hor.plan <- 22
target.old.pct <- 0.2

# Two factors are considered in the determination of sustainable harvest level: 
# whether there is a recalculation sustainable harvest level at each 
# period (replanification: 1 = oui, 0 = non), and whether a buffer is used to mitigate 
# the impact of random fires (a.priori: between 0 et 1)

# a.priori <- 1
# replanif <- 1
fire.rate.increase <-  0.005 # rate of increase per year 

# maximal colonization distances for each species, according to an optimistic and a pessimistic scenario.

if (mig_dist[i] == "l"){
  radius.buff <-  c(75000,60000,50000,50000,50000) # PET BOJ ERS SAB EPN
} else {
  radius.buff <-  c(15000,12000,10000,10000,10000) # PET BOJ ERS SAB EPN
}
#

# Number of cells required within the maximal colonization distance to enable migration to the cell.
# This number is set to nb.buff=1 for all species in the current scenarios.
nb.buff <- c(1,1,1,1,1) #PET,BOJ,ERS,SAB,EPN
# species persistence: 0 indicates that they do not posess the capacity to persist  
# when climate becomes too warm (relative to their climatic envelopes)
persist <- c(pers_sc[i],pers_sc[i],pers_sc[i],pers_sc[i],pers_sc[i])
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "radius.buff", "time.horizon",
       "nrun","hor.plan","target.old.pct","persist","nb.buff","fire.rate.increase"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)

}

for(i in 1:16) {

 system.time(landscape.dyn(scens[i])) 
}
