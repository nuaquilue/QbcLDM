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

############################# SCN 01 #############################
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
rm(list = c(toremove, 'toremove'))
# Create a scenario with customized parameters
scn.name <- "pres8"
define.scenario(scn.name)
# New parameters values 
is.climate.change <- TRUE
time.horizon <- 90 # starting in 2010, stable after 2100
 
nrun <- 2
disturb <- c(TRUE, FALSE, FALSE, TRUE, TRUE)  # feux, TBE, chablis, coupe totale, coupe partielle 

# parametres recupération: pourcentage du feu récupérable(recup_feu), et pourcentage maximal
# de l'approvisionnement qui peut être constitué de bois de feu (recup_UA)
hor.plan <- 22
target.old.pct <- 0.20

# Deux facteurs à décider: replanification (1 = oui, 0 = non)
# et a.priori (entre 0 et 1)

# a.priori <- 1
# replanif <- 1
fire.rate.increase <-  0.005 # rate of increase per year 

# hypothèses de distance de colonisation

#  distances maximales de colonisation3
radius.buff <-  c(75000,60000,50000,50000,50000) # PET BOJ ERS SAB EPN
#radius.buff <-  c(15000,12000,10000,10000,10000) # PET BOJ ERS SAB EPN

# nombre de cellules nécéssaires pour que la colonisation soit possible
nb.buff <- c(1,1,1,1,1) #PET,BOJ,ERS,SAB,EPN
# persistance des essences: 0 indique qu'elles ne possèdent pas la capacité de 
# persister lorsque le climat devient trop chaud
persist <- c(1,1,1,1,1)
# Write the name of any updated parameter in the following call
dump(c("is.climate.change", "disturb", "radius.buff", "time.horizon",
       "nrun","hor.plan","target.old.pct","persist","nb.buff","fire.rate.increase"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))

#############################
######## run the 16 scenarios. Parameters are defined in the "output" folders
######## corresponding to each scenario.


system.time(landscape.dyn("SCC.AF.post"))
system.time(landscape.dyn("SCC.AF.pri"))
system.time(landscape.dyn("SCC.SF.post"))

system.time(landscape.dyn("CC.SF.post"))
system.time(landscape.dyn("CC.AF.post"))
system.time(landscape.dyn("CC.AF.pri"))

system.time(landscape.dyn("worst.CC.SF.post"))
system.time(landscape.dyn("worst.CC.AF.post"))
system.time(landscape.dyn("worst.CC.AF.pri"))

system.time(landscape.dyn("fireFHarvFMigSpersF"))
system.time(landscape.dyn("feuTcouTbufCpersF"))
system.time(landscape.dyn("feuTcouTbufLpersT"))
system.time(landscape.dyn("feuTcouTbufLpersF"))
system.time(landscape.dyn("feuTcouFbufCpersT"))
system.time(landscape.dyn("feuTcouFbufCpersF"))
system.time(landscape.dyn("feuTcouFbufLpersT"))
system.time(landscape.dyn("feuTcouFbufLpersF"))
system.time(landscape.dyn("feuFcouTbufCpersT"))
system.time(landscape.dyn("feuFcouTbufCpersF"))
system.time(landscape.dyn("feuFcouTbufLpersT"))
system.time(landscape.dyn("feuFcouTbufLpersF"))
system.time(landscape.dyn("feuFcouFbufCpersT"))
system.time(landscape.dyn("feuFcouFbufCpersF"))
system.time(landscape.dyn("feuFcouFbufLpersT"))
system.time(landscape.dyn("feuFcouFbufLpersF"))

system.time(landscape.dyn("feuTcouTbufCpersT"))
system.time(landscape.dyn("feuTcouTbufCpersF"))
system.time(landscape.dyn("feuTcouTbufLpersT"))
system.time(landscape.dyn("feuTcouTbufLpersF"))
system.time(landscape.dyn("feuTcouFbufCpersT"))
system.time(landscape.dyn("feuTcouFbufCpersF"))
system.time(landscape.dyn("feuTcouFbufLpersT"))
system.time(landscape.dyn("feuTcouFbufLpersF"))
system.time(landscape.dyn("feuFcouTbufCpersT"))
system.time(landscape.dyn("feuFcouTbufCpersF"))
system.time(landscape.dyn("feuFcouTbufLpersT"))
system.time(landscape.dyn("feuFcouTbufLpersF"))
system.time(landscape.dyn("feuFcouFbufCpersT"))
system.time(landscape.dyn("feuFcouFbufCpersF"))
system.time(landscape.dyn("feuFcouFbufLpersT"))
system.time(landscape.dyn("feuFcouFbufLpersF"))

#### run the extra scenario mentionned in discussion

system.time(landscape.dyn("barrier"))
