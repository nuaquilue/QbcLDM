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

############################# SCN 01 #############################
################################################
# Create a scenario with customized parameters
scn.name <- "var.c_aug.r_aug"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
processes <- c(T, F , TRUE, TRUE)  # feux, TBE, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
#is.climate.change <- 0
clim.scn <- 45

time.horizon <- 90 # starting in 2010, stable after 2100
a.priori <- 1  # 0.8 = baisse de 20% a priori - fonds de r?serve
replanif <- 0  # when 1, timber supply calculation is done at each time step to readjust harvest level
# to consider changes in FMU age structure (caused by fire) (a posteriori approach)
persist <- c(1,1,1,1,1) # BOJ EPN ERS PET SAB
target.old.pct <- 0.1 # minimum proportion of mature forest that are set apart 
# for biodiversity conservation purposes
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
# also a modifyer of the landcape-level fire regime
enfeuil=1 # Corresponds to the proportion of black spruce stands that will be converted to hardwood 
# after fire (plantation), in order to reduce the fire risk
salvage.rate.FMU <- 1  # maximum proportion of mature burned stands that will be salvaged when burned
write.sp.outputs <- 0
enable.succ <- 1
increase.fire <- 1 # if 0, remains stable
zone.fuel.load <- 1
p.failure <- 0.0 # modification de la probabilité que l'EPN revienne suite à elle-même (en pourcentage)

diff.prematurite = 0
# timber.supply <- "area.based"

wflam <- 1
wwind <- 0.0

# Write the name of any updated parameter in the following call
dump(c("processes", "time.horizon","clim.scn",
       "nrun","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","write.sp.outputs",
       "enable.succ","target.old.pct","increase.fire","enfeuil","p.failure","diff.prematurite",
       "zone.fuel.load","wflam","wwind"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))


###########################################################################
###########################################################################
# Graphiques superficie totale et volume récolté
library(ggplot2)
library(gridExtra)
library(reshape)
UAF <- 9351
data <- read.table("outputs/var.c_aug.r_const/SppByAgeClass.txt", header=T)
data11 <- data[data$SppGrp != "NonFor",]
data1 <- data11[!is.na(data11$MgmtUnit) ,]# & data11$MgmtUnit == UAF
 x <-  aggregate(n ~ SppGrp + year, data=data1, sum)    
plot1 <- ggplot(x, aes(x=year, y=n, fill=SppGrp)) + 
     geom_area()

data <- read.table("outputs/var.c_aug.r_const/ClearCutSpp.txt", header=T)
data1 <- data#[data$MgmtUnit == UAF,] 
x1 <- cast(data1, year~SppGrp, value="spp.ccut" , sum)
x2 <- melt(x1, id=c("year"))
plot2 <- ggplot(x2, aes(x=year, y=value, fill=SppGrp)) + 
  geom_area()

grid.arrange(plot1, plot2, ncol=2)


############################
######## graphiques fire regimes
zone.stat <- "C"
fire.regime.1 <- read.table("outputs/var.c_aug.r_aug/FireRegime.txt", header=T)
fire.regime.2 <- read.table("outputs/var.c_aug.r_const/FireRegime.txt", header=T)
fire.regime.3 <- read.table("outputs/var.c_const.r_const/FireRegime.txt", header=T)
fire.regime.11 <- aggregate(aburnt   ~ year + zone, data=fire.regime.1, mean)  
fire.regime.12 <- aggregate(aburnt   ~ year + zone, data=fire.regime.2, mean) 
fire.regime.13 <- aggregate(aburnt   ~ year + zone, data=fire.regime.3, mean) 
fire.regime.111 <- fire.regime.11[fire.regime.11$zone %in% c(zone.stat),]
fire.regime.121 <- fire.regime.12[fire.regime.12$zone %in% c(zone.stat),]
fire.regime.131 <- fire.regime.13[fire.regime.13$zone %in% c(zone.stat),]
fire.regime.141 <- fire.regime.111
fire.regime.141$aburnt <- mean(fire.regime.131$aburnt)
fire.regime.111$sc <- "sc4"
fire.regime.121$sc <- "sc3"
fire.regime.131$sc <- "sc2"
fire.regime.141$sc <- "sc1"
fig1 <- rbind(fire.regime.111,fire.regime.121,fire.regime.131,fire.regime.141)

ggplot(fig1, aes(x=year, y=aburnt , color=sc)) + 
  + ylim(0, 8000)
  geom_line(size=2)


