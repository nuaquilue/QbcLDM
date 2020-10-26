########################################################## 
## Run the LANDSCAPE DYNAMIC MODEL for QU?BEC for multiple
## scenarios with customized parameters
########################################################## 

# Clean the working space
rm(list = ls())

# Set the working directory 

setwd("C:/Users/Mathieu/Desktop/LDM/DevelopMB")

#setwd("C:/Users/boumav/Desktop/QbcLDM")


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
################################################
# Create a scenario with customized parameters
scn.name <- "job1"
define.scenario(scn.name)
# New parameters values 
nrun <- 1
processes <- c(F, F , TRUE, TRUE)  # feux, TBE, coupe totale, coupe partielle 
# climat change: si 0, climat stable. Si 45, scen 4.5. Autres, 8.5
#is.climate.change <- 0
clim.scn <- 45

time.horizon <- 90 # starting in 2010, stable after 2100
fire.rate.increase <- 0.005 # rate of increase per year 
a.priori <- 1  # 0.8 = baisse de 20% a priori - fonds de r?serve
replanif <- 0  # when 1, timber supply calculation is done at each time step to readjust harvest level
# to consider changes in FMU age structure (caused by fire) (a posteriori approach)
persist <- c(1,1,1,1,1) 
target.old.pct <- 0.1 # minimum proportion of mature forest that are set apart 
# for biodiversity conservation purposes
avec.combu <- 1 # prise en compte du combustible lors de la propagation des feux
# also a modifyer of the landcape-level fire regime
enfeuil=0 # Corresponds to the proportion of black spruce stands that will be converted to hardwood 
# after fire (plantation), in order to reduce the fire risk
salvage.rate.FMU <- 0.7  # maximum proportion of mature burned stands that will be salvaged when burned
write.sp.outputs <- 0
enable.succ <- 1
increase.fire <- 1 # if 0, remains stable
feu.stable <- 0
p.failure <- 0.7
lutte <- 0 # 1 = reboisement systématique des peuplements conifériens qui deviennent feuillus
           # suite a une perturbation
diff.prematurite = 0
#timber.supply <- "area.based"

# Write the name of any updated parameter in the following call
dump(c("processes", "time.horizon","clim.scn",
       "nrun","fire.rate.increase","a.priori","salvage.rate.FMU","replanif","persist","avec.combu","write.sp.outputs",
       "enable.succ","target.old.pct","increase.fire","feu.stable","enfeuil","lutte","p.failure","diff.prematurite"), 
     paste0("outputs/", scn.name, "/scn.custom.def.r"))
# Run this scenario (count the time it takes)
system.time(landscape.dyn(scn.name))


# Graphiques superficie totale et volume récolté
library(ggplot2)
library(gridExtra)
library(reshape)
UAF <- 9351
data <- read.table("outputs/job1/SppByAgeClass.txt", header=T)
data1 <- data[!is.na(data$MgmtUnit) & data$MgmtUnit == UAF,]
 x <-  aggregate(n ~ SppGrp + year, data=data1, sum)    
plot1 <- ggplot(x, aes(x=year, y=n, fill=SppGrp)) + 
     geom_area()

data <- read.table("outputs/job1/ClearCutSpp.txt", header=T)
data1 <- data[data$MgmtUnit == UAF,]
x1 <- cast(data1, year~SppGrp, value="spp.ccut" , sum)
x2 <- melt(x1, id=c("year"))
plot2 <- ggplot(x2, aes(x=year, y=value, fill=SppGrp)) + 
  geom_area()

grid.arrange(plot1, plot2, ncol=2)