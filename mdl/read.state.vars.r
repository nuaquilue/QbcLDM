######################################################################################
###  read.state.vars()
###
###  Description >  Reads the .dbf associated to a shape file with forest stands data
###                 to initialize the state variables of the Landscape Dynamic model.
###
###  Arguments >  
###     work.path: model's working directory
###
###  Value >  It saves land dataframe, MASK raster and sp.input list in .rdata files.
######################################################################################

read.state.vars <- function(work.path){  
  
  ## Load required packages and functions 
  suppressPackageStartupMessages({
    library(rgdal)
    library(RANN)
    library(foreign)
    library(raster)
    library(rasterVis)
    library(viridis)
    library(tidyverse)
  })
  
  
  ## Load function to look for spp abundance within a neighbour and select a species for the
  ## sites in regeneration
  source("mdl/neighbour.spp.r")
  
  ## Model's global parameters
  time.step <- 5
  cell.size <- 2000 # in m
    
  
  ## 1. Read data from the .dbf 
  forest.data <- read.dbf(paste0(work.path, "QbcLDM/inputlyrs/dbf/exp_LDM_v3.dbf"))
  
  ## 2. Build a Raster object from the X and Y coordinates and a dummy variable Z=1 
  ## Fix first cell size (in m)
  ## Note that the points.shp is +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
  ## But the coordinates of the dbf is in the following projection:
  # crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
  #                   +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  MASK <- rasterFromXYZ(data.frame(forest.data[,1:2], z=1), res=c(cell.size, cell.size), digits=0,
                        crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0
                             +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
     # sum(MASK[], na.rm=T) # 185.479
     # levelplot(MASK, margin=FALSE, colorkey=F, par.settings=viridisTheme())
  
  
  ## 3. Search for, check and remove duplicates
  forest.data$XY <- paste0(forest.data$XCOO,"_",forest.data$YCOO)
  # Remove duplicates
  a <- duplicated(forest.data$XY) 
  # dupli <- forest.data[which(a),]
  # for(i in 1:nrow(dupli)){
  #   r <- filter(forest.data, XCOO==dupli[i,1], YCOO==dupli[i,2])
  #   print(r)
  # }
  forest.data <- forest.data[-which(a),] %>% select(-XY)
  
  
  ## 4. Build a data frame with cell.id, mask values (1, NA) and raster coordinates 
  ## Then join the forest data 
  dta <- data.frame(cell.id = 1:ncell(MASK), mask=MASK[], round(coordinates(MASK),0)) %>%
         left_join(forest.data, by=c("x"="XCOO", "y"="YCOO")) %>% filter(!is.na(mask))
  
  
  ## 5. Build the 'land' data frame with forest data and fire regime zone, 
  ## temperature, precipitation, management unit, species group, age, maturity age,
  ## type of soil, and type of exclusion (plantation / protected) 
  land <- data.frame(cell.id = dta$cell.id,
                     x = dta$x,
                     y = dta$y,
                     temp = dta$temp,
                     prec = dta$prec,
                     frz = ifelse(dta$FRZ=="A", "Z1", ifelse(dta$FRZ=="B" | dta$FRZ=="C" | dta$FRZ=="D", "Z2",
                           ifelse(dta$FRZ=="E" | dta$FRZ=="F" | dta$FRZ=="H" | dta$FRZ=="I" | dta$FRZ=="J", "Z3",
                           ifelse(dta$FRZ=="G", "Z4", ifelse(dta$FRZ=="K", "Z5",  
                           ifelse(dta$FRZ=="L" | dta$FRZ=="M", "Z6", ifelse(dta$FRZ=="Y", "Y",  
                           ifelse(dta$FRZ=="Z", "Z", NA)))))))),
                     eco.type = dta$TYPE_ECO,
                     bioclim.domain = substr(dta$REG_ECO,1,1),
                     mgmt.unit = dta$UAwww,
                     spp = dta$GRESS4corr,
                     age = dta$AGE_CORR,
                     age.matu = dta$matu,
                     soil.type = dta$DEP_QLDM,
                     exclus = ifelse(dta$exclus=="EXC", "EXC", NA))  
  
  ## Reclassify BOP (white birch) as OTH.FEU.N
  land$spp[land$spp=="BOP"] <- "OTH.FEU.N"
  land$spp <- factor(land$spp)      # remove the 'BOP' level 
  
  # # Missing FRZ when spp is informed
  # aux <- select(land, spp, frz)
  # aux$frz[!is.na(aux$frz)] <- "A"
  # aux$frz[is.na(aux$frz) & !is.na(aux$spp)] <- "B"
  # MAP <- MASK
  # MAP[!is.na(MAP[])] <- aux$frz
  # levelplot(MAP, margin=FALSE, colorkey=T, par.settings=viridisTheme())
  
  # # Missing MgmtUnit when spp is informed
  # aux <- select(land, spp, mgmt.unit)
  # aux$mgmt.unit[aux$mgmt.unit!="AGNA"] <- "1171"
  # aux$mgmt.unit[aux$mgmt.unit=="AGNA" & is.na(aux$spp)] <- "2471"
  # aux$mgmt.unit[aux$mgmt.unit=="AGNA" & !is.na(aux$spp)] <- "2661"
  # MAP <- MASK
  # MAP[!is.na(MAP[])] <- aux$mgmt.unit
  # levelplot(MAP, margin=FALSE, colorkey=T, par.settings=viridisTheme())
  
  ## 6. Reclassify regenerating stands according to the composition within a 10 km circular neigbhorhood
  ## or the ecological type
  land$spp[!is.na(land$spp) & land$spp=="rege"] <- neighour.spp(land, radius.neigh=10000, cell.size)
  land$spp <- factor(land$spp)      # remove the 'rege' level 
  ## Species: BOJ - Yellow birch
  ##          EPN - Black spruce
  ##          ERS - Sugar maple
  ##          OTH.FEU.N - Other deciduous  boreal                
  ##          OTH.FEU.S - Other deciduous temperate
  ##          OTH.RES.N - Other conifer boreal
  ##          OTH.RES.S - Other conifer temperate
  ##          PET - Trembling aspen
  ##          SAB - Balsam fir
      # table(land$spp)
      # round(100*table(land$spp)/nrow(land),1)
  
  ## 7. Assign mean precipitation and temperature of the 8 neigh cells to those cells with non informed values
  zcells <- filter(land, !is.na(spp) & is.na(temp) & is.na(prec))
  for(id in zcells$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells, cell.id==id) %>% select(x,y), searchtype="priority", k=9)
    values <- land$temp[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells$temp[zcells$cell.id==id] <- mean(values)
    values <- land$prec[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells$prec[zcells$cell.id==id] <- mean(values)
  }
  land$temp[!is.na(land$spp) & is.na(land$temp)] <- zcells$temp
  land$prec[!is.na(land$spp) & is.na(land$prec)] <- zcells$prec
  

  
  ## 8. Assign FRZ to those forest cells without FRZ informed
  zcells <- filter(land, !is.na(spp) & is.na(frz))
  r <- 3
  while(nrow(zcells)>0){
    neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
    values <- matrix(land$frz[neighs$nn.idx], ncol=r^2)
    land$frz[!is.na(land$spp) & is.na(land$frz)] <- apply(values, 1, find.moda)
    zcells <- filter(land, !is.na(spp) & is.na(frz))
    r <- r+2
  }
  
  
  ## 9. Assign Bioclimatic Domain to those forest cells without it informed
  zcells <- filter(land, !is.na(spp) & is.na(bioclim.domain))
  r <- 3
  while(nrow(zcells)>0){
    neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
    values <- matrix(land$bioclim.domain[neighs$nn.idx], ncol=r^2)
    land$bioclim.domain[!is.na(land$spp) & is.na(land$bioclim.domain)] <- apply(values, 1, find.moda)
    zcells <- filter(land, !is.na(spp) & is.na(bioclim.domain))
    r <- r+2
  }
  
  ## 10. Assign "T" to missing soil types
  land$soil.type[!is.na(land$spp) & land$soil.type=="-"] <- "T"
  
  
  ## 11. Re-equilibrate the age class distribution of locations with age <= 20 years
  ## to compensate for a lack of precision in the initial values of regenerating stands 
  ## (due to the state of forest inventories in Québec)
    selection <- !is.na(land$spp) & land$spp!="NonFor"
    table(land$age[selection])
  ## Age 66 --> 60 or 65
  selection <- !is.na(land$age) & land$age==66; sum(selection)
  land$age[selection] <-  sample(c(60,65), sum(selection), replace=T)
  ## Randomize ages 10, 30, 50, 70 and 90
  selection <- !is.na(land$age) & land$age %in% c(10,30,50,70,90) & land$spp!="NonFor"; sum(selection)
  land$age[selection] <-  land$age[selection] + sample(c(-10,-5,0,5), sum(selection), replace=T)
  ## Randomize ages 120 and 200
  selection <- !is.na(land$age) & land$age==120 & land$spp!="NonFor"; sum(selection)
  land$age[selection] <-  land$age[selection] + sample(c(-15,-10,-5,0), sum(selection), replace=T)

  
  ## 12. Assign most abundant age of the neigbour cells to those cells with non informed values
  zcells <- filter(land, !is.na(spp) & is.na(age) & spp!="NonFor")
  r <- 3
  while(nrow(zcells)>0){
    neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
    values <- matrix(land$age[neighs$nn.idx], ncol=r^2)
    land$age[!is.na(land$spp) & is.na(land$age) & land$spp!="NonFor"] <- apply(values, 1, find.moda)
    zcells <- filter(land, !is.na(spp) & is.na(age) & spp!="NonFor")
    r <- r+2
  }
  land$age <- as.numeric(land$age)
  
  
  ## 13. Initialize other state variables
  ## 13.1. Initalize the 4 time since last disturbance variables
  ## The origin of any disturbance that may have impacted the study area is known.
  land$tsfire <- 100
  land$tssbw <- 100
  land$tsccut <- land$age
  
  ## 13.2. Create a variable that records the time since the last change in forest composition 
  ## i.e. transition to another dominant forest type.
  ## A cell will be considered potential "source" population for migration and range expansion 
  ## if this period is >= 50 years.
  ## This information is not available in current forest inventories, so it is set at 50 years at t=0
  land$tscomp <- 50
  
  ## 13.3. Create a variable that records the time since the last partical cut
  ## To all locations be selectable at t=0, assign as time since the last partial cut, half the age of maturity
  land$tspcut <- (land$age.matu %/% 10)/2*10
  
  
  ## 14. Give raster structure to each state variable to be saved in a layer stack 
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$frz; FRZone <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$mgmt.unit; MgmtUnit <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$spp; SppGrp <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$age; Age <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$temp; Temp <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$prec; Precip <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$soil.type; SoilType <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$exclus; Exclus <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$age.matu; AgeMatu <- MAP
  MAP <- MASK; MAP[!is.na(MAP[])] <- land$eco.type; EcoType <- MAP
  sp.input <- list(FRZone=FRZone, MgmtUnit=MgmtUnit, SppGrp=SppGrp, Age=Age, Temp=Temp, Precip=Precip,
                   SoilType=SoilType, Exclus=Exclus, AgeMatu=AgeMatu, EcoType=EcoType) 
  save(sp.input, file="inputlyrs/rdata/sp.input.rdata")
    
  
  ## 15. Save the MASK raster in a .rdata with NA everywhere species is not informed
  dta <- data.frame(cell.id = 1:ncell(MASK)) %>% left_join(land, by="cell.id")
  MASK[is.na(dta$spp)] <- NA
  levelplot(MASK, margin=FALSE, colorkey=F, par.settings=viridisTheme())
  save(MASK, file="inputlyrs/rdata/mask.rdata")
  
  
  ## 16. Keep only cells that are not NA and save the 'land' data frame with all state variables
  land <- land[!is.na(land$spp),]
  save(land, file="inputlyrs/rdata/land.rdata")
  
}


find.moda <- function(x){
  a <-  names(which.max(table(x)))
  a <- ifelse(is.null(a), NA, a)
  return(a)
}
