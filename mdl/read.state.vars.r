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
  
  ## Function to select items not in a vector
  `%notin%` <- Negate(`%in%`)
  
  ## Model's global parameters
  time.step <- 5
  cell.size <- 2000 # in m
    
  
  ## 1. Read data from the .dbf with the most approximative map of the province
  forest.data <- read.dbf(paste0(work.path, "QbcLDM/inputlyrs/dbf/exp_LDM_v1.dbf"))
  
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
  forest.data <- forest.data[-which(a),] %>% select(XY)
  
  
  ## 4. Read data from the dbf with more fields informed and traspass to forest.data
  forest.info <- read.dbf(paste0(work.path, "QbcLDM/inputlyrs/dbf/exp_LDM_v2.dbf")) %>% 
                 mutate(XY = paste0(XCOO, "_", YCOO))
  a <- duplicated(forest.info$XY) 
  forest.info <- forest.info[-which(a),] 
  forest.data <- left_join(forest.data, forest.info, by="XY")
  
  
  ## 5. Build a data frame with cell.id, mask values (1, NA) and raster coordinates 
  ## Then join the forest data 
  dta <- data.frame(cell.id = 1:ncell(MASK), mask=MASK[], round(coordinates(MASK),0)) %>%
         left_join(forest.data, by=c("x"="XCOO", "y"="YCOO")) %>% filter(!is.na(mask))
  
  
  ## 6. Build the 'land' data frame with forest data and fire regime zone, 
  ## temperature, precipitation, management unit, species group, age, maturity age,
  ## type of soil, and type of exclusion (plantation / protected) 
  land <- data.frame(cell.id = dta$cell.id,
                     x = dta$x,
                     y = dta$y,
                     temp = dta$temp,
                     prec = dta$prec,
                     frz = dta$FRZ,
                     eco.type = dta$TYPE_ECO,
                     mgmt.unit = dta$UAwww,
                     spp = dta$GRESS4corr,
                     age = dta$AGE_CORR,
                     age.matu = dta$matu,
                     soil.type = dta$DEP_QLDM,
                     exclus = dta$exclus)  

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
  
  ## 7. Reclassify regenerating stands according to the composition within a 10 km circular neigbhorhood
  ## or the ecological type
  land$spp[!is.na(land$spp) & land$spp=="rege"] <- neighour.spp(land, radius.neigh=10000, cell.size)
  land$spp <- factor(land$spp)      # remove the 'rege' level 
  ## Species: BOJ - Yellow birch
  ##          BOP - White birch
  ##          EPN - Black spruce
  ##          ERS - Sugar maple
  ##          OTH.FEU.N - Other deciduous  boreal                
  ##          OTH.FEU.S - Other deciduous temperate
  ##          OTH.RES.N - Other conifer boreal
  ##          OTH.RES.S - Other conifer temperate
  ##          PET - Trembling aspen
  ##          SAB - Balsam fir

  
  ## 8. Assign mean precipitation and temperature of the 8 neigh cells to those cells with non informed values
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
  
  
  ## 9. Assign most abundant age of the 8 / 24 neigh cells to those cells with non informed values
  zcells <- filter(land, !is.na(spp) & is.na(age))
  for(id in zcells$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells, cell.id==id) %>% select(x,y), searchtype="priority", k=9)
    values <- land$age[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells$age[zcells$cell.id==id] <- names(which.max(table(values)))
  }
  land$age[!is.na(land$spp) & is.na(land$age)] <- zcells$age
  zcells2 <- filter(land, !is.na(land$spp) & is.na(age))
  for(id in zcells2$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells2, cell.id==id) %>% select(x,y), searchtype="priority", k=25)
    values <- land$age[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells2$age[zcells2$cell.id==id] <- names(which.max(table(values)))
  }
  land$age[!is.na(land$spp) & is.na(land$age)] <- zcells2$age
  
  
  ## 10. Assign FRZ to those forest cells without FRZ informed
  zcells <- filter(land, !is.na(spp) & is.na(frz))
  r <- 3
  while(nrow(zcells)>0){
    neighs <- nn2(select(land, x, y), select(zcells, x,y), searchtype="priority", k=r^2)
    values <- matrix(land$frz[neighs$nn.idx], ncol=r^2)
    land$frz[!is.na(land$spp) & is.na(land$frz)] <- apply(values, 1, find.frz)
    zcells <- filter(land, !is.na(spp) & is.na(frz))
    r <- r+2
  }
  
  
  ## 11. Assign "T" to missing soil types
  land$soil.type[!is.na(land$spp) & land$soil.type=="-"] <- "T"
  
  
  ## 12. Initialize other state variables
  ## 12.1. Initalize the 4 time since last disturbance variables
  ## The origin of any disturbance that may have impacted the study area is known.
  land$TSF <- 100
  land$TSSBW <- 100
  land$TSCcut <- land$age
  
  ## 12.2. Create a variable that records the time since the last change in forest composition 
  ## i.e. transition to another dominant forest type.
  ## A cell will be considered potential "source" population for migration and range expansion 
  ## if this period is >= 50 years.
  ## This information is not available in current forest inventories, so it is set at 50 years at t=0
  land$Tcomp <- 50
  
  ## 12.3. Create a variable that records the time since the last partical cut
  ## To all locations be selectable at t=0, assign as time since the last partial cut, half the age of maturity
  land$TSPcut <- (land$age.matu %/% 10)/2*10
  
  
  ## 13. Give raster structure to each state variable to be saved in a layer stack 
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
    
  
  ## 14. Save the MASK raster in a .rdata with NA everywhere species is not informed
  dta <- data.frame(cell.id = 1:ncell(MASK)) %>% left_join(land, by="cell.id")
  MASK[is.na(dta$spp)] <- NA
  levelplot(MASK, margin=FALSE, colorkey=F, par.settings=viridisTheme())
  save(MASK, file="inputlyrs/rdata/mask.rdata")
  
  
  ## 15. Keep only cells that are not NA and save the 'land' data frame with all state variables
  land <- land[!is.na(land$spp),]
  save(land, file="inputlyrs/rdata/land.rdata")
  
}


find.frz <- function(x){
  a <-  names(which.max(table(x)))
  a <- ifelse(is.null(a), NA, a)
  return(a)
}
