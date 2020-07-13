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
    library(foreign)
    library(raster)
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
    
  
  ## 1. Read data form the .dbf associated to the .shp layer
  forest.data <- read.dbf(paste0(work.path, "QbcLDM/inputlyrs/dbf/points_2k2_ll.dbf"))
  forest.data$X_COORD <- round(forest.data$X_COORD, 0)
  forest.data$Y_COORD <- round(forest.data$Y_COORD, 0)  
  
  
  ## 2. Read the new fire regime zones, transform to the .dbf projections and overlap
  ZONES <- readOGR(paste0(work.path, "DataIn/ZonageFeux/2020.06.02/zones_nuria.shp"))
  ZONESp <- spTransform(ZONES, CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  points <- SpatialPoints(forest.data[,2:3],  CRS("+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  aux <- data.frame(points, over(points, ZONESp)) %>% dplyr::select(-x)
  names(aux)[3] <- "FRZ"
  forest.data <- left_join(forest.data, aux,  by = c("X_COORD", "Y_COORD"))
  rm(aux)
    # save(forest.data, file=paste0(work.path, "QbcLDM/inputlyrs/dbf/forest.data.rdata"))
  
  
  ## 3. Build a Raster object from the X and Y coordinates and a dummy variable Z=1 
  ## Fix first cell size (in m)
  ## Note that the points.shp is +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
  ## But the coordinates of the dbf is in the following projection
  MASK <- rasterFromXYZ(data.frame(forest.data[,2:3], z=1), res=c(cell.size, cell.size), digits=0,
                    crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  

  ## 4. Build a data frame with cell.id, mask values (1, NA) and raster coordinates 
  ## Then join the forest data 
  dta <- data.frame(cell.id = 1:ncell(MASK), mask=MASK[], round(coordinates(MASK),0)) %>%
         left_join(dplyr::select(forest.data, UNIQUE_ID, X_COORD, Y_COORD, UH_REG2, TECO2, TEMPE, PRECI, 
                          COMPO, TSD, DEP3, EXCLU, UAF18, MATU, FRZ, DOM), 
                   by=c("x"="X_COORD", "y"="Y_COORD"))
  
  
  ## 5. Build the 'land' data frame with forest data and fire regime zone, bioclimatic domain, 
  ## temperature, precipitation, management unit, species group, time since disturbance,
  ## type of soil (depot), and type of exclusion (plantation / protected) 
  land <- data.frame(cell.id = dta$cell.id,
                     x = dta$x,
                     y = dta$y,
                     FRZone = dta$FRZ,
                     BCDomain = ifelse(is.na(substr(dta$DOM,1,1)), NA, paste0("D", substr(dta$DOM,1,1))),
                     Temp = dta$TEMPE,
                     Precip = dta$PRECI,
                     MgmtUnit = dta$UAF18,
                     SppGrp = sub("BOP", "OthDT", sub("ter_co", "NonFor",  sub("EAU", "Water", dta$COMPO))),
                     EcoType = substr(dta$TECO2,0,2),
                     Age = dta$TSD,
                     SoilType = sub("argile", "A", sub("organ", "O", sub("roc", "R", sub("sable", "S", sub("till", "T", sub("aut", "Urb", dta$DEP3)))))),
                     Exclus = dta$EXCLU,
                     AgeMatu = dta$MATU)  
  
  
  ## 6. Do some cleaning and customizations at the initial data:
  ## 6.1 Reclassify Other category in 4 categories: OthDT, OthDC, OthDB, OthCB according to EcoType
  thesaurus <- data.frame(EcoType=c("FC", "FE", "FO", "LA", "LL", "MA", "ME", "MF", "MJ", "MS", "RB", "RC", "RE", "RP", "RS", "RT", "TO"),
                         other=c("OthDT", "OthDT", "OthDT", "OthDT", "OthDT", "OthCB", "OthCB", "OthDT", "OthDB", "OthCB", "OthCT", "OthCB", "OthCB", "OthCT", "OthCB", "OthCT", "OthCB"))
  land <- left_join(land, thesaurus, by="EcoType")
  levels(land$SppGrp) <- c(levels(land$SppGrp), "OthDT",  "OthCT", "OthDB", "OthCB")
  selection <- !is.na(land$SppGrp) & land$SppGrp=="other"
  land$SppGrp[selection] <- land$other[selection]
  land$SppGrp <- factor(land$SppGrp)      # remove the 'other' level (and the 'rege' level too)
  
  ## 6.2. Reclassify regenerating stands according to the composition of the neigbhour and
  ## the site's ecological type. 
  land$SppGrp[!is.na(land$SppGrp) & land$SppGrp=="rege"] <- 
              neighour.spp(land, target.cells=land[!is.na(land$SppGrp) & land$SppGrp=="rege", c("cell.id", "x", "y")],
              radius.neigh=10000, km2.pixel=(cell.size/1000)^2)
  
  ## 6.3. Remove "EcoType" and "other", as these are not anymore needed 
  land <- select(land, -EcoType, -other)
  
  ## 6.4. Clean Age to be suitable for the model
  land$Age[land$Age==-99] <- -1
  ## Re-equilibrate the age class distribution of locations with age <= 20 years
  ## to compensate for a lack of precision in the initial values of regenerating stands 
  ## (due to the state of forest inventories in Québec)
  ## Do not change age of 3 FMU that already have 5y precision: 2371, 2471, and 2571
  selection <- !is.na(land$Age) & land$Age<=20 & land$MgmtUnit %notin% c(2371, 2471, 2571) 
  land$Age[selection] <-  sample(c(0,5,10,15,20), sum(selection), replace=T)
  ## Randomize ages 30, 50, 70 and 90
  selection <- !is.na(land$Age) & land$Age %in% c(30,50,70,90) & land$MgmtUnit %notin% c(2371, 2471, 2571) 
  land$Age[selection] <-  land$Age[selection] + sample(c(-5,0,5,10), sum(selection), replace=T)
  ## Randomize ages 120 and 200
  selection <- !is.na(land$Age) & land$Age %in% c(120,200) & land$MgmtUnit %notin% c(2371, 2471, 2571) 
  land$Age[selection] <-  land$Age[selection] + sample(c(-10,0,10,20), sum(selection), replace=T)
  ## Make sure that the age classes are presented in 5-year increments
  land$Age <- round(land$Age/time.step)*time.step
  
  ## 6.5. Modify maturity
  land$AgeMatu[is.na(land$AgeMatu) & land$SppGrp %in% c("ERS", "BOJ")] <- 105
  land$AgeMatu[is.na(land$AgeMatu) & land$SppGrp %in% c("PET")] <- 65
  land$AgeMatu[is.na(land$AgeMatu) & land$Temp < -1] <- 80
  land$AgeMatu[is.na(land$AgeMatu) & land$Temp > 1]  <- 65
  land$AgeMatu[is.na(land$AgeMatu)] <- 70
  land$AgeMatu[land$AgeMatu < 60] <- 60
  land$AgeMatu[land$AgeMatu > 100 ] <- 100    
  
  ## 6.6. Assign "T" to missing surficial deposits
  land$SoilType[!is.na(land$SppGrp) & is.na(land$SoilType)] <- "T"
  
  
  ## 7. Initialize other state variables
  ## 7.1. Initalize the Time since last disturbance and the Type of the last disturbance
  ## The origin of any disturbance that may have impacted the study area is known.
  land$TSDist <- 400
  land$DistType <- 0
  
  ## 7.2. Create a variable that records the time since the last change in forest composition 
  ## i.e. transition to another dominant forest type.
  ## A cell will be considered potential "source" population for migration and range expansion 
  ## if this period is >= 50 years.
  ## This information is not available in current forest inventories, so it is set at 50 years at t=0
  land$Tcomp <- 50
  
  ## 7.3. Create a variable that records the time since the last partical cut
  ## To all locations be selectable at t=0, assign as time since the last partial cut, 
  ## half the age of maturity
  land$TPCut <- land$AgeMatu/2
  
  ## 8. Mask cells that are 'Water' or 'Urb' (urban areas, infrastructures or even croplands), 
  ## reset factor's levels, and only eep cells that are not NA
  land[!is.na(land$SppGrp) & land$SppGrp=="Water", -c(1:3)] <- NA
  land[!is.na(land$SoilType) & land$SoilType == "Urb", -c(1:3)] <- NA
  land$SppGrp <- factor(land$SppGrp)      # remove the 'water' level (and the 'rege' level too)
  land$SoilType <- factor(land$SoilType)  # remove the 'urb' level
    ## Species groups are:  BOJ - Yellow birch
    ##                      EPN - Black spruce
    ##                      ERS - Sugar maple
    ##                      NonFor - Non forest
    ##                      other                
    ##                      PET - Trembling aspen
    ##                      SAB - Balsam fir
  
  
  ## 9. Assign mean precipitation of the 8 neigh cells to those cells with Precip=-9999
  z <- -9999
  zcells <- filter(land, !is.na(land$SppGrp) & Temp==z)
  for(id in zcells$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells, cell.id==id) %>% select(x,y), searchtype="priority", k=9)
    values <- land$Temp[neighs$nn.idx]
    values <- values[!is.na(values) & values!=z]
    if(length(values)>0)
      zcells$Temp[zcells$cell.id==id] <- mean(values)
  }
  land$Temp[!is.na(land$SppGrp) & land$Temp==z] <- zcells$Temp
  zcells2 <- filter(land, !is.na(land$SppGrp) & Temp==z)
  for(id in zcells2$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells2, cell.id==id) %>% select(x,y), searchtype="priority", k=25)
    values <- land$Temp[neighs$nn.idx]
    values <- values[!is.na(values) & values!=z]
    if(length(values)>0)
      zcells2$Temp[zcells2$cell.id==id] <- mean(values)
  }
  land$Temp[!is.na(land$SppGrp) & land$Temp==z] <- zcells2$Temp
  zcells3 <- filter(land, !is.na(land$SppGrp) & Temp==z)
  for(id in zcells3$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells3, cell.id==id) %>% select(x,y), searchtype="priority", k=11^2)
    values <- land$Temp[neighs$nn.idx]
    values <- values[!is.na(values) & values!=z]
    if(length(values)>0)
      zcells3$Temp[zcells3$cell.id==id] <- mean(values)
  }
  land$Temp[!is.na(land$SppGrp) & land$Temp==z] <- zcells3$Temp
  zcells4 <- filter(land, !is.na(land$SppGrp) & Temp==z)
  
  
  ## 10. Assign FRZ to those forest cells without FRZ informed
  zcells <- filter(land, !is.na(land$SppGrp) & is.na(FRZone))
  for(id in zcells$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells, cell.id==id) %>% select(x,y), searchtype="priority", k=9)
    values <- land$FRZone[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells$FRZone[zcells$cell.id==id] <- names(which.max(table(values)))
  }
  land$FRZone[!is.na(land$SppGrp) & is.na(land$FRZone)] <- zcells$FRZone
  zcells2 <- filter(land, !is.na(land$SppGrp) & is.na(FRZone))
  for(id in zcells2$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells2, cell.id==id) %>% select(x,y), searchtype="priority", k=25)
    values <- land$FRZone[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells2$FRZone[zcells2$cell.id==id] <- names(which.max(table(values)))
  }
  land$FRZone[!is.na(land$SppGrp) & is.na(land$FRZone)] <- zcells2$FRZone
  zcells3 <- filter(land, !is.na(land$SppGrp) & is.na(FRZone))
  for(id in zcells3$cell.id){
    neighs <- nn2(select(land, x, y), filter(zcells3, cell.id==id) %>% select(x,y), searchtype="priority", k=11^2)
    values <- land$FRZone[neighs$nn.idx]
    values <- values[!is.na(values)]
    if(length(values)>0)
      zcells3$FRZone[zcells3$cell.id==id] <- names(which.max(table(values)))
  }
  land$FRZone[!is.na(land$SppGrp) & is.na(land$FRZone)] <- zcells3$FRZone
  zcells4 <- filter(land, !is.na(land$SppGrp) & is.na(FRZone))
  
  
  ## 11. Save the MASK raster in a .rdata 
  MASK[is.na(land$SppGrp)] <- NA
  save(MASK, file="inputlyrs/rdata/mask.rdata")
  
  
  ## 12. Give raster structure to each state variable to be saved in a layer stack 
  MASK[] <- land$FRZone; FRZone <- MASK
  MASK[] <- land$BCDomain; BCDomain <- MASK
  MASK[] <- land$MgmtUnit; MgmtUnit <- MASK
  MASK[] <- land$SppGrp; SppGrp <- MASK
  MASK[] <- land$Age; Age <- MASK
  MASK[] <- land$Temp; Temp <- MASK
  MASK[] <- land$Precip; Precip <- MASK
  MASK[] <- land$SoilType; SoilType <- MASK
  MASK[] <- land$Exclus; Exclus <- MASK
  MASK[] <- land$AgeMatu; AgeMatu <- MASK
  sp.input <- list(FRZone=FRZone, BCDomain=BCDomain, MgmtUnit=MgmtUnit,
                   SppGrp=SppGrp, Age=Age, Temp=Temp, Precip=Precip,
                   SoilType=SoilType, Exclus=Exclus, AgeMatu=AgeMatu) 
  save(sp.input, file="inputlyrs/rdata/sp.input.rdata")
    
  
  ## 13. Keep only cells that are not NA and save the 'land' data frame with all state variables
  land <- land[!is.na(land$SppGrp),]
  save(land, file="inputlyrs/rdata/land.rdata")
  
}


