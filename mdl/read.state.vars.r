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
    library(foreign)
    library(raster)
    library(tidyverse)
  })
  
  
  ## Load function to look for spp abundance within a neighbour and select a species for the
  ## sites in regeneration
  source("mdl/neighbour.spp.r")
  
  
  ## Model's global parameters
  time.step <- 5
  cell.size <- 2000 # in m
    
  
  ## 1. Read data form the .dbf associated to the .shp layer
  forest.data <- read.dbf(paste0(work.path, "/inputlyrs/dbf/points_2k2_ll.dbf"))
  forest.data$X_COORD <- round(forest.data$X_COORD, 0)
  forest.data$Y_COORD <- round(forest.data$Y_COORD, 0)  
  
  
  ## 2. Build a Raster object from the X and Y coordinates and a dummy variable Z=1 
  ## Fix first cell size (in m)
  ## Note that the points.shp is +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 
  ## But the coordinates of the dbf is in the following projection
  MASK <- rasterFromXYZ(data.frame(forest.data[,2:3], z=1), res=c(cell.size, cell.size), digits=0,
                    crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  

  ## 3. Build a data frame with cell.id, mask values (1, NA) and raster coordinates 
  ## Then join the forest data 
  dta <- data.frame(cell.id = 1:ncell(MASK), mask=MASK[], round(coordinates(MASK),0)) %>%
         left_join(select(forest.data, UNIQUE_ID, X_COORD, Y_COORD, UH_REG2, SDOM2, TECO2, TEMPE, PRECI, 
                          COMPO, TSD, DEP3, EXCLU,UAF18, DOM, MATU, CCBIO), 
                   by=c("x"="X_COORD", "y"="Y_COORD"))
  
  
  ## 4. Build the 'land' data frame with forest data and fire regime zone, bioclimatic domain, 
  ## temperature, precipitation, management unit, species group, time since disturbance,
  ## type of soil (depot), and type of exclusion (plantation / protected) 
  land <- data.frame(cell.id = dta$cell.id,
                     x = dta$x,
                     y = dta$y,
                     FRZone = sub("1000", "D", sub("500", "C", sub("250", "B",  sub("150", "A", dta$UH_REG2)))),
                     BCDomain = ifelse(is.na(substr(dta$DOM,1,1)), NA, paste0("D", substr(dta$DOM,1,1))),
                     Temp = dta$TEMPE,
                     Precip = dta$PRECI,
                     MgmtUnit = dta$UAF18,
                     SppGrp = sub("BOP", "other", sub("ter_co", "NonFor",  sub("EAU", "Water", dta$COMPO))),
                     EcoType = substr(dta$TECO2,0,2),
                     Age = dta$TSD,
                     SoilType = sub("argile", "A", sub("organ", "O", sub("roc", "R", sub("sable", "S", sub("till", "T", sub("aut", "Urb", dta$DEP3)))))),
                     Exclus = dta$EXCLU,
                     AgeMatu = dta$MATU,
                     CCbio = dta$CCBIO )  
  
  
  ## 5. Do some cleaning and customizations at the initial data:
  ## 5.1. Reclassify regenerating stands according to the composition of the neigbhour and
  ## the site's ecological type. 
  land$SppGrp[!is.na(land$SppGrp) & land$SppGrp=="rege"] <- 
              neighour.spp(select(land, "cell.id", "SppGrp", "EcoType", "x", "y"), 
              target.cells=land[!is.na(land$SppGrp) & land$SppGrp=="rege", c("cell.id", "x", "y")],
              radius.neigh=10000, km2.pixel=(cell.size/1000)^2)
  
  ## 5.2. Remove "EcoType" as it is not anymore needed 
  land <- select(land, -EcoType)
  
  ## 5.3. Clean Age to be suitable for the model
  land$Age[land$Age==-99] <- -1
  ## Re-equilibrate the age class distribution of locations with age <= 20 years
  ## to compensate for a lack of precision in the initial values
  ## for regenerating stands (due to the state of forest inventories in Québec)
  land$Age[!is.na(land$Age) & land$Age<=20] <- 
    sample(c(0,5,10,15,20), sum(!is.na(land$Age) & land$Age<=20), replace=T)
  ## Make sure that the age classes are presented in 5-year increments
  land$Age <- round(land$Age/time.step)*time.step
  
  ## 5.4. Modify maturity
  land$AgeMatu[is.na(land$AgeMatu) & land$Temp < -1] <- 80
  land$AgeMatu[is.na(land$AgeMatu) & land$Temp > 1]  <- 60
  land$AgeMatu[is.na(land$AgeMatu)] <- 70
  land$AgeMatu[land$AgeMatu < 60] <- 60
  land$AgeMatu[land$AgeMatu > 100 ] <- 100    
  
  ## 5.5. Assign "T" to missing surficial deposits
  land$SoilType[!is.na(land$SppGrp) & is.na(land$SoilType)] <- "T"
  
  
  ## 6. Initialize other state variables
  ## 6.1. Initalize the Time since last disturbance and the Type of the last disturbance
  ## The origin of any disturbance that may have impacted the study area is known.
  land$TSDist <- NA
  land$DistType <- NA  # chgcompo.id <- 5 (defined in define.scenario.r)
  
  ## 6.2. Create a variable that records the time since the last change in forest composition 
  ## i.e. transition to another dominant forest type.
  ## A cell will be considered potential "source" population for migration and range expansion 
  ## if this period is >= 50 years.
  ## This information is not available in current forest inventories, so it is set at 50 years at t=0
  land$Tcomp <- 50
  
  
  ## 7. Mask cells that are 'Water' or 'Urb' (urban areas, infrastructures or even croplands), 
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
  
  
  ## 8. Assign mean precipitation of the 8 neigh cells to those cells with Precip=-9999
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
  
  
  ## 9. Save the MASK raster in a .rdata 
  MASK[is.na(land$SppGrp)] <- NA
  save(MASK, file="inputlyrs/rdata/mask.rdata")
  
  
  ## 10. Give raster structure to each state variable to be saved in a layer stack 
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
  MASK[] <- land$CCbio; CCbio <- MASK
  sp.input <- list(FRZone=FRZone, BCDomain=BCDomain, MgmtUnit=MgmtUnit,
                   SppGrp=SppGrp, Age=Age, Temp=Temp, Precip=Precip,
                   SoilType=SoilType, Exclus=Exclus, AgeMatu=AgeMatu, CCbio=CCbio) 
  save(sp.input, file="inputlyrs/rdata/sp.input.rdata")
    
  
  ## 11. Keep only cells that are not NA and save the 'land' data frame with all state variables
  land <- land[!is.na(land$SppGrp),]
  save(land, file="inputlyrs/rdata/land.rdata")
  
}


