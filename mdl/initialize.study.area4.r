#################### WARNING !!!!
## There's still NAs in 'land' data.frame
## matrice_buff is not longer needed

######################################################################################
###  initialize.study.area()
###
###  Description >  Reads the .dbf associated to a shape file that describe the 
###                 spatial inputs of the Landscape Dynamic model.
###
###  Arguments >  
###   lyr : name of the .dbf file
###   neighborhood : number of neighbors (4 or 8) in the neighborhood of a cell
###   plot.raster : if TRUE spatial variables are written in ASCII raster format
###   file.mask.layer : NULL by default, or the customized name of the .rdata file containing the 'MASK' layer 
###   file.land.df : NULL by default, or the customized name of the .rdata file containing the 'land' data frame
###   file.sp.input : NULL by default, or the customized name of the .rdata file containing the 'sp.input' raster stack
###
###  Details > 
###
###  Value >  It saves 5 objects in separated .rdata files.
######################################################################################

initialize.study.area <- function(lyr, neighborhood=4, plot.raster=T, file.mask.layer=NULL,
                                  file.land.df=NULL, file.buff=NULL, file.neighs.ft=NULL, file.neighs.fn=NULL, file.sp.input=NULL){  
  
  library(foreign)
  library(raster)
  library(plyr)
  library(RANN)
  library(reshape2)
  options(warn=-1)
  
  # Function to search for the spp abundance in a neighbour and select a species for the
  # sites in regeneration
  source("mdl/neighbour.spp.r")
  
  # Read data form the .dbf associated to the .shp layer
  data <- read.dbf(paste0(lyr, ".dbf"))
  data$X_COORD <- round(data$X_COORD, 0)
  data$Y_COORD <- round(data$Y_COORD, 0)  # str(data$X_COORD)
  
  # Fix the sieze of the cells (in m)
  cell.size <- 2000
  
  ##### 1. Build a Raster object from the X and Y coordinates where the dummy variable Z=1,
  # The size of the raster cells has to be known
  # table(MASK[])   1   175894; NA: 175086 
  MASK <- rasterFromXYZ(data.frame(data[,2:3], z=1), res=c(cell.size, cell.size), digits=0,
                    crs="+proj=lcc +lat_1=46 +lat_2=60 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 
                    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  

  ##### 2. Build a data frame with the raster.index of the cells and the raster coordinates 
  # dim(id.coord) = 350980 3
  id.coord <- data.frame(cell.indx = 1:ncell(MASK), mask=MASK[], coordinates(MASK))
  id.coord$x <- round(id.coord$x, 0)
  id.coord$y <- round(id.coord$y, 0)
  
  # Merge this complete coordinates data frame, with the quebec data frame
  # dim(aux) = 350980 13
  aux <- merge(id.coord, subset(data, select=c(UNIQUE_ID, X_COORD, Y_COORD, UH_REG2, 
                                 SDOM2, TECO2, TEMPE, PRECI, COMPO, TSD, DEP3, EXCLU,UAF18,DOM,MATU,CCBIO)),
               by.x=c("x", "y"), by.y=c("X_COORD", "Y_COORD"), all.x=T)

  ## exporter un ficher équivalence index et mask pour climat
  # bid <- aux[,1:5] 
  # write.csv(bid, file = "equil.csv")


    # To build a data frame with the variables of interest (describing the land)
  # Fire Regime Zone, BioClimatic Domain, Temperature, Precipitation,
  # Management Unit, Species Group, TSDisturbance, Type of soil (depot),
  # Type of exclusion (plantation / protected) 
  # dim(land) = 350980 13
  land <- data.frame( cell.indx = aux$cell.indx,
                      dta.indx = aux$UNIQUE_ID,
                      #mask = aux$mask,
                      FRZone = sub("1000", "D", sub("500", "C", sub("250", "B",  sub("150", "A", aux$UH_REG2)))),
                      BCDomain = ifelse(is.na(substr(aux$DOM,1,1)), NA, paste0("D", substr(aux$DOM,1,1))),
                      Temp = aux$TEMPE,
                      Precip = aux$PRECI,
                      MgmtUnit = aux$UAF18,
                      SppGrp = sub("BOP", "other", sub("ter_co", "NonFor",  sub("EAU", "Water", aux$COMPO))),
                      EcoType = substr(aux$TECO2,0,2),
                      TSD = aux$TSD,
                      SoilType = sub("argile", "A", sub("organ", "O", sub("roc", "R", sub("sable", "S", sub("till", "T", sub("aut", "Urb", aux$DEP3)))))),
                      Exclus = aux$EXCLU,
                      MATU = aux$MATU,
                      CoordX = aux$x,
                      CoordY = aux$y,
                      CCBIO = aux$CCBIO )  
  
  # Order 'land' data.frame by cell.indx, so we can assign values of land directly to 
  # a raster looking like as MASK
  land <- land[order(land$cell.indx),]
  
  # Reclassification of regenerating stands according to the composition of the neigbhour and
  # the site's ecological type 
  land$SppGrp[!is.na(land$SppGrp) & land$SppGrp=="rege"] <- 
          neighour.spp(microland=land[, c("cell.indx", "SppGrp", "EcoType", "CoordX", "CoordY")], 
          target.cells=land[!is.na(land$SppGrp) & land$SppGrp=="rege", c("cell.indx", "CoordX", "CoordY")],
          radius.neigh=10000, km2.pixel=(cell.size/1000)^2)
  
  # Remove "EcoType" as t isn't needed anymore
  land <- subset(land, select=-EcoType)
  
  # Just modifiy TSD to be suitable for the Landscape Dynamic model
  land$TSD[land$TSD==-99] <- -1
  land$TSD[land$TSD==200] <- 180    
  
  # Those cells with TSD == 0 --> random reclassification to 0 or 5
  land$TSD[!is.na(land$TSD) & land$TSD == 0] <- 
    sample(c(0,5,15,20), length(land$TSD[!is.na(land$TSD) & land$TSD == 0 ]), replace=T)
  # Those cells with TSD > 0 (that is 10, 30, 50, 70, 90, 120, 180) -->
  # random reclassification to TSF -5, +0, +5, +10 
  # To have TSD's values in {0,5, 10, 15, ..., 120, 125, 130, 175, 180, 185, 190}
  land$TSD[!is.na(land$TSD) & land$TSD > 0] <- 
    land$TSD[!is.na(land$TSD) & land$TSD > 0 ] +
    sample(c(-5,0,5,10),length(land$TSD[!is.na(land$TSD) & land$TSD > 0 ] ),replace=T)
  
  # Mask as NAs cells that are 'Water'  on 'Urb' (urban areas, infrastructures or even croplands):
  # table(MASK[]):   1  > 175894; NA >  175086
  # sum(water)   41389
  # sum (soil)   9294
  # So now:   table(MASK[]):   1  > 125211; NA >  225769
  water <- !is.na(land$SppGrp) & land$SppGrp=="Water"
  soil <- !is.na(land$SoilType) & land$SoilType == "Urb"
  MASK[water==T | soil==T] <- NA
  land[water==T | soil==T, -c(which(names(land)=="cell.indx"), which(names(land)=="CoordX"), which(names(land)=="CoordY"))] <- NA
  land$SppGrp <- factor(land$SppGrp)  # to remove the 'water' level (and the 'rege' level too)
  land$SoilType <- factor(land$SoilType)  # to remove the 'urb' level
  
  # Save in .radata the MASK object
  if(is.null(file.mask.layer))
    file.mask.layer <- "mask"  
  save(MASK, file=paste0("inputlyrs/rdata/", file.mask.layer, ".rdata")) 
#image(SppGrp)
  
  #### 3. Give raster structure to each state variable to be saved in a layer stack and plotted
  MASK[] <- land$FRZone; FRZone <- MASK
  MASK[] <- land$BCDomain; BCDomain <- MASK
  MASK[] <- land$MgmtUnit; MgmtUnit <- MASK
  MASK[] <- land$SppGrp; SppGrp <- MASK
  MASK[] <- land$TSD; TSD <- MASK
  MASK[] <- land$Temp; Temp <- MASK
  MASK[] <- land$Precip; Precip <- MASK
  MASK[] <- land$SoilType; SoilType <- MASK
  MASK[] <- land$Exclus; Exclus <- MASK
  MASK[] <- land$MATU; MATU <- MASK
  MASK[] <- land$CCBIO; CCBIO <- MASK
  
  sp.input <- list(FRZone=FRZone, BCDomain=BCDomain, MgmtUnit=MgmtUnit,
                   SppGrp=SppGrp, TSD=TSD, Temp=Temp, Precip=Precip,
                   SoilType=SoilType, Exclus=Exclus, MATU=MATU, CCBIO=CCBIO) 
  if(plot.raster){
    out.path <- "inputlyrs/asc/"
    writeRaster(FRZone, paste0(out.path, "FRZone.asc"), format="ascii", overwrite=T)
    writeRaster(BCDomain, paste0(out.path, "BCDomain.asc"), format="ascii", overwrite=T)
    writeRaster(MgmtUnit, paste0(out.path, "MgmtUnit.asc"), format="ascii", overwrite=T)    
    writeRaster(SppGrp, paste0(out.path, "SppGrp_t0.asc"), format="ascii", overwrite=T)    
    writeRaster(TSD, paste0(out.path, "TSD_t0.asc"), format="ascii", overwrite=T)    
  }
  # Save the spatial inputs list in a .rdata
  if(is.null(file.sp.input))
    file.sp.input <- "sp.input"  
  save(sp.input, file=paste0("inputlyrs/rdata/", file.sp.input, ".rdata")) 
    
  
  #### Only keep the cells whose value is not NA   
  land <- land[!is.na(land$SppGrp),]
  # Save the LAND object
  if(is.null(file.land.df))
    file.land.df <- "land"
  save(land, file=paste0("inputlyrs/rdata/", file.land.df, ".rdata")) 


}


