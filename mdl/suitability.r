######################################################################################
###  suitability()
###
###  Description >  Determines the suitability of a given subset of cells for given
###                 climatic and soil (surficial deposits) conditions. Called in the
###                 regeneration-succession section of landscape.dyn
###
###  Arguments >  
###   land : data frame of the state variables (subset of cells to be examined)      
###   temp.suitability: thresholds of temperature suitability for 5 species
###   precip.suitability: thresholds of precipitation suitability for 5 species
###   soil.suitability: thresholds of soil type suitability for 5 species
###   
###  Details > Called in the succession section of landscape.dyn
###
###  Value > Returns a data frame with cell index and suitability
######################################################################################

suitability <- function(land, temp.suitability, precip.suitability, soil.suitability, suboptimal){
  
  # Matrix to store the suitability thresholds per location and species
  suitab_x <- data.frame(cell.id=land$cell.id, SAB=0, EPN=0, PET=0, BOJ=0, ERS=0, other=0)
  suitab_x2 <- suitab_x
  # Matrix with thresholds
  mtemp <- temp.suitability[,-1]
  mprec <- precip.suitability[,-1]
  msols <- soil.suitability[,-1]
  
  
  # Following function produces three vectors for the 6 species (one
  # vector temp, one for prec, one for soils). For the 'other' species only
  # soil suitability is assessed (not climate).
  # Suitability codes: NA : unsuitable, 1 or 3 : moderate, 2 : high  
  
  for (k in 1:6) {  # 
    suitability_t <- as.numeric(cut(land$Temp, mtemp[k,])) 
    suitability_t2 <- (suitability_t==2)*1+(suitability_t %in% c(1,3) * suboptimal)  
    suitability_t2[is.na(suitability_t2)] <- 0    
    suitability_p <- as.numeric(cut(land$Precip, mprec[k,])) 
    suitability_p2 <- (suitability_p==2)*1+(suitability_p %in% c(1,3) * suboptimal)  
    suitability_p2[is.na(suitability_p2)] <- 0  
    suitability_s <- as.numeric(msols[k,][match(land$SoilType, c("T","O","R","S","A"))]  )
    suitability_s[is.na(suitability_s)] <- 0   # 0 means non forest  
    suitab_x[,k+1]  <- pmin(suitability_s)  
    suitab_x2[,k+1] <- pmin(suitability_t2, suitability_p2)  
    
  }  
  # two output vectors: one for climatic suitability, one for soil suitability
  x <- melt(suitab_x,id=c("cell.id"))
  names(x) <- c("cell.id","PotSpp","SuitSoil")
  x$SuitClim <- melt(suitab_x2,id=c("cell.id"))[,3]
  return(x[order(x$cell.id),])
 
}
