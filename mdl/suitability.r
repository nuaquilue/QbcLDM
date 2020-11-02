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
###   suboptimal:
###
###  Details > Called in the succession section of landscape.dyn
###
###  Value > Returns a data frame with cell index and suitability
######################################################################################

suitability <- function(land, temp.suitability, precip.suitability, soil.suitability, suboptimal){
  
<<<<<<< HEAD
  ## patch for missing surficial deposits
=======
  # Vector with Potential Species
  PotSpp <- levels(land$SppGrp)
  PotSpp <- c(PotSpp[str_length(PotSpp)==3], "OTH")
>>>>>>> developMB
  
  # Compute soil and climatic suitability per SppGrp  
  # Final suitability corresponds to the minimum value between soil and climate suitability
  dta <- data.frame(cell.id=NA, PotSpp=NA, SuitSoil=NA, SuitClim=NA)
  for(spp in PotSpp){
    th.temp <- filter(temp.suitability, Spp==spp) %>% select(-Spp)
    th.precip <- filter(precip.suitability, Spp==spp) %>% select(-Spp)
    th.soil <- filter(soil.suitability, Spp==spp) %>% select(-Spp)
    aux <- data.frame(cell.id=land$cell.id, PotSpp=spp, temp=land$Temp, precip=land$Precip, soil=land$SoilType) %>%
           mutate(class.temp=as.numeric(cut(temp, th.temp)),
                  class.precip=as.numeric(cut(precip, th.precip)),
                  suit.temp=ifelse(is.na(class.temp), 0, ifelse(class.temp==2, 1, suboptimal)),
                  suit.precip=ifelse(is.na(class.precip), 0, ifelse(class.precip==2, 1, suboptimal)),
                  SuitSoil=as.numeric(th.soil[match(soil, c("T","O","R","S","A"))]),
                  SuitClim=pmin(suit.temp, suit.precip)) %>%
           select(cell.id, PotSpp, SuitSoil, SuitClim)
    dta <- rbind(dta, aux)
  }
  
  ## Upgrade climatic and soil suitability for "other" and "NonFor" SppGrp
  # subland$SuitClim[subland$PotSpp=="other"] <- 1
  dta <- rbind(dta, data.frame(cell.id=land$cell.id, PotSpp="NonFor", SuitSoil=1, SuitClim=1))
  
<<<<<<< HEAD
  for (k in 1:6) {  # 
    suitability_t <- as.numeric(cut(subland$Temp, mtemp[k,])) 
    suitability_t2 <- (suitability_t==2)*1+(suitability_t %in% c(1,3) * Subopt)  
    suitability_t2[is.na(suitability_t2)] <- 0    
    suitability_p <- as.numeric(cut(subland$Precip, mprec[k,])) 
    suitability_p2 <- (suitability_p==2)*1+(suitability_p %in% c(1,3) * Subopt)  
    suitability_p2[is.na(suitability_p2)] <- 0  
    suitability_s <- as.numeric(msols[k,][match(subland$SoilType, c("T","O","R","S","A"))]  )
    suitability_s[is.na(suitability_s)] <- 0   # 0 means non forest  
    suitab_x[,k+1]  <- pmin(suitability_s)  
    suitab_x2[,k+1] <- pmin(suitability_t2, suitability_p2)  
    
  }  
  # two output vectors: one for climatic suitability, one for soil suitability
  x <- melt(suitab_x,id=c("cell.indx"))
  names(x) <- c("cell.indx","PotSpp","SuitSoil")
  x$SuitClim <- melt(suitab_x2,id=c("cell.indx"))[,3]
  return(x[order(x$cell.indx),])
 
}
=======
  # Remove first NA row and order by cell.id
  dta <- dta[-1,]
  dta <- dta[order(dta$cell.id),]
  return(dta)
}
>>>>>>> developMB
