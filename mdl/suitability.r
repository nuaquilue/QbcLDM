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

suitability <- function(land, temp.suitability, prec.suitability, soil.suitability, suboptimal){
  
  # Vector with Potential Species
  potential.spp <- levels(land$spp)
  potential.spp <- c(potential.spp[str_length(potential.spp)==3], "OTH")
  
  # Compute soil and climatic suitability per SppGrp  
  # Final suitability corresponds to the minimum value between soil and climate suitability
  dta <- data.frame(cell.id=NA, potential.spp=NA, suit.soil=NA, suit.clim=NA)
  for(ispp in potential.spp){
    th.temp <- filter(temp.suitability, spp==ispp) 
    th.prec <- filter(prec.suitability, spp==ispp) 
    th.soil <- filter(soil.suitability, spp==ispp) 
    aux <- data.frame(cell.id=land$cell.id, potential.spp=ispp, temp=land$temp, prec=land$prec, soil=land$soil.type) %>%
           mutate(class.temp=as.numeric(cut(temp, th.temp)),
                  class.prec=as.numeric(cut(prec, th.prec)),
                  suit.temp=ifelse(is.na(class.temp), 0, ifelse(class.temp==2, 1, suboptimal)),
                  suit.prec=ifelse(is.na(class.prec), 0, ifelse(class.prec==2, 1, suboptimal)),
                  suit.soil=as.numeric(th.soil[match(soil, c("T","O","R","S","A"))]),
                  suit.clim=pmin(suit.temp, suit.prec)) %>%
           select(cell.id, potential.spp, suit.soil, suit.clim)
    dta <- rbind(dta, aux)
  }
  
  ## Upgrade climatic and soil suitability for "other" and "NonFor" SppGrp
  # subland$SuitClim[subland$PotSpp=="other"] <- 1
  dta <- rbind(dta, data.frame(cell.id=land$cell.id, potential.spp="NonFor", suit.soil=1, suit.clim=1))
  
  # Remove first NA row and order by cell.id
  dta <- dta[-1,]
  dta <- dta[order(dta$cell.id),]
  return(dta)
}