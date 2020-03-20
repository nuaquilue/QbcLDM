######################################################################################
###  forest.trans()
###
###  Description >  Assigns a new tree species according to the post-disturbance 
###                 regeneration or forest succession hypotheses. The function is
###                 called in the regen/succession sections of landscape.dyn
###
###  Arguments >  
###   subland : data frame of the state variables       
###   prob.reg : probability of regeneration of pre-species to post-species
###   buffer = presence of a sufficient number of source populations of a given 
###            tree species in the neighborhood
###   suitability: cell suitability for each species in terms of climate and soil
###   dtype: disturbance type B=burn, C=clearcut, S=succession
###   persist: is it a simulation scenario where species persistence in the face of unsuitable 
###            conditions is allowed
###   p.accid: probability of regeneration failaure in young stands (burned)
###
###
###   Details > It is a generic function that applies any species transition matrix 
###             to any subset of land. 
###
###   Value > Returns a data frame with cell index and new species composition
###          
######################################################################################

#          subland  <- subset(land, select=c(cell.indx, SppGrp), cell.indx %in% burnt.cells)
#          prob.reg <- subset(post.fire.reg, select=-age.class, age.class=="adult")
#
#          subland  <- subset(land, select=c(cell.indx, SppGrp), land$TSC == 0)
#          prob.reg <- post.harvest.reg

forest.trans <- function(subland, prob.reg, buffer, suitab, dtype, persist, p.failure, age.seed, Subopt){
  
  # if empty data.frame
  if(nrow(subland)==0)
    return(numeric())
  
  # reset levels in SppGrp of the 'subland' data.frame so they match with those in
  # the 'land.prob' data.frame
  subland$SppGrp <- factor(subland$SppGrp)

  
  # 1. Join data frames of tree species and probability of spp change    
  # names(land.prob) : "cell.indx" "SppGrp" "Temp"  "Precip" "SoilType"  "PotSpp"  "ptrans" 
  land.prob <- join(subland, prob.reg, by=c("SppGrp"), type="left", match="all")
  # 2. Add buffer results indicating whether the potential species is present in the surrounding neighborhood
  land.prob <- join(land.prob, buffer, by=c("cell.indx","PotSpp"), type="left", match="all")
  
  # 3. Add climatic-soil suitability index (i.e. modifier)
  land.prob <- join(land.prob, suitab[,1:4], by=c("cell.indx","PotSpp"), type="left", match="all")

  # 4. Reset suitability for non species
  if("other" %in% levels(land.prob$PotSpp))
    land.prob$SuitClim[land.prob$PotSpp=="other"] <- 1

  if("NonFor" %in% levels(land.prob$PotSpp)) {
    land.prob$SuitClim[land.prob$PotSpp=="NonFor"] <- 1
    land.prob$SuitSoil[land.prob$PotSpp=="NonFor"] <- 1
     } 
    
    
 
  # Reburning case: if burnt stands too young, probability of successful natural regen is lower
  if(dtype=="B"){  # dtype="B"
      land.prob[land.prob$SppGrp=="EPN" & land.prob$PotSpp=="EPN"& land.prob$TSD<age.seed,]$ptrans <- 
      land.prob[land.prob$SppGrp=="EPN" & land.prob$PotSpp=="EPN"& land.prob$TSD<age.seed,]$ptrans * (1-p.failure)
  }
  
  # Stability criteria: if the species is present in the target location,
  # soil conditions are assumed to be optimal (not limiting)
  
  levels(land.prob$SppGrp) <- levels(land.prob$PotSpp)
  land.prob$PresBuffer[land.prob$SppGrp == land.prob$PotSpp] <- 1
  land.prob$SuitSoil[land.prob$SppGrp == land.prob$PotSpp] <- 1

  
  # Species persistence when climatic conditions become unfavorable: 
  # when persistence is allowed (1), there is a floor probability of self-replacement
  # corresponding to sub-optimal conditions (under the assumption that 
  # competition is more limiting than  physiological response to climate)
  especes <- c("PET","BOJ","ERS","SAB","EPN")
  for (i in 1:5) {
    if (persist[i] == 1) {
    land.prob$SuitClim[(especes[i]==land.prob$SppGrp) & (land.prob$SppGrp == land.prob$PotSpp) 
                       & (land.prob$SuitClim<Subopt)] <- Subopt  
      
  }
  }
  # Final soil suitability corresponds to the minimum value between soil and climate suitability
    
  land.prob$SuitAll <- pmin(land.prob$SuitClim,land.prob$SuitSoil)
  
  # Determine the final succession / regeneration probability 
  land.prob$p <- land.prob$ptrans * land.prob$SuitAll * land.prob$PresBuffer
  
  # Reshape the data frame, so we have a column for each potential species with 
  # the corresponding transition probability (one row per target cell)
  aux  <- dcast(land.prob, formula = cell.indx ~ PotSpp, value.var = "p")
  
  # Now select a new spp according to these probabilities  
  # and assign the corresponing species name
  id.spp <- apply(aux[,2:ncol(aux)], 1, select.spp, nspp=ncol(aux))
  spp.names <- c(names(aux)[2:ncol(aux)], "NULL")  ## NULL name for cases where p=0
  newspp <- spp.names[id.spp]
  
  # If after all filters, p for all PotSpp is 0, the current species remains
  cell.withoutspp <- aux$cell.indx[id.spp==ncol(aux)]
  if(length(cell.withoutspp)>0){
    renewspp <- subland$SppGrp[subland$cell.indx %in% cell.withoutspp]
    newspp[id.spp==ncol(aux)] <- spp.names[renewspp]
  }
 
  # return the vector with the name of the new spp
  return(newspp)

}


select.spp <- function(x, nspp){
  if(sum(x)==0)
    return(c(nspp))
  id.spp <- sample(1:length(x), 1, replace=FALSE, prob=x)
  return(id.spp)
}
