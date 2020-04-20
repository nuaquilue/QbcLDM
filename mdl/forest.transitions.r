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
###
###
###   Details > It is a generic function that applies any species transition matrix 
###             to any subset of land. 
###
###   Value > Returns a data frame with cell index and new species composition
###          
######################################################################################

          # subland  <- filter(land, cell.id %in% burnt.cells)
          # prob.reg <- post.fire.reg
#
         subland  <- filter(land, cell.id %in% unlist(chg.comp.cells))
         prob.reg <- forest.succ

forest.trans <- function(subland, prob.reg, buffer, suitab, potential.spp, 
                         dtype, p.failure, age.seed, suboptimal, enfeuil){
  
  ## Tracking
  cat("Forest transition", "\n")
  
  
  ## If target data.frame is empty
  if(nrow(subland)==0)
    return(numeric())
  
  ## Keep the current species in case any potential species can colonize the site.
  ## In that case, the current species, persist.
  current.spp <- subland$SppGrp  
  
  ## Join to the subland data frame the probability of transition to PotSpp (according to initial SppGrp)
  ## Then join buffer results indicating whether the potential species is present in the surrounding neighborhood
  ## Finally join the climatic-soil suitability index (i.e. modifier) of the potential spp
  subland <- left_join(subland, prob.reg, by="SppGrp") %>%
             left_join(buffer, by=c("cell.id","PotSpp")) %>%
             left_join(suitab, by=c("cell.id","PotSpp")) 

  # Eufeuillement volontaire suite aux coupes
  if(dtype=="C" & enfeuil>0){  
      vec.enfeuil  <- land.prob[land.prob$SppGrp %in% c("EPN","SAB") & land.prob$PotSpp=="PET",]$ptrans
      vec.enfeuil2 <- (vec.enfeuil) + ((runif(length(vec.enfeuil))<enfeuil )*1000)
      land.prob[land.prob$SppGrp %in% c("EPN","SAB") & land.prob$PotSpp=="PET",]$ptrans <- vec.enfeuil2
  }
  
  ## Reburning case: If burnt stands are too young, probability of successful natural regeneration is lower
  if(dtype=="B"){ 
    subland$ptrans[subland$SppGrp=="EPN" & subland$PotSpp=="EPN"& subland$Age<age.seed] <- 
      subland$ptrans[subland$SppGrp=="EPN" & subland$PotSpp=="EPN"& subland$Age<age.seed] * (1-p.failure)
  }
  
  ## Stability criteria: if the species is present in the target location, then
  ## soil conditions are assumed to be optimal (not limiting)
      # levels(subland$SppGrp) <- levels(subland$PotSpp)
  subland$PressBuffer <- (subland$SppGrp == subland$PotSpp) | (subland$PressBuffer)
  subland$SuitSoil[subland$SppGrp == subland$PotSpp] <- 1

  ## Species persistence when climatic conditions become unfavorable: when persistence is allowed (1), 
  ## there is a floor probability of self-replacement corresponding to sub-optimal conditions 
  ## (under the assumption that competition is more limiting than  physiological response to climate)
  ## First, find which spp are allowed to persist then, upgrade climatic suitability to suboptimal
  ## in case this is lower than suboptimal.
  spp.persist <- potential.spp$PotSpp[potential.spp$persist==0.1]
  subland$SuitClim[subland$SppGrp %in% spp.persist & 
                     subland$SppGrp == subland$PotSpp & subland$SuitClim<suboptimal] <- suboptimal
  
  ## Determine the final succession / regeneration probability 
  subland$p <- subland$ptrans * subland$PressBuffer * pmin(subland$SuitClim, subland$SuitSoil)
  
  ## Reshape the data frame, so we have a column for each potential species with 
  ## the corresponding transition probability (one row per target cell)
  ## Substitute dcast by "gather" or "spread" from tidyverse
  aux <- reshape2::dcast(subland, formula = cell.id ~ PotSpp, value.var = "p")
  
  ## Now select a new spp according to these probabilitiesand assign the corresponing species name
  ## If after all filters, p for all PotSpp is 0, the current species remains
  spp.names <- names(aux)[2:ncol(aux)]
  id.spp <- apply(aux[,2:ncol(aux)], 1, select.spp)
  new.spp <- numeric(length=length(id.spp))
  new.spp[id.spp!=0] <- spp.names[id.spp[id.spp!=0]]
  new.spp[id.spp==0] <- as.character(current.spp[id.spp==0])
    
  ## Return the vector with the name of the new spp
  return(new.spp)

}

## Function that returns spp id according to probability x
select.spp <- function(x){
  if(sum(x)==0)
    return(0)
  id.spp <- sample(1:length(x), 1, replace=FALSE, prob=x)
  return(id.spp)
}
