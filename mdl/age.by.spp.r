######################################################################################
###  age.by.spp()
###
###  Description >  Reports the age class abundance per stand type. Called in landscape.dyn.
###
###  Arguments >  
###   land : data frame of the state variables       
###   km2.pixel : number of km2 per pixel on the grid
###   min.time.step : the lowest common denominator of the disturbances time schedulings
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###
###  Details > Counts the cells in each age interval of 20 years for each species.
###
###  Value > It writes a data frame with the assessment of age class per species group.
######################################################################################

age.by.spp <- function(land, km2.pixel, min.time.step, irun, t){

  # Will only consider cells with vegetation or non-forested (NonFor).
  subland <- land[!is.na(land$SppGrp), ]                  
  
  # Assign a numeric age class according to the age intervals:
  subland$AgeClass <- (subland$TSD > -1 & subland$TSD <= 20) * 1 + 
                      (subland$TSD > 20 & subland$TSD <= 40) * 2 +
                      (subland$TSD > 40 & subland$TSD <= 60) * 3 +
                      (subland$TSD > 60 & subland$TSD <= 80) * 4 +
                      (subland$TSD > 80 & subland$TSD <= 100) * 5 +
                      (subland$TSD == -1 | subland$TSD > 100) * 6
  subland$AgeClass <- factor(subland$AgeClass)
  all.levels <- as.character(1:6) 
  levels(subland$AgeClass) <- c(levels(subland$AgeClass),
                              all.levels[!(all.levels %in% levels(subland$AgeClass))])
  
  # Number of Ecozones (bioclimatic domains), Species Groups and Age Classes  
  nBCDomain <- length(levels(subland$BCDomain))
  nSppGrp <- length(levels(subland$SppGrp))
  nAgeClass <- length(levels(subland$AgeClass))
  
  # Counts the cells in each Age Class per Bioclimatic Domain (in pixels)
  # 'split' function returns a list, each element corresponds to a Bioclimatic Domain (ecozone)
  # then 'tapply' assesses the age distribution per Species Group, it returns a list of
  # as many elements as there are species groups.
  
  domains <- unique(subland$BCDomain)
  tab_so <- table(subland$SppGrp,subland$AgeClass,subland$BCDomain)
  for(d in 1:nBCDomain){ # d=1
    tabx <- as.data.frame(cbind(rep(domains[d],nrow(tab_so[,,d])),row.names(tab_so[,,d]),tab_so[,,d]*km2.pixel))
    
    if(d==1)
      age.spp <- tabx
    else
      age.spp <- rbind(age.spp, tabx )
  }
  
  #  permutation of columns in case an AgeClass is missing
  new.col <- as.numeric(levels(subland$AgeClass))
  kk <- c(which(new.col==1), which(new.col==2), which(new.col==3), 
          which(new.col==4), which(new.col==5), which(new.col==6))
  age.spp <- age.spp[,c(1:2,2+kk)]  
  age.spp$Tot <- as.numeric(levels(age.spp[,3])) [age.spp[,3]]  +
                 as.numeric(levels(age.spp[,4])) [age.spp[,4]]  +
                 as.numeric(levels(age.spp[,5])) [age.spp[,5]]  +
                 as.numeric(levels(age.spp[,6])) [age.spp[,6]]  +
                 as.numeric(levels(age.spp[,7])) [age.spp[,7]]  +
                 as.numeric(levels(age.spp[,8])) [age.spp[,8]]  
                                                            

                            
  # Complete the data frame with the run and the time step, then write it in a text file
  age.spp <- data.frame(rep(irun, nrow(age.spp)), rep(t, nrow(age.spp)), age.spp)
  names(age.spp) <- c("run","time","domain", "spp", "0-20","20-40","40-60","60-80","80-100",">100","Tot")
  return(age.spp)

}
 