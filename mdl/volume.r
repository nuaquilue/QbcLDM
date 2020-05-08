######################################################################################
###  volume()
###
###  Description >  Calculates harvested volume per FMU. Called in landscape.dyn.
###
###  Arguments >  
###   land : data frame of the state variables       
###   km2.pixel : number of km2 per pixel on the grid
###   min.time.step : the lowest common denominator of the disturbances time schedulings
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###
###  Details > Counts the cells in each age invterval of 20 years for each vegetation class.
###
###  Value > It writes a data frame with the assessment of age class per species group.
######################################################################################

volume <- function(subland, km2.pixel, min.time.step, irun, t){
  
  VolDis <- read.table("C:/Users/boumav/Desktop/QLD_V2/inputfiles/Volume.txt", header = T)
  VolDis1 <- melt(VolDis, id.vars = c("Time"))
  names(VolDis1) <-c("TSD","SppGrp","vol")

  # Calculer superficie productive
  #subland <- land
  
  s.inc <- subland[!is.na(subland$MgmtUnit) &  subland$TSD >= 0 & is.na(subland$Exclus) & subland$SppGrp != "NonFor",]
  superf <- aggregate(list(s.inc$cell.indx>0,s.inc$TSC==0,s.inc$TSPC==0),  by=list (s.inc$MgmtUnit), FUN=sum)
  names(superf) <- c("uaf","s.prod","s.CC","s.PC")
  subland.rec <- subland[!is.na(subland$MgmtUnit) & subland$TSC==0 | subland$TSPC==0 , ]                  

  # associer volume 

  subland.rec$TSD[subland.rec$TSD>120] <- 120
  land.vol <- join(subland.rec, VolDis1, by=c("TSD","SppGrp"), type="left", match="all")
  land.vol$vol[land.vol$TSPC==0] <- land.vol$vol[land.vol$TSPC==0]/2
  # volume récolté par UA et par groupement d'essences
  
  UAF_vol <- aggregate(land.vol$vol*400/5,  by=list (land.vol$SppGrp,land.vol$MgmtUnit), FUN=sum)
  
  # Count the cells in each Age Class per Bioclimatic Domain (in pixels)
  # 'split' functions returns a list, each element corresponds to a Bioclimatic Domain (ecozone)
  # then 'tapply' assess the age distribution per Species Group, it returns a list of
  # as many elements as number of species groups. Each element is a vector with 6 values: 
  # the number of cells per age class   
  

  # Complete the data frame with the run and the time step, then write it in a text file
  UAF_vol2 <- data.frame(rep(irun, nrow(UAF_vol)), rep(t, nrow(UAF_vol)), UAF_vol)
  names(UAF_vol2) <- c("run","time","spp", "uaf", "m3/an")

  UAF_vol3 <- dcast(UAF_vol2, run + time + uaf ~ spp, value.var=c("m3/an"), fun=sum)
  UAF_vol3$vtot <- rowSums(UAF_vol3[,4:9])
  UAF_vol4 <- join(superf,UAF_vol3, by=c("uaf"), type="left", match="all")
  
  write.table(data.frame(run=irun, time=t, UA=UAF_vol4$uaf, SProd_ha = UAF_vol4$s.prod*400, 
                         S_CC_ha_an = UAF_vol4$s.CC*400/5, S_PC_ha_an = UAF_vol4$s.PC*400/5, vol = UAF_vol4$vtot) ,
              file=paste0(out.path, "/Volumes.txt"), quote=FALSE, sep="\t",
              append=(irun != 1 | t != 5), 
              row.names=FALSE, col.names=(irun == 1 & t == 5)) 
  

}
 