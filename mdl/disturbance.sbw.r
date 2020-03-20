######################################################################################
###  disturbance.sbw()
###
###  Description >  Simulates spruce budworm mortality according to a given probability 
###   of mortality after defoliation.
###
###  Arguments >  
###   subland : appropiate selection fo the data frame of the state variables
###   severity : severity of the sbw outbreak defined by the user
###   sbw.mortality : data frame of the probability of mortality by bioclimatic domain 
###                   and ecological type
###   disturb.interact : data frame of the time after a distrubance cap happen following
###                      another disturbance 
###   write.tbl.outputs : if TRUE
###   km2.pixel : number of km2 per pixel on the grid 
###   irun : the current replica (used when writing results)
###   t : the current time step  (used when writing results)
###   out.path : directory path to save output text files
###   out.overwrite : if TRUE the output text files are overwritten 
###
###  Details > Trees mortality by spruce budworm defoliation depends on the severity
###            of the epidemics.
###
###  Value >  A vector of the indexes of the killed cells.
######################################################################################

# subland <- subset(land, select = -c(FRZone, MgmtUnit, TSW, Exclus))
# subland <- subset(land, select=c(cell.indx, BCDomain, SppGrp, Temp, TSF, TSD), SppGrp!="NonFor") 
# severity = 1 0.5
#subland <- land
disturbance.sbw <- function(subland, severity, write.tbl.outputs=T,
                            km2.pixel=1, irun=1, t, out.path=NULL, out.overwrite=T){
  # Silence
  options(warn=-1)
  
  # If SBW severity is not fixed, determine High (1), Moderate (0.5), or Low (0.2) 
  # Les probabilités initiales sont définies à l'optimum (high=1)
  # SBW outbreak severity with probability p = c(1/6,2/6,3/6), respectively
  #if(is.null(severity))
  #  severity <- which( rmultinom(n=c(1,0.5,0.2), size=1, prob=c(1/6,2/6,3/6)) == 1)
  
  #####  suitability
  
  # three climatic levels, determined from SDM: highly favorable (1), moderately favorable (0.5), unfavorable (0)
  sbw.clim  <- ifelse(subland$Temp> 0.5 & subland$Temp < 2.8, 1, 
                             ifelse(subland$Temp> -1.5 & subland$Temp < 4, 0.3, 0))
  # three age levels: old (1), young (0.5), regen (0)
  sbw.age  <- ifelse(subland$TSD > 60 & subland$TSF !=0  , 1, 
                             ifelse(subland$TSD > 30 & subland$TSF !=0, 0.8, 0.6))
  # three compositions, defined in the highly favorable zone: fir (0.5), EPN (0.15), others (0)
  sbw.comp <- ifelse(subland$SppGrp == "SAB", 0.6, 
                            ifelse(subland$SppGrp =="EPN", 0.1, 0))  

  prob <- sbw.clim*sbw.age * sbw.comp * (severity) 

  subland$kill <- prob > runif(length(prob))
  
  # Write the results (area killed by domain and species group) in a output text file
#  if(write.tbl.outputs){
#    aux <- data.frame(domain = subland$BCDomain[subland$kill==1],
#                      spp.grp = subland$SppGrp[subland$kill==1], kill.area = km2.pixel)
#    aux <- aggregate(aux$kill.area, list(domain=aux$domain, spp.grp=aux$spp.grp), sum)
#    names(aux)[3] <- "kill.area"
#    write.table(data.frame(run=irun, time=t, sever=severity, aux),
#                file=paste0(out.path, "/Epidemic.txt"), append=!out.overwrite, quote=F,
#                sep="\t", row.names=F , col.names=out.overwrite)
#  }
  load(file=paste0("inputlyrs/rdata/mask", name.resol, ".rdata"))
   MASK[!is.na(MASK)] <- as.factor(ifelse(prob==0, 1,
                                         ifelse (prob<0.2,2,
                                                 ifelse(prob<0.4,3,
                                                        ifelse(prob<0.6,4,
                                                               ifelse(prob<0.8,5,6))))))
  image(MASK,col=c("grey","pink","pink","pink","red","red","red"))
  print(paste(t,table(as.data.frame(MASK))))
  print(paste("clim - ",mean(sbw.clim)))
  print(paste("age - ",mean(sbw.age)))
  print(paste("comp - ",mean(sbw.comp)))
  print(paste("prob - ",mean(prob)))  
  #writeRaster(MASK, paste0("outputs/test_drf_2019_pri_avecreplan/asc/TBE_", t, ".asc"), format="ascii", overwrite=T)
  # Return the sbw killed cell.indx 
  return(subland$cell.indx[subland$kill==1])
  
}
