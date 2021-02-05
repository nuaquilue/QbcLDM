######################################################################################
###  disturbance.sbw()
###
###  Description >  Simulates spruce budworm mortality according to a given probability 
###   of mortality after defoliation.
###
###  Arguments >  
###   land : appropiate selection fo the data frame of the state variables
###   severity : severity of the sbw outbreak defined by the user
###   sbw.mortality : data frame of the probability of mortality by bioclimatic domain 
###                   and ecological type
###   disturb.interact : data frame of the time after a distrubance cap happen following
###                      another disturbance 
###   km2.pixel : number of km2 per pixel on the grid 
###
###  Details > Trees mortality by spruce budworm defoliation depends on the severity
###            of the epidemics.
###
###  Value >  A vector of the indexes of the killed cells.
######################################################################################

sbw.outbreak<- function(land, severity, km2.pixel){
  
  # If SBW severity is not fixed, determine High (1), Moderate (0.5), or Low (0.2) 
  # Les probabilit?s initiales sont d?finies ? l'optimum (high=1)
  # SBW outbreak severity with probability p = c(1/6,2/6,3/6), respectively
  #if(is.null(severity))
  #  severity <- which( rmultinom(n=c(1,0.5,0.2), size=1, prob=c(1/6,2/6,3/6)) == 1)
  
  #####  suitability
  
  # three climatic levels, determined from SDM: highly favorable (1), moderately favorable (0.5), unfavorable (0)
  sbw.clim  <- ifelse(land$temp> 0.5 & land$temp < 2.8, 1, ifelse(land$temp> -1.5 & land$temp < 4, 0.3, 0))
  # three age levels: old (1), young (0.5), regen (0)
  sbw.age  <- ifelse(land$age>60 & land$tsfire!=0, 1, ifelse(land$age>30 & land$tsfire!=0, 0.8, 0.6))
  # three compositions, defined in the highly favorable zone: fir (0.5), EPN (0.15), others (0)
  sbw.comp <- ifelse(land$spp == "SAB", 0.4, ifelse(land$spp =="EPN", 0.05, 0))  

  prob <- sbw.clim*sbw.age * sbw.comp * (severity) 

  land$kill <- prob > runif(length(prob))
  
  # Write the results (area killed by domain and species group) in a output text file
#  if(write.tbl.outputs){
#    aux <- data.frame(domain = land$BCDomain[land$kill==1],
#                      spp.grp = land$SppGrp[land$kill==1], kill.area = km2.pixel)
#    aux <- aggregate(aux$kill.area, list(domain=aux$domain, spp.grp=aux$spp.grp), sum)
#    names(aux)[3] <- "kill.area"
#    write.table(data.frame(run=irun, time=t, sever=severity, aux),
#                file=paste0(out.path, "/Epidemic.txt"), append=!out.overwrite, quote=F,
#                sep="\t", row.names=F , col.names=out.overwrite)
#  }

#  load(file=paste0("inputlyrs/rdata/mask", name.resol, ".rdata"))
#   MASK[!is.na(MASK)] <- as.factor(ifelse(prob==0, 1,
#                                         ifelse (prob<0.2,2,
#                                                 ifelse(prob<0.4,3,
#                                                        ifelse(prob<0.6,4,
#                                                               ifelse(prob<0.8,5,6))))))
#  image(MASK,col=c("grey","pink","pink","pink","red","red","red"))
#  print(paste(t,table(as.data.frame(MASK))))
  
  
  print(paste("clim - ",mean(sbw.clim)))
  print(paste("age - ",mean(sbw.age)))
  print(paste("comp - ",mean(sbw.comp)))
  print(paste("prob - ",mean(prob)))  
  #writeRaster(MASK, paste0("outputs/test_drf_2019_pri_avecreplan/asc/TBE_", t, ".asc"), format="ascii", overwrite=T)
  # Return the sbw killed cell.indx 
  return(land$cell.indx[land$kill==1])
  
}
