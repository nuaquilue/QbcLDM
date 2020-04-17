library(weathercan)

stations.wind <- filter(normals_measurements, prov=="QC" & stringr::str_detect(measurement, "wind_dir")) 
unique(stations.wind$measurement)
unique(stations.wind$climate_id)
# get data
n <- normals_dl(unique(stations.wind$climate_id))

i <- 1
winds <- data.frame(id=NA, m1=NA, m2=NA, m3=NA, m4=NA, m5=NA, m6=NA, m7=NA, m8=NA,
                    m9=NA, m10=NA, m11=NA, m12=NA, my=NA)
for(i in 1:nrow(n)){
  w <- n$normals[[i]]$wind_dir
  winds <- rbind(winds, c(n$climate_id[i], w))
}
winds <- winds[-1,]
round(table(winds$my)/11,1)
