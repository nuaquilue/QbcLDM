library(weathercan)
library(viridis)

# Filter
stations.wind <- dplyr::filter(normals_measurements, prov=="QC" & stringr::str_detect(measurement, "wind_dir")) 
  # unique(stations.wind$measurement); unique(stations.wind$climate_id)
info.stations <- dplyr::filter(stations, climate_id %in% unique(stations.wind$climate_id), interval=="month")
info.stations

# get data and retrieve wind directions
n <- normals_dl(unique(stations.wind$climate_id))
winds <- data.frame(climate_id=NA, m1=NA, m2=NA, m3=NA, m4=NA, m5=NA, m6=NA, m7=NA, m8=NA,
                    m9=NA, m10=NA, m11=NA, m12=NA, my=NA)
for(i in 1:nrow(n)){
  w <- n$normals[[i]]$wind_dir
  winds <- rbind(winds, c(n$climate_id[i], w))
}
winds <- winds[-1,]

# Percentage
round(table(winds$my)/11,1)

# Map
library(maps)
library(mapdata)
info.stations$wind <- winds$my  
wind.col <- data.frame(wind=c("S", "SW", "W"), col=c("purple", "darkgreen", "yellow"))
info.stations <- dplyr::left_join(info.stations, wind.col)
# map("worldHires", "Canada", xlim=c(-141,-53), ylim=c(40,85), col="gray90", fill=TRUE)
map("worldHires", "Canada", xlim=c(-100,-30), ylim=c(40,70), col="gray85", fill=TRUE)
map("worldHires","usa", xlim=c(-100,-30), ylim=c(43,64), col="gray95", fill=TRUE, add=TRUE)
# points(info.stations$lon, info.stations$lat, pch=19, col="red", cex=1)
points(info.stations$lon, info.stations$lat, pch=19, col=info.stations$col, cex=1)
legend(-50, 60, title="Wind dir", legend=c("S", "SW", "W"), pt.cex=2.5, col='black', 
       pch=21, pt.bg=c("red", "black", "green"), bty="n") 
map.scale(-60, 43, ratio=FALSE, relwidth=0.1, cex=0.8)
