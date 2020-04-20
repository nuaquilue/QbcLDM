rm(list=ls())
setwd("C:/work/qbcmod/QbcLDM")
scn.name <- "Test01"
fires <- read.table(paste0("outputs/", scn.name, "/Fires.txt"), header = T)
View(fires)
