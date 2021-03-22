dir.create("outputs/Scn_rcp45_WH_salvage20_replan")
lfiles <- list.files("outputs/Scn_rcp45_WH_salvage20_replan_A", pattern="*.txt") 
for(txt in lfiles){
  dta <- read.table(paste0("outputs/Scn_rcp45_WH_salvage20_replan_A/", txt), header=T)
  for(i in LETTERS[2:4]){
    aux <- read.table(paste0("outputs/Scn_rcp45_WH_salvage20_replan_", i, "/", txt), header=T)
    aux$run <- aux$run+max(dta$run)
    dta <- rbind(dta, aux)
  }
  write.table(dta, paste0("outputs/Scn_rcp45_WH_salvage20_replan/",txt), row.names=F, quote=F, sep="\t")
}
  


