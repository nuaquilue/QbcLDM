# Set working directory 
rm(list = ls())
# Load the model and the default scenario's parameters
# Clean all data (but not functions) of the workspace
fun <- unclass(lsf.str())
toremove <- setdiff(ls(), fun)
toremove <- toremove[-which(toremove=="scenar")]
rm(list = c(toremove, 'toremove'))

####
# les outputs originaux sont en km2/ période de 5 ans. Transformé en ha/an
#

setwd("C:/Users/boumav/Desktop/QLD_V2")

# niveau de récolte au temps 0, déterminé à partir du scenario reference (ou autre)
# On additionne la CT et la CP (divisé par deux pour l'équivalent)
niveau.base <- read.table(paste0("outputs/SC9.TotProt/InitialHarvestLevel.txt"),header=TRUE)
niveau.base$ref.base.cpct <- niveau.base$ref.harv.level + niveau.base$ref.harv.level.cp/2
niveau.base$UA <- row.names(niveau.base)

# seuil qui correspond à la baisse de possibilité qui est considérée acceptable, 
# par rapport au niveau calculé à la première période

critere.failure <- 0.7 

### fonction qui permet d'extraire les valeurs pour les figures

fig.bfec <- function(scen){ # unique(sc_A$run)
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  sc_A2a <- aggregate(sc_A$cc.area.unaff,list(sc_A$time,sc_A$UA),mean)
  sc_A2b <- aggregate(sc_A$cc.area.salvaged,list(sc_A$time,sc_A$UA),mean)
  sc_A2a$recup <-  sc_A2b$x
  sc_A2a$tot_CT <-  sc_A2a$x + sc_A2a$recup
  sc_A2a$tot_CT <- (sc_A2a$tot_CT*100)/5
  sc_A2a$recup <- (sc_A2a$recup*100)/5
  sc_A2a$x <- (sc_A2a$x*100)/5
  # risque d'accident de régénération
  scA.rege.exc <- aggregate(sc_A$reg.fail.ex,list(sc_A$time,sc_A$UA),mean)
  scA.rege.inc <- aggregate(sc_A$reg.fail.inc,list(sc_A$time,sc_A$UA),mean)
  sc_A2a$riskrege_ex <- (scA.rege.exc$x * 100)/5
  sc_A2a$riskrege_inc <- (scA.rege.inc$x * 100)/5
  # superficie en coupe partielle
  sc_B <- read.table(paste0("outputs/", scen, "/PartialCut.txt"),header=TRUE)
  sc_B2a <- aggregate(sc_B$s.rec.pc,list(sc_B$time,sc_B$UA),mean)
  sc_B2a$tot_CP <- (sc_B2a$x*100)/5
  sc_A2a$tot_CP <- sc_B2a$tot_CP
  # équivalent volume total (deux fois moins dans les coupes partielles)
  sc_A2a$vol_tot <- sc_A2a$tot_CP/2 + sc_A2a$tot_CT
  
  # % failures = taux de récolte baisse en bas de 80% niveau initial
  # incluant les coupes partielles
  sc_A$harv.pc <- sc_B$s.rec.pc
  sc_ref <- merge(sc_A, niveau.base, by = "UA")
  liste.UA <- unique(sc_ref$UA)
  # 
  fail.UA <- data.frame(0)
  fail.UA <- data.frame(UA=character(),
                   per=numeric(0), 
                   Freq=numeric(0)) 
  for (i in 1:length(liste.UA)) {
    # extraire information par UA
    fail.1 <- sc_ref[sc_ref$UA == liste.UA[i],]
    periode <- numeric()
      for (j in 1:max(fail.1$run)) { # 
      
          # pour chaque run, identifier la première période où il y avait rupture de stock
           fail.2 <- fail.1[fail.1$run ==j, ]
           vec.fail.2 <- (fail.2$cc.area.unaff + fail.2$cc.area.salvaged + (fail.2$harv.pc/2))<(fail.2$ref.base.cpct*critere.failure)
           periode[j] <- min(fail.2$time[vec.fail.2])

         }
      bidon2 <- table(periode)  
      periode.id <- as.data.frame(seq(5,max(fail.1$time),5))
      names(periode.id)[1]<- "periode"            
      bidon3 <- merge(periode.id,as.data.frame(bidon2), all.x = T)
      bidon3$Freq[is.na(bidon3$Freq)] <- 0
      bidon3$Freq <- round(cumsum(bidon3$Freq)/max(fail.1$run),3)    
      bidon3$UA <- liste.UA[i] 
      fail.UA <- rbind(fail.UA,bidon3)
  }
  names(fail.UA) <- c("Group.1","fail","Group.2")
  sc_A3<- merge(sc_A2a,fail.UA)
  sc_A4 <- sc_A3[order(sc_A3$Group.2, sc_A3$Group.1),]
  
  return(sc_A4)
}

####################   resultats associés aux scenarios

scen <- "SC1.WPlant.Post.Lsalv"
scen1x<- fig.bfec(scen)
scen <- "SC2.Wplant.Prio.Lsalv"
scen2x <- fig.bfec(scen)
scen <- "SC3.NoPlant.Post.Lsalv"
scen3x <- fig.bfec(scen)
scen <- "SC4.NoPlant.Prio.Lsalv"
scen4x <- fig.bfec(scen)
scen <- "SC5.WPlant.Post.Hsalv"
scen5x<- fig.bfec(scen)
scen <- "SC6.WPlant.Prio.Hsalv"
scen6x <- fig.bfec(scen)
scen <- "SC7.NoPlant.Post.Hsalv"
scen7x <- fig.bfec(scen)
scen <- "SC8.NoPlant.Prio.Hsalv"
scen8x <- fig.bfec(scen)
scen <- "SC9.TotProt"
scen9x <- fig.bfec(scen)


###########################
### Figure superficies récoltées par coupe totale
### faudrait ajouter coupes partielles
par(mfrow=c(1,2), oma=c(4, 2, 2, 2),mar=c(1,3,1,0.3))


scen.UA <- c("2751","2571","2371","2471")
#scen.UA <- unique(scen1x$Group.2 )

scen1x2 <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,9)]
scen1 <- aggregate(scen1x2$vol_tot,list(scen1x2$Group.1),sum)
scen2x2 <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,9)]
scen2 <- aggregate(scen2x2$vol_tot,list(scen2x2$Group.1),sum)
scen3x2 <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,9)]
scen3 <- aggregate(scen3x2$vol_tot,list(scen3x2$Group.1),sum)
scen4x2 <- scen4x[scen4x$Group.2 %in% scen.UA,c(1,9)]
scen4 <- aggregate(scen4x2$vol_tot,list(scen4x2$Group.1),sum)
scen5x2 <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,9)]
scen5 <- aggregate(scen5x2$vol_tot,list(scen5x2$Group.1),sum)
scen6x2 <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,9)]
scen6 <- aggregate(scen6x2$vol_tot,list(scen6x2$Group.1),sum)
scen7x2 <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,9)]
scen7 <- aggregate(scen7x2$vol_tot,list(scen7x2$Group.1),sum)
scen8x2 <- scen8x[scen8x$Group.2 %in% scen.UA,c(1,9)]
scen8 <- aggregate(scen8x2$vol_tot,list(scen8x2$Group.1),sum)
scen9x2 <- scen9x[scen9x$Group.2 %in% scen.UA,c(1,9)]
scen9 <- aggregate(scen9x2$vol_tot,list(scen9x2$Group.1),sum)

par(mfrow=c(1,2), oma=c(4, 2, 2, 2),mar=c(1,3,1,0.3))

plot(scen1, t="l",ylim=c(00000,max(scen9[,2])),col=1,lty=1,ylab="",xaxt='n')
lines(scen2,col=2,lty=1)
lines(scen3,col=1,lty=1)
lines(scen4,col=2,lty=1)
lines(scen5,col=1,lty=2)
lines(scen6,col=2,lty=2)
lines(scen7,col=1,lty=2)
lines(scen8,col=2,lty=2)
lines(scen9,lwd=2, col=1)
legend("bottomleft", legend=c("SC1.WPlant.Post.Lsalv","SC2.Wplant.Prio.Lsalv","SC3.NoPlant.Post.Lsalv","SC4.NoPlant.Prio.Lsalv",
                              "SC5.WPlant.Post.Hsalv","SC6.WPlant.Prio.Hsalv","SC7.NoPlant.Post.Hsalv","SC8.NoPlant.Prio.Hsalv","SC9.TotProt"),
       cex=0.8,col=c(1,2,1,2,1,2,1,2,1),lwd=c(1,1,1,1,1,1,1,1,2),lty=c(1,1,1,1,2,2,2,2), bty="n",seg.len=5)   
mtext("ha par an", side=2,  line=0, outer = TRUE,cex=1,family="serif", font=1)
axis(side = 1,labels=(c("2030","2050","2070","2090")),at=c(20,40,60,80),las=0, tck = -0.02) 
mtext("Province au complet", side=3,  line=0, outer = F,cex=1,family="serif", font=1)



###########################
###  failures - pourcentage des runs pour lesquelles les feux entrainent une baisse du taux de récolte 
###  baisse en bas de 70% du niveau initial (t=0). 
par(mfrow=c(1,2), oma=c(4, 2, 2, 2),mar=c(1,3,1,0.3))
scen.UA <- c("9351")
#scen.UA <- unique(scenAx$Group.2 )

scen1.fail <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,10)]
scen2.fail <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,10)]
scen3.fail <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,10)]
scen4.fail <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,10)]
scen5.fail <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,10)]
scen6.fail <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,10)]
scen7.fail <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,10)]
scen8.fail <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,10)]
scen9.fail <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,10)]

plot(scen1.fail, t="l",ylim=c(00000,1),col=1,lty=1,ylab="",xaxt='n')
lines(scen2.fail,col=2,lty=1)
lines(scen3.fail,col=1,lty=1)
lines(scen4.fail,col=2,lty=1)
lines(scen5.fail,col=1,lty=2)
lines(scen6.fail,col=2,lty=2)
lines(scen7.fail,col=1,lty=2)
lines(scen8.fail,col=2,lty=2)
lines(scen9.fail,lwd=2, col=1)
legend("topleft", legend=c("SC1.WPlant.Post.Lsalv","SC2.Wplant.Prio.Lsalv","SC3.NoPlant.Post.Lsalv","SC4.NoPlant.Prio.Lsalv",
                           "SC5.WPlant.Post.Hsalv","SC6.WPlant.Prio.Hsalv","SC7.NoPlant.Post.Hsalv","SC8.NoPlant.Prio.Hsalv","SC9.TotProt"),
       cex=0.8,col=c(1,2,1,2,1,2,1,2,1),lwd=c(1,1,1,1,1,1,1,1,2),lty=c(1,1,1,1,2,2,2,2), bty="n",seg.len=5)   
mtext("Proportion cumulée de runs avec rupture de stock", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2030","2050","2070","2090")),at=c(20,40,60,80),las=0, tck = -0.02) 
mtext("UA 9351 (boreal)", side=3,  line=0, outer = F,cex=1,family="serif", font=1)

#########################################################################
### ratio recup/vert
par(mfrow=c(1,1), oma=c(4, 2, 2, 2),mar=c(1,3,1,0.3))

scen.UA <- c("2751")
#scen.UA <- unique(scenAx$Group.2 )

scen1.vert <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,3)]
scen1.vert2 <- aggregate(scen1.vert$x,list(scen1.vert$Group.1),sum)
scen1.recup <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,4)]
scen1.recup2 <- aggregate(scen1.recup$recup,list(scen1.recup$Group.1),sum)
vec.recup1 <- round(scen1.recup2$x / (scen1.vert2$x +  scen1.recup2$x),2)

scen2.vert <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,3)]
scen2.vert2 <- aggregate(scen2.vert$x,list(scen2.vert$Group.1),sum)
scen2.recup <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,4)]
scen2.recup2 <- aggregate(scen2.recup$recup,list(scen2.recup$Group.1),sum)
vec.recup2 <- round(scen2.recup2$x / (scen2.vert2$x +  scen2.recup2$x),2)

scen3.vert <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,3)]
scen3.vert2 <- aggregate(scen3.vert$x,list(scen3.vert$Group.1),sum)
scen3.recup <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,4)]
scen3.recup2 <- aggregate(scen3.recup$recup,list(scen3.recup$Group.1),sum)
vec.recup3 <- round(scen3.recup2$x / (scen3.vert2$x +  scen3.recup2$x),2)

scen4.vert <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,3)]
scen4.vert2 <- aggregate(scen4.vert$x,list(scen4.vert$Group.1),sum)
scen4.recup <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,4)]
scen4.recup2 <- aggregate(scen4.recup$recup,list(scen4.recup$Group.1),sum)
vec.recup4 <- round(scen4.recup2$x / (scen4.vert2$x +  scen4.recup2$x),2)

scen5.vert <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,3)]
scen5.vert2 <- aggregate(scen5.vert$x,list(scen5.vert$Group.1),sum)
scen5.recup <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,4)]
scen5.recup2 <- aggregate(scen5.recup$recup,list(scen5.recup$Group.1),sum)
vec.recup5 <- round(scen5.recup2$x / (scen5.vert2$x +  scen5.recup2$x),2)

scen6.vert <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,3)]
scen6.vert2 <- aggregate(scen6.vert$x,list(scen6.vert$Group.1),sum)
scen6.recup <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,4)]
scen6.recup2 <- aggregate(scen6.recup$recup,list(scen6.recup$Group.1),sum)
vec.recup6 <- round(scen6.recup2$x / (scen6.vert2$x +  scen6.recup2$x),2)

scen7.vert <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,3)]
scen7.vert2 <- aggregate(scen7.vert$x,list(scen7.vert$Group.1),sum)
scen7.recup <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,4)]
scen7.recup2 <- aggregate(scen7.recup$recup,list(scen7.recup$Group.1),sum)
vec.recup7 <- round(scen7.recup2$x / (scen7.vert2$x +  scen7.recup2$x),2)

scen8.vert <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,3)]
scen8.vert2 <- aggregate(scen8.vert$x,list(scen8.vert$Group.1),sum)
scen8.recup <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,4)]
scen8.recup2 <- aggregate(scen8.recup$recup,list(scen8.recup$Group.1),sum)
vec.recup8 <- round(scen8.recup2$x / (scen8.vert2$x +  scen8.recup2$x),2)

scen9.vert <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,3)]
scen9.vert2 <- aggregate(scen9.vert$x,list(scen9.vert$Group.1),sum)
scen9.recup <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,4)]
scen9.recup2 <- aggregate(scen9.recup$recup,list(scen9.recup$Group.1),sum)
vec.recup9 <- round(scen9.recup2$x / (scen9.vert2$x +  scen9.recup2$x),2)


plot(vec.recup1, t="l",ylim=c(00000,1),col=1,lty=1,ylab="",xaxt='n')
lines(vec.recup2,col=2,lty=1)
lines(vec.recup3,col=1,lty=1)
lines(vec.recup4,col=2,lty=1)
lines(vec.recup5,col=1,lty=2)
lines(vec.recup6,col=2,lty=2)
lines(vec.recup7,col=1,lty=2)
lines(vec.recup8,col=2,lty=2)
lines(vec.recup9,lwd=2, col=1)
legend("topleft", legend=c("SC1.WPlant.Post.Lsalv","SC2.Wplant.Prio.Lsalv","SC3.NoPlant.Post.Lsalv","SC4.NoPlant.Prio.Lsalv",
                           "SC5.WPlant.Post.Hsalv","SC6.WPlant.Prio.Hsalv","SC7.NoPlant.Post.Hsalv","SC8.NoPlant.Prio.Hsalv","SC9.TotProt"),
       cex=0.8,col=c(1,2,1,2,1,2,1,2,1),lwd=c(1,1,1,1,1,1,1,1,2),lty=c(1,1,1,1,2,2,2,2), bty="n",seg.len=5)   
mtext("Proportion de bois récupéré", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2030","2050","2070","2090")),at=c(20,40,60,80),las=0, tck = -0.02) 
mtext("UA 2751", side=3,  line=0, outer = F,cex=1,family="serif", font=1)


### ratio CP/CT - en SUPERFICIE

par(mfrow=c(1,2), oma=c(4, 2, 2, 2),mar=c(1,3,1,0.3))

scen.UA <- 2751

scen1.CP <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,8)]
scen1.CP2 <- aggregate(scen1.CP$tot_CP,list(scen1.CP$Group.1),sum)
scen1.CT <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,5)]
scen1.CT2 <- aggregate(scen1.CT$tot_CT,list(scen1.CT$Group.1),sum)
vec.CP.1 <- round(scen1.CP2$x / (scen1.CT2$x +  scen1.CP2$x),3)

scen2.CP <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,8)]
scen2.CP2 <- aggregate(scen2.CP$tot_CP,list(scen2.CP$Group.1),sum)
scen2.CT <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,5)]
scen2.CT2 <- aggregate(scen2.CT$tot_CT,list(scen2.CT$Group.1),sum)
vec.CP.2 <- round(scen2.CP2$x / (scen2.CT2$x +  scen2.CP2$x),3)

scen3.CP <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,8)]
scen3.CP2 <- aggregate(scen3.CP$tot_CP,list(scen3.CP$Group.1),sum)
scen3.CT <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,5)]
scen3.CT2 <- aggregate(scen3.CT$tot_CT,list(scen3.CT$Group.1),sum)
vec.CP.3 <- round(scen3.CP2$x / (scen3.CT2$x +  scen3.CP2$x),3)

scen4.CP <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,8)]
scen4.CP2 <- aggregate(scen4.CP$tot_CP,list(scen4.CP$Group.1),sum)
scen4.CT <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,5)]
scen4.CT2 <- aggregate(scen4.CT$tot_CT,list(scen4.CT$Group.1),sum)
vec.CP.4 <- round(scen4.CP2$x / (scen4.CT2$x +  scen4.CP2$x),3)

scen5.CP <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,8)]
scen5.CP2 <- aggregate(scen5.CP$tot_CP,list(scen5.CP$Group.1),sum)
scen5.CT <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,5)]
scen5.CT2 <- aggregate(scen5.CT$tot_CT,list(scen5.CT$Group.1),sum)
vec.CP.5 <- round(scen5.CP2$x / (scen5.CT2$x +  scen5.CP2$x),3)

scen6.CP <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,8)]
scen6.CP2 <- aggregate(scen6.CP$tot_CP,list(scen6.CP$Group.1),sum)
scen6.CT <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,5)]
scen6.CT2 <- aggregate(scen6.CT$tot_CT,list(scen6.CT$Group.1),sum)
vec.CP.6 <- round(scen6.CP2$x / (scen6.CT2$x +  scen6.CP2$x),3)

scen7.CP <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,8)]
scen7.CP2 <- aggregate(scen7.CP$tot_CP,list(scen7.CP$Group.1),sum)
scen7.CT <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,5)]
scen7.CT2 <- aggregate(scen7.CT$tot_CT,list(scen7.CT$Group.1),sum)
vec.CP.7 <- round(scen7.CP2$x / (scen7.CT2$x +  scen7.CP2$x),3)

scen8.CP <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,8)]
scen8.CP2 <- aggregate(scen8.CP$tot_CP,list(scen8.CP$Group.1),sum)
scen8.CT <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,5)]
scen8.CT2 <- aggregate(scen8.CT$tot_CT,list(scen8.CT$Group.1),sum)
vec.CP.8 <- round(scen8.CP2$x / (scen8.CT2$x +  scen8.CP2$x),3)

scen9.CP <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,8)]
scen9.CP2 <- aggregate(scen9.CP$tot_CP,list(scen9.CP$Group.1),sum)
scen9.CT <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,5)]
scen9.CT2 <- aggregate(scen9.CT$tot_CT,list(scen9.CT$Group.1),sum)
vec.CP.9 <- round(scen9.CP2$x / (scen9.CT2$x +  scen9.CP2$x),3)


plot(vec.CP.1, t="l",ylim=c(00000,max(c(vec.CP.9,vec.CP.4,vec.CP.1))),col=1,lty=1,ylab="",xaxt='n')
lines(vec.CP.2,col=2,lty=1)
lines(vec.CP.3,col=1,lty=1)
lines(vec.CP.4,col=2,lty=1)
lines(vec.CP.5,col=1,lty=2)
lines(vec.CP.6,col=2,lty=2)
lines(vec.CP.7,col=1,lty=2)
lines(vec.CP.8,col=2,lty=2)
lines(vec.CP.9,lwd=2, col=1)
legend("bottomleft", legend=c("SC1.WPlant.Post.Lsalv","SC2.Wplant.Prio.Lsalv","SC3.NoPlant.Post.Lsalv","SC4.NoPlant.Prio.Lsalv",
                              "SC5.WPlant.Post.Hsalv","SC6.WPlant.Prio.Hsalv","SC7.NoPlant.Post.Hsalv","SC8.NoPlant.Prio.Hsalv","SC9.TotProt"),
       cex=0.8,col=c(1,2,1,2,1,2,1,2,1),lwd=c(1,1,1,1,1,1,1,1,2),lty=c(1,1,1,1,2,2,2,2), bty="n",seg.len=5)   
mtext("Proportion de coupes partielles", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2015","2050","2090")),at=c(1,9,17),las=0, tck = -0.02) 
mtext("UA 2751 - (boreal)", side=3,  line=0, outer = F,cex=1,family="serif", font=1)



###########################################
########## risque rege

scen.UA <- unique(scen1x$Group.2)
par(mfrow=c(2,2), oma=c(4, 2, 2, 2),mar=c(1,4,1,0.3))

scen1.inc <- scen1x[scen1x$Group.2  %in% scen.UA,c(1,7)]
scen1.inc2 <- aggregate(scen1.inc$riskrege_inc,list(scen1.inc$Group.1),sum)
scen2.inc <- scen2x[scen2x$Group.2  %in% scen.UA,c(1,7)]
scen2.inc2 <- aggregate(scen2.inc$riskrege_inc,list(scen2.inc$Group.1),sum)
scen3.inc <- scen3x[scen3x$Group.2  %in% scen.UA,c(1,7)]
scen3.inc2 <- aggregate(scen3.inc$riskrege_inc,list(scen3.inc$Group.1),sum)
scen4.inc <- scen4x[scen4x$Group.2  %in% scen.UA,c(1,7)]
scen4.inc2 <- aggregate(scen4.inc$riskrege_inc,list(scen4.inc$Group.1),sum)
scen5.inc <- scen5x[scen5x$Group.2  %in% scen.UA,c(1,7)]
scen5.inc2 <- aggregate(scen5.inc$riskrege_inc,list(scen5.inc$Group.1),sum)
scen6.inc <- scen6x[scen6x$Group.2  %in% scen.UA,c(1,7)]
scen6.inc2 <- aggregate(scen6.inc$riskrege_inc,list(scen6.inc$Group.1),sum)
scen7.inc <- scen7x[scen7x$Group.2  %in% scen.UA,c(1,7)]
scen7.inc2 <- aggregate(scen7.inc$riskrege_inc,list(scen7.inc$Group.1),sum)
scen8.inc <- scen8x[scen8x$Group.2  %in% scen.UA,c(1,7)]
scen8.inc2 <- aggregate(scen8.inc$riskrege_inc,list(scen8.inc$Group.1),sum)
scen9.inc <- scen9x[scen9x$Group.2  %in% scen.UA,c(1,7)]
scen9.inc2 <- aggregate(scen9.inc$riskrege_inc,list(scen9.inc$Group.1),sum)

par(mfrow=c(1,2), oma=c(4, 2, 2, 2),mar=c(1,4,1,0.3))

#par(mfrow=c(1,1), oma=c(4, 2, 2, 2),mar=c(1,2.5,1,0.3))

plot(scen1.inc2, t="l",ylim=c(0,max(scen3.inc2,scen4.inc2,scen7.inc2)),col=1,main="",ylab="", xaxt='n')
mtext("superficie à risque (ha/an)", side=2,  line=0, outer = TRUE,cex=1.2,family="serif", font=1)
axis(side = 1,labels=(c("2030","2060","2090")),at=c(20,50,80),las=0, tck = -0.02) 

lines(scen2.inc2,col=1)
lines(scen3.inc2,col=2)
lines(scen4.inc2,col=2)
lines(scen5.inc2,col=1)
lines(scen6.inc2,col=1)
lines(scen7.inc2,col=2)
lines(scen8.inc2,col=2)
lines(scen9.inc2,lwd=2, col=3)
frame()
legend("bottomleft", legend=c("SC1.WPlant.Post.Lsalv","SC2.Wplant.Prio.Lsalv","SC3.NoPlant.Post.Lsalv","SC4.NoPlant.Prio.Lsalv",
                              "SC5.WPlant.Post.Hsalv","SC6.WPlant.Prio.Hsalv","SC7.NoPlant.Post.Hsalv","SC8.NoPlant.Prio.Hsalv","SC9.TotProt"),
       cex=0.8,col=c(1,1,2,2,1,1,2,2,3),lwd=c(1,1,1,1,1,1,1,1,2),lty=c(1,1,1,1,1,1,1,1), bty="n",seg.len=5)  


############################# superficie récoltée par essence (CT uniquement pour l'instant)

require(ggplot2)
require(gridExtra)

fig.bfec.ess <- function(scen){ 
  sc_A <- read.table(paste0("outputs/", scen, "/ClearCut.txt"),header=TRUE)
  harv.EPN2 <- aggregate(sc_A$harv.cc.EPN,list(sc_A$time,sc_A$UA),mean)
  harv.SAB2 <- aggregate(sc_A$harv.cc.SAB,list(sc_A$time,sc_A$UA),mean)
  harv.PET2 <- aggregate(sc_A$harv.cc.PET,list(sc_A$time,sc_A$UA),mean)
  harv.BOJ2 <- aggregate(sc_A$harv.cc.BOJ,list(sc_A$time,sc_A$UA),mean)
  harv.ERS2 <- aggregate(sc_A$harv.cc.ERS,list(sc_A$time,sc_A$UA),mean)
  harv.AUT2 <- aggregate(sc_A$harv.cc.other,list(sc_A$time,sc_A$UA),mean)
  out.harv <- cbind(harv.EPN2, harv.SAB2$x, harv.PET2$x, harv.BOJ2$x, harv.ERS2$x,harv.AUT2$x)
  out.harv[,3:8] <- round((out.harv[,3:8]*100)/5,0)
  out.harv  <- as.data.frame(out.harv)
  names(out.harv) <- c("Periode","UA","hEPN","hSAB","hPET","hBOJ","hERS","hAUT")
  return(out.harv)
}

UA="2371"

scen <- "Afeuil_Post_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
d<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Post_Lsalv")

scen <- "Afeuil_Pri_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
e<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Pri_Lsalv")

scen <- "Sfeuil_Post_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
f<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Post_Lsalv")

scen <- "Sfeuil_Pri_Lsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
g<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Pri_Lsalv")

scen <- "Afeuil_Post_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
h<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Post_Hsalv")

scen <- "Afeuil_Pri_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
i<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Afeuil_Pri_Hsalv")

scen <- "Sfeuil_Post_Hsalv"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
j<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("Sfeuil_Post_Hsalv")

scen <- "TotProt"
recolte <- fig.bfec.ess(scen)
xx <- recolte[recolte[,2]==UA,]
mdata <- melt(xx[,-2], id=c("Periode"))
k<- ggplot(mdata, aes(x=Periode, y=value, fill=variable)) + labs(x="Temps (an)",y="Superficie (ha/an)")+
  geom_area() + ggtitle("TotProt")

grid.arrange(d, e,f,g,h,i,j,k, nrow = 4, top = "Superficie récoltée en coupe TOTALE par type de peuplement - 2371")
