a.priori <-
1
age.seed <-
50
avec.combu <-
0
cc.step <-
5
diff.prematurite <-
10
disturb <-
c(TRUE, TRUE, TRUE, TRUE, TRUE)
ecocrisis <-
FALSE
ecocrisis.freq <-
0
fire.rate.increase <-
0
fire.regime.file <-
"inputfiles/NumFires.txt"
fire.sizes.file <-
"inputfiles/FireSizesEmpiric.txt"
fire.step <-
5
forest.succ <-
structure(list(SppGrp = structure(c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("BOJ", "EPN", 
"ERS", "NonFor", "other", "PET", "SAB"), class = "factor"), PotSpp = structure(c(7L, 
2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 
3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 
5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L
), .Label = c("BOJ", "EPN", "ERS", "NonFor", "other", "PET", 
"SAB"), class = "factor"), ptrans = c(90, 5, 1, 5, 5, 1, 0, 15, 
80, 1, 1, 1, 1, 0, 20, 35, 10, 10, 15, 5, 0, 1, 1, 10, 85, 5, 
5, 0, 15, 1, 10, 25, 60, 15, 0, 5, 15, 10, 5, 5, 75, 0, 0, 0, 
0, 0, 0, 0.01, 100)), class = "data.frame", row.names = c(NA, 
-49L))
FSdistrib <-
structure(list(lower = c(0L, 50L, 100L, 150L, 200L, 250L, 300L, 
350L, 400L, 450L, 500L, 550L, 600L, 650L, 700L, 750L, 800L, 850L, 
900L, 950L, 1000L, 1050L, 1100L, 1150L, 1200L, 1250L, 1300L, 
1350L, 1400L, 1450L, 1500L, 1550L, 1600L, 1650L, 1700L, 1750L, 
1800L, 1850L, 1900L, 1950L), upper = c(50L, 100L, 150L, 200L, 
250L, 300L, 350L, 400L, 450L, 500L, 550L, 600L, 650L, 700L, 750L, 
800L, 850L, 900L, 950L, 1000L, 1050L, 1100L, 1150L, 1200L, 1250L, 
1300L, 1350L, 1400L, 1450L, 1500L, 1550L, 1600L, 1650L, 1700L, 
1750L, 1800L, 1850L, 1900L, 1950L, 2000L), ZA = c(0.15, 0.25, 
0.15, 0.1, 0.05, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
0.01, 0.01, 0, 0.01, 0, 0.01, 0, 0.01, 0, 0.01, 0, 0.01, 0, 0.01, 
0, 0.01), ZB = c(0.15, 0.25, 0.15, 0.1, 0.05, 0.02, 0.02, 0.01, 
0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 
0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0, 0.01, 0, 0.01, 0, 
0.01, 0, 0.01, 0, 0.01, 0, 0.01, 0, 0.01), ZC = c(0.3, 0.3, 0.2, 
0.1, 0.04, 0.02, 0.01, 0.01, 0.01, 0.01, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0), ZD = c(0.9, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0)), class = "data.frame", row.names = c(NA, -40L))
fuel.types.modif <-
c(0.1, 0.4, 0.95)
hor.plan <-
22
is.climate.change <-
TRUE
min.time.step <-
5
name.resol <-
""
nb.buff <-
c(1, 1, 1, 1, 1)
NFdistrib <-
structure(list(zone = structure(1:4, .Label = c("ZA", "ZB", "ZC", 
"ZD"), class = "factor"), distrib = structure(c(1L, 1L, 1L, 1L
), .Label = "Possion", class = "factor"), lambda = c(2.6, 2.7, 
2.9, 1), param2 = c(NA, NA, NA, NA), min.nfires = c(1L, 1L, 1L, 
1L), max.nfires = c(2000L, 300L, 300L, 300L)), class = "data.frame", row.names = c(NA, 
-4L))
nrun <-
1
out.path <-
"outputs/SC1.WPlant.Post.Lsalv"
p.failure <-
0
persist <-
c(1, 1, 1, 1, 1)
plot.fires <-
FALSE
post.fire.reg <-
structure(list(SppGrp = structure(c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("BOJ", "EPN", 
"ERS", "NonFor", "other", "PET", "SAB"), class = "factor"), age.class = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
), .Label = "adult", class = "factor"), PotSpp = structure(c(7L, 
2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 
3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 
5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L
), .Label = c("BOJ", "EPN", "ERS", "NonFor", "other", "PET", 
"SAB"), class = "factor"), ptrans = c(30L, 35L, 20L, 0L, 1L, 
10L, 0L, 10L, 85L, 5L, 0L, 1L, 5L, 0L, 1L, 25L, 70L, 1L, 1L, 
5L, 0L, 0L, 0L, 60L, 20L, 10L, 25L, 0L, 0L, 0L, 75L, 20L, 20L, 
25L, 0L, 5L, 20L, 15L, 10L, 5L, 35L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 100L)), class = "data.frame", row.names = c(NA, -49L))
post.harvest.reg <-
structure(list(SppGrp = structure(c(7L, 7L, 7L, 7L, 7L, 7L, 7L, 
2L, 2L, 2L, 2L, 2L, 2L, 2L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 3L, 3L, 
3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("BOJ", "EPN", 
"ERS", "NonFor", "other", "PET", "SAB"), class = "factor"), PotSpp = structure(c(7L, 
2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 
3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 
5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L, 7L, 2L, 6L, 3L, 1L, 5L, 4L
), .Label = c("BOJ", "EPN", "ERS", "NonFor", "other", "PET", 
"SAB"), class = "factor"), ptrans = c(70L, 15L, 5L, 1L, 15L, 
1L, 0L, 20L, 60L, 20L, 1L, 1L, 1L, 0L, 15L, 15L, 40L, 10L, 12L, 
20L, 0L, 1L, 1L, 1L, 90L, 5L, 5L, 0L, 20L, 1L, 10L, 25L, 55L, 
1L, 0L, 10L, 10L, 10L, 10L, 5L, 55L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 100L)), class = "data.frame", row.names = c(NA, -49L))
radius.buff <-
c(75000, 60000, 50000, 50000, 50000)
replanif <-
1
salvage.rate.event <-
1
salvage.rate.FMU <-
1
scn.name <-
"SC1.WPlant.Post.Lsalv"
Subopt <-
0.5
succ.enable <-
1
target.old.pct <-
0.2
ThAnnualPrecip <-
structure(list(Spp = structure(c(6L, 2L, 5L, 1L, 3L, 4L), .Label = c("BOJ", 
"EPN", "ERS", "other", "PET", "SAB"), class = "factor"), Th1 = c(-9999L, 
-9999L, -9999L, -9999L, -9999L, -9999L), Th2 = c(1100L, -9998L, 
-9998L, -9998L, -9998L, -9998L), Th3 = c(9999L, 1075L, 9998L, 
9998L, 9998L, 9998L), Th4 = c(10000L, 9999L, 9999L, 9999L, 9999L, 
9999L)), class = "data.frame", row.names = c(NA, -6L))
ThMeanTemp <-
structure(list(Spp = structure(c(6L, 2L, 5L, 1L, 3L, 4L), .Label = c("BOJ", 
"EPN", "ERS", "other", "PET", "SAB"), class = "factor"), Th1 = c(-999, 
-999, -0.25, 1.1, 1.9, -0.2), Th2 = c(-1.9, -998, 0.2, 1.7, 2.9, 
3), Th3 = c(3.3, 0, 5.5, 3.4, 7, 998), Th4 = c(5, 2.1, 8.5, 4.7, 
14, 999)), class = "data.frame", row.names = c(NA, -6L))
ThSoil <-
structure(list(Spp = structure(c(6L, 2L, 5L, 1L, 3L, 4L), .Label = c("BOJ", 
"EPN", "ERS", "other", "PET", "SAB"), class = "factor"), ThT = c(1, 
0.8, 0.7, 1, 1, 0.7), ThO = c(0.1, 1, 0.2, 0, 0.2, 0.8), ThR = c(0.7, 
0.9, 0.8, 0.1, 1, 0.8), ThS = c(0.7, 0.7, 0.9, 0.3, 0.7, 1), 
    ThA = c(0.3, 0.9, 1, 0, 0.3, 0.9)), class = "data.frame", row.names = c(NA, 
-6L))
time.horizon <-
100
write.sp.outputs <-
TRUE
write.tbl.outputs <-
TRUE
