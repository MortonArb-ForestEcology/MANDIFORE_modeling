# Quick analysis of our carbon storage, sequestration etc. from the MANDIFORE simulations

library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"
path.figs <- file.path(path.google, "figures")


f.all <- dir(file.path(path.google, "output"), "MgmtNone_Site.csv")
carb.arb <- data.frame(GCM=rep(NA, length(f.all)), rcp=NA, agb=NA, gpp=NA, npp=NA, nee=NA)
for(i in 1:length(f.all)){
  ftemp <- read.csv(file.path(path.google, "output", f.all[i]))
  ftemp <- ftemp[ftemp$year>=2010 & ftemp$year<=2019,]
  
  # Just getting a single value per model for our baseline
  carb.arb[i,] <- aggregate(cbind(agb, gpp, npp, nee)~GCM + rcp, data=ftemp, FUN=mean)
}

summary(carb.arb)

# # All values are currently kgC/m2 --> convert to kgCO2 Acres
# carb.arb[,c("agb", "gpp", "npp", "nee")] <- carb.arb[,c("agb", "gpp", "npp", "nee")]*2*4046.8564224
m2Ac <- 4046.8564224

mean(carb.arb$agb); sd(carb.arb$agb)  #  in kgC/m2
mean(carb.arb$agb)*1e-3*m2Ac; sd(carb.arb$agb)*1e-3*m2Ac  #  in MgCO2/Ac

mean(carb.arb$npp); sd(carb.arb$npp)  #  in kgC/m2
mean(carb.arb$npp)*1e-3*m2Ac; sd(carb.arb$npp)*1e-3*m2Ac  #  in MgCO2/Ac

-mean(carb.arb$nee); sd(carb.arb$nee)  #  in kgC/m2
-mean(carb.arb$nee)*1e-3*m2Ac; sd(carb.arb$nee)*1e-3*m2Ac  #  in MgCO2/Ac

# Rough estimate of Arb emissions from Rachel Novick: 4,000 metric tonnes CO2 in 2019; 1 T CO2 = 1/3.67 T Carbon 
arb.emit <- 4000/3.67
