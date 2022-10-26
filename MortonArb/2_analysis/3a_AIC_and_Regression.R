#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection and does linear regression
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: AIC table
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

path.reg <- "../figures/regression/"
if(!dir.exists(path.reg)) dir.create(path.reg, recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "agb", sep=""))) dir.create(paste(path.reg, "agb", sep=""), recursive=T, showWarnings = F)

path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff.future),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.late$Driver.set <- paste0(runs.late$GCM,"." ,runs.late$rcp)

#----------------------------------------------------#
# Setting up the AIC
#----------------------------------------------------#
#Structure on its own and with Management
#AGB
agb.test <- lme(agb.rel.diff.future ~ agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.agb.test <- lme(agb.rel.diff.future ~ Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Mean DBH
dbh.mean.test <- lme(agb.rel.diff.future ~ tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH
dbh.sd.test <- lme(agb.rel.diff.future ~ tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
density.tree.test <- lme(agb.rel.diff.future ~ density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.density.tree.test <- lme(agb.rel.diff.future ~ Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Mean height
height.mean.test <- lme(agb.rel.diff.future ~ tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.height.mean.test <- lme(agb.rel.diff.future ~ Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
height.sd.test <- lme(agb.rel.diff.future ~ tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

MNG.height.sd.test <- lme(agb.rel.diff.future ~ Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#-----------------------------------------------------------------------------------#
#Precipitation as the weather metric
#-----------------------------------------------------------------------------------#
p.test <- lme(agb.rel.diff.future ~ precip.total*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.test <- lme(agb.rel.diff.future ~ precip.total*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
p.agb.test <- lme(agb.rel.diff.future ~ precip.total*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.agb.test <- lme(agb.rel.diff.future ~ precip.total*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
p.dbh.mean.test <- lme(agb.rel.diff.future ~ precip.total*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ precip.total*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.diff.future ~ precip.total*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ precip.total*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
p.density.tree.test <- lme(agb.rel.diff.future ~ precip.total*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.density.tree.test <- lme(agb.rel.diff.future ~ precip.total*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
p.height.mean.test <- lme(agb.rel.diff.future ~ precip.total*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.height.mean.test <- lme(agb.rel.diff.future ~ precip.total*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
p.height.sd.test <- lme(agb.rel.diff.future ~ precip.total*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

p.MNG.height.sd.test <- lme(agb.rel.diff.future ~ precip.total*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#Temperature as the weather metric
#-----------------------------------------------------------------------------------#
tair.test <- lme(agb.rel.diff.future ~ tair*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.test <- lme(agb.rel.diff.future ~ tair*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
tair.agb.test <- lme(agb.rel.diff.future ~ tair*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.agb.test <- lme(agb.rel.diff.future ~ tair*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
tair.dbh.mean.test <- lme(agb.rel.diff.future ~ tair*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ tair*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
tair.dbh.sd.test <- lme(agb.rel.diff.future ~ tair*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ tair*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
tair.density.tree.test <- lme(agb.rel.diff.future ~ tair*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.density.tree.test <- lme(agb.rel.diff.future ~ tair*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
tair.height.mean.test <- lme(agb.rel.diff.future ~ tair*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.height.mean.test <- lme(agb.rel.diff.future ~ tair*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
tair.height.sd.test <- lme(agb.rel.diff.future ~ tair*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

tair.MNG.height.sd.test <- lme(agb.rel.diff.future ~ tair*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#VPD as the weather metric
#-----------------------------------------------------------------------------------#
VPD.test <- lme(agb.rel.diff.future ~ VPD*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.test <- lme(agb.rel.diff.future ~ VPD*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
VPD.agb.test <- lme(agb.rel.diff.future ~ VPD*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.agb.test <- lme(agb.rel.diff.future ~ VPD*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
VPD.dbh.mean.test <- lme(agb.rel.diff.future ~ VPD*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ VPD*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
VPD.dbh.sd.test <- lme(agb.rel.diff.future ~ VPD*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ VPD*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
VPD.density.tree.test <- lme(agb.rel.diff.future ~ VPD*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.density.tree.test <- lme(agb.rel.diff.future ~ VPD*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
VPD.height.mean.test <- lme(agb.rel.diff.future ~ VPD*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.height.mean.test <- lme(agb.rel.diff.future ~ VPD*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
VPD.height.sd.test <- lme(agb.rel.diff.future ~ VPD*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

VPD.MNG.height.sd.test <- lme(agb.rel.diff.future ~ VPD*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#Precipitation as the weather metric
#-----------------------------------------------------------------------------------#
rel.p.test <- lme(agb.rel.diff.future ~ rel.precip*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.test <- lme(agb.rel.diff.future ~ rel.precip*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
rel.p.agb.test <- lme(agb.rel.diff.future ~ rel.precip*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.agb.test <- lme(agb.rel.diff.future ~ rel.precip*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
rel.p.dbh.mean.test <- lme(agb.rel.diff.future ~ rel.precip*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ rel.precip*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
rel.p.dbh.sd.test <- lme(agb.rel.diff.future ~ rel.precip*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ rel.precip*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
rel.p.density.tree.test <- lme(agb.rel.diff.future ~ rel.precip*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.density.tree.test <- lme(agb.rel.diff.future ~ rel.precip*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
rel.p.height.mean.test <- lme(agb.rel.diff.future ~ rel.precip*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.height.mean.test <- lme(agb.rel.diff.future ~ rel.precip*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
rel.p.height.sd.test <- lme(agb.rel.diff.future ~ rel.precip*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.p.MNG.height.sd.test <- lme(agb.rel.diff.future ~ rel.precip*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#Temperature as the weather metric
#-----------------------------------------------------------------------------------#
diff.tair.test <- lme(agb.rel.diff.future ~ diff.tair*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.test <- lme(agb.rel.diff.future ~ diff.tair*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
diff.tair.agb.test <- lme(agb.rel.diff.future ~ diff.tair*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.agb.test <- lme(agb.rel.diff.future ~ diff.tair*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
diff.tair.dbh.mean.test <- lme(agb.rel.diff.future ~ diff.tair*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ diff.tair*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
diff.tair.dbh.sd.test <- lme(agb.rel.diff.future ~ diff.tair*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ diff.tair*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
diff.tair.density.tree.test <- lme(agb.rel.diff.future ~ diff.tair*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.density.tree.test <- lme(agb.rel.diff.future ~ diff.tair*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
diff.tair.height.mean.test <- lme(agb.rel.diff.future ~ diff.tair*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.height.mean.test <- lme(agb.rel.diff.future ~ diff.tair*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
diff.tair.height.sd.test <- lme(agb.rel.diff.future ~ diff.tair*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

diff.tair.MNG.height.sd.test <- lme(agb.rel.diff.future ~ diff.tair*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#VPD as the weather metric
#-----------------------------------------------------------------------------------#
rel.VPD.test <- lme(agb.rel.diff.future ~ rel.VPD*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#AGB
rel.VPD.agb.test <- lme(agb.rel.diff.future ~ rel.VPD*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.agb.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*agb*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#DBH mean
rel.VPD.dbh.mean.test <- lme(agb.rel.diff.future ~ rel.VPD*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.dbh.mean.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*tree.dbh.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of DBH section
rel.VPD.dbh.sd.test <- lme(agb.rel.diff.future ~ rel.VPD*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.dbh.sd.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*tree.dbh.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Tree density
rel.VPD.density.tree.test <- lme(agb.rel.diff.future ~ rel.VPD*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.density.tree.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*density.tree*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#Height mean
rel.VPD.height.mean.test <- lme(agb.rel.diff.future ~ rel.VPD*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.height.mean.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*tree.height.mean*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

#SD of height
rel.VPD.height.sd.test <- lme(agb.rel.diff.future ~ rel.VPD*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")

rel.VPD.MNG.height.sd.test <- lme(agb.rel.diff.future ~ rel.VPD*Management*tree.height.sd*rcp, random=list(GCM=~1), data = runs.late, method = "ML")


models <- list(agb.test, MNG.agb.test, dbh.mean.test, MNG.dbh.mean.test, dbh.sd.test, MNG.dbh.sd.test, density.tree.test, MNG.density.tree.test,
               height.mean.test, MNG.height.mean.test, height.sd.test, MNG.height.sd.test, 
               p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test,
               tair.test, tair.agb.test, tair.MNG.test, tair.MNG.agb.test, tair.dbh.sd.test, tair.MNG.dbh.sd.test, tair.density.tree.test, tair.MNG.density.tree.test,
               tair.height.sd.test, tair.MNG.height.sd.test, tair.height.mean.test, tair.MNG.height.mean.test, tair.dbh.mean.test, tair.MNG.dbh.mean.test,
               VPD.test, VPD.agb.test, VPD.MNG.test, VPD.MNG.agb.test, VPD.dbh.sd.test, VPD.MNG.dbh.sd.test, VPD.density.tree.test, VPD.MNG.density.tree.test,
               VPD.height.sd.test, VPD.MNG.height.sd.test, VPD.height.mean.test, VPD.MNG.height.mean.test, VPD.dbh.mean.test, VPD.MNG.dbh.mean.test,
               rel.p.test, rel.p.agb.test, rel.p.MNG.test, rel.p.MNG.agb.test, rel.p.dbh.sd.test, rel.p.MNG.dbh.sd.test, rel.p.density.tree.test, rel.p.MNG.density.tree.test,
               rel.p.height.sd.test, rel.p.MNG.height.sd.test, rel.p.height.mean.test, rel.p.MNG.height.mean.test, rel.p.dbh.mean.test, rel.p.MNG.dbh.mean.test,
               diff.tair.test, diff.tair.agb.test, diff.tair.MNG.test, diff.tair.MNG.agb.test, diff.tair.dbh.sd.test, diff.tair.MNG.dbh.sd.test, diff.tair.density.tree.test, diff.tair.MNG.density.tree.test,
               diff.tair.height.sd.test, diff.tair.MNG.height.sd.test, diff.tair.height.mean.test, diff.tair.MNG.height.mean.test, diff.tair.dbh.mean.test, diff.tair.MNG.dbh.mean.test,
               rel.VPD.test, rel.VPD.agb.test, rel.VPD.MNG.test, rel.VPD.MNG.agb.test, rel.VPD.dbh.sd.test, rel.VPD.MNG.dbh.sd.test, rel.VPD.density.tree.test, rel.VPD.MNG.density.tree.test,
               rel.VPD.height.sd.test, rel.VPD.MNG.height.sd.test, rel.VPD.height.mean.test, rel.VPD.MNG.height.mean.test, rel.VPD.dbh.mean.test, rel.VPD.MNG.dbh.mean.test) 


model.names <- c('agb', 'MNG*agb', 'dbh,mean', 'MNG*dbh.mean', 'dbh.sd.test', 'MNG*dbh.sd', 'density.tree', 'MNG*density.tree',
                 'height.mean', 'MNG*height.mean', 'height.sd', 'MNG*height.sd',
                 'precip','precip*agb', 'precip*MNG', 'precip*MNG*agb', 'precip*dbh.sd', 'precip*MNG*dbh.sd', 'precip*density.tree', 'precip*MNG*density.tree',
                 'precip*height.sd', 'precip*MNG*height.sd', 'precip*height.mean', 'precip*MNG*height.mean', 'precip*dbh.mean', 'precip*MNG*dbh.mean',
                 'Air.temp','Air.temp*agb', 'Air.temp*MNG', 'Air.temp*MNG*agb', 'Air.temp*dbh.sd', 'Air.temp*MNG*dbh.sd', 'Air.temp*density.tree', 'Air.temp*MNG*density.tree',
                 'Air.temp*height.sd', 'Air.temp*MNG*height.sd', 'Air.temp*height.mean', 'Air.temp*MNG*height.mean', 'Air.temp*dbh.mean', 'Air.temp*MNG*dbh.mean',
                 'VPD','VPD*agb', 'VPD*MNG', 'VPD*MNG*agb', 'VPD*dbh.sd', 'VPD*MNG*dbh.sd', 'VPD*density.tree', 'VPD*MNG*density.tree',
                 'VPD*height.sd', 'VPD*MNG*height.sd', 'VPD*height.mean', 'VPD*MNG*height.mean', 'VPD*dbh.mean', 'VPD*MNG*dbh.mean',
                 'Rel.precip','Rel.precip*agb', 'Rel.precip*MNG', 'Rel.precip*MNG*agb', 'Rel.precip*dbh.sd', 'Rel.precip*MNG*dbh.sd', 'Rel.precip*density.tree', 'Rel.precip*MNG*density.tree',
                 'Rel.precip*height.sd', 'Rel.precip*MNG*height.sd', 'Rel.precip*height.mean', 'Rel.precip*MNG*height.mean', 'Rel.precip*dbh.mean', 'Rel.precip*MNG*dbh.mean',
                 'Diff.air.temp','Diff.air.temp*agb', 'Diff.air.temp*MNG', 'Diff.air.temp*MNG*agb', 'Diff.air.temp*dbh.sd', 'Diff.air.temp*MNG*dbh.sd', 'Diff.air.temp*density.tree', 'Diff.air.temp*MNG*density.tree',
                 'Diff.air.temp*height.sd', 'Diff.air.temp*MNG*height.sd', 'Diff.air.temp*height.mean', 'Diff.air.temp*MNG*height.mean', 'Diff.air.temp*dbh.mean', 'Diff.air.temp*MNG*dbh.mean',
                 'Rel.VPD','Rel.VPD*agb', 'Rel.VPD*MNG', 'Rel.VPD*MNG*agb', 'Rel.VPD*dbh.sd', 'Rel.VPD*MNG*dbh.sd', 'Rel.VPD*density.tree', 'Rel.VPD*MNG*density.tree',
                 'Rel.VPD*height.sd', 'Rel.VPD*MNG*height.sd', 'Rel.VPD*height.mean', 'Rel.VPD*MNG*height.mean', 'Rel.VPD*dbh.mean', 'Rel.VPD*MNG*dbh.mean') 


diff.aic <- aictab(models, model.names)

diff.aic

write.csv(diff.aic, "../data/Full_AIC.csv", row.names=F)

#BIC of all models
diff.bic <-  bictab(models, model.names)

diff.bic

#Seems we have a minor preference for VPD instead of relative VPD. They are close enoguh I think we can pick off of story

#-------------------------------------------------#
# Actual model runs
#-------------------------------------------------#
#VPD, management, and height sd
#THIS IS OUR BEST MODEL
VPD.agb.MNG <- lme(agb.rel.diff.future ~ VPD*Management*agb, random=list(GCM=~1), data = runs.late, method = "ML")
summary(VPD.agb.MNG)
anova(VPD.agb.MNG)
plot(VPD.agb.MNG)

VPD.dbh.sd.MNG <- lme(agb.rel.diff.future ~ VPD*Management*tree.dbh.sd, random=list(GCM=~1), data = runs.late, method = "ML")
summary(VPD.dbh.sd.MNG)
anova(VPD.dbh.sd.MNG)
plot(VPD.dbh.sd.MNG)

VPD.density.tree.MNG <- lme(agb.rel.diff.future ~ VPD*Management*density.tree, random=list(GCM=~1), data = runs.late, method = "ML")
summary(VPD.density.tree.MNG )
anova(VPD.density.tree.MNG )
plot(VPD.density.tree.MNG )




#--------------------------------------------------------------#
#Working with our binomial crash framework AIC
#--------------------------------------------------------------#
library(lme4)

path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

runs.comb$loss.event.20 <- ifelse(runs.comb$agb.rel.diff.future <= -.20, 1, 0)

#Counting individual instances of a crash beginning
for(i in 5:nrow(runs.comb)){
  DRIVE <- runs.comb[i, "Driver.set"]
  MNG <- runs.comb[i, "Management"]
  YR <- runs.comb[i, "year"]
  if(YR != 2007){
    prev.20 <- runs.comb[runs.comb$Driver.set == DRIVE & runs.comb$Management == MNG & runs.comb$year == YR-1 , "loss.event.20"]
    runs.comb[i, "nonseq.loss.event.20"] <- ifelse((runs.comb[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

runs.new <- data.frame()
for(GCM in unique(runs.comb$GCM)){
  for(RCP in unique(runs.comb$rcp[runs.comb$GCM==GCM])){
    for(MGMT in unique(runs.comb$Management[runs.comb$GCM==GCM & runs.comb$rcp==RCP])){
      temp <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp==RCP & runs.comb$Management == MGMT,]
      count <- 0
      for(i in 3:(nrow(temp)-1)){
        if(temp[i, "nonseq.loss.event.20"]==T){
          temp[i, "time.since.crash"] <- count
          count <- 0
        } else{
          count <- count + 1
          temp[i, "time.since.crash"] <- count
        }
      }
      runs.new <- rbind(runs.new, temp)
    }
  }
}


runs.late <- runs.new[runs.new$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff.future),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.late$tair.c <- runs.late$tair - 273.15
#----------------------------------------------------#
# Setting up the AIC
#----------------------------------------------------#
#Structure on its own and with Management
#AGB
agb.test <- glmer(nonseq.loss.event.20 ~ agb + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH
dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH
dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height
height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd + (1|rcp:GCM) + (1|time.since.crash), data = runs.late, family = binomial)


#----------------------------------------------------#
# Now we are working with precip (We are doing our relative precip because otherwise the models scale is off)
#----------------------------------------------------#
#AGB
relp.agb.test <- glmer(nonseq.loss.event.20 ~ agb*rel.precip + (1|rcp:GCM) + (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH
relp.dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean*rel.precip + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH
relp.dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd*rel.precip + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
relp.density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree*rel.precip + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height - having issue with this one
relp.height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean*rel.precip + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
relp.height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd*rel.precip + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#----------------------------------------------------#
# Now we are working with VPD
#----------------------------------------------------#
#AGB
VPD.agb.test <- glmer(nonseq.loss.event.20 ~ agb*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH
VPD.dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH
VPD.dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
VPD.density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height - having issue with this one
VPD.height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
VPD.height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd*I(VPD/1000) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#----------------------------------------------------#
# Now we are working with VPD
#----------------------------------------------------#
#AGB
relVPD.agb.test <- glmer(nonseq.loss.event.20 ~ agb*rel.VPD + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH
relVPD.dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean*rel.VPD  + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH
relVPD.dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd*rel.VPD  + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
relVPD.density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree*rel.VPD  + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height - having issue with this one
relVPD.height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean*rel.VPD  + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
relVPD.height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd*rel.VPD  + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#----------------------------------------------------#
# Now we are working with temp
#----------------------------------------------------#
#AGB
difftair.agb.test <- glmer(nonseq.loss.event.20 ~ agb*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH - trouble with this one
difftair.dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH - trouble with this one
difftair.dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
difftair.density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height
difftair.height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
difftair.height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd*diff.tair + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)


models <- list(agb.test, dbh.mean.test,  dbh.sd.test, density.tree.test,
               height.mean.test,  height.sd.test, relp.agb.test, relp.dbh.mean.test, relp.dbh.sd.test, relp.density.tree.test,
               relp.height.mean.test, relp.height.sd.test, VPD.agb.test, VPD.dbh.mean.test, VPD.dbh.sd.test, VPD.density.tree.test,
               VPD.height.mean.test, VPD.height.sd.test, relVPD.agb.test, relVPD.dbh.mean.test, relVPD.dbh.sd.test, relVPD.density.tree.test,
               relVPD.height.mean.test, relVPD.height.sd.test, difftair.agb.test, difftair.dbh.mean.test, difftair.dbh.sd.test, difftair.density.tree.test,
               difftair.height.mean.test, difftair.height.sd.test)

model.names <- c('agb', 'dbh,mean', 'dbh.sd.test','density.tree',
                 'height.mean', 'height.sd', 'relp.agb', 'relp.dbh.mean', 'relp.dbh.sd', 'relp.density.tree',
                 'relp.height.mean', 'relp.height.sd', 'VPD.agb', 'VPD.dbh.mean', 'VPD.dbh.sd', 'VPD.density.tree', 
                 'VPD.height.mean', 'VPD.height.sd', 'relVPD.agb', 'relVPD.dbh.mean', 'relVPD.dbh.sd', 'relVPD.density.tree', 
                 'relVPD.height.mean', 'relVPD.height.sd', 'difftair.agb', 'difftair.dbh.mean', 'difftair.dbh.sd', 'difftair.density.tree', 
                 'difftair.height.mean', 'difftair.height.sd') 


diff.aic <- aictab(models, model.names)

diff.aic

write.csv(diff.aic, "../data/Pcrash_Full_AIC.csv", row.names=F)

#BIC of all models
diff.bic <-  bictab(models, model.names)

diff.bic


#----------------------------------------------------#
# Now we are working with precip (We are doing our relative precip because otherwise the models scale is off)
#----------------------------------------------------#
#AGB
p.agb.test <- glmer(nonseq.loss.event.20 ~ agb*log(precip.total) + (1|rcp:GCM) + (1|time.since.crash), data = runs.late, family = binomial)

#Mean DBH
p.dbh.mean.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.mean*log(precip.total) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of DBH
p.dbh.sd.test <- glmer(nonseq.loss.event.20 ~ tree.dbh.sd*log(precip.total) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Tree density
p.density.tree.test <- glmer(nonseq.loss.event.20 ~ density.tree*log(precip.total) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#Mean height - having issue with this one
p.height.mean.test <- glmer(nonseq.loss.event.20 ~ tree.height.mean*log(precip.total) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

#SD of height
p.height.sd.test <- glmer(nonseq.loss.event.20 ~ tree.height.sd*log(precip.total) + (1|rcp:GCM)+ (1|time.since.crash), data = runs.late, family = binomial)

