library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.late$Driver.set <- paste0(runs.late$GCM,"." ,runs.late$rcp)

#----------------------------------------------------#
# Setting up the AIC
#----------------------------------------------------#
#Structure on its own and with Management
#AGB
agb.test <- lme(agb.rel.diff ~ agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.agb.test <- lme(agb.rel.diff ~ Management*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Mean DBH
dbh.mean.test <- lme(agb.rel.diff ~ dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.dbh.mean.test <- lme(agb.rel.diff ~ Management*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of DBH
dbh.sd.test <- lme(agb.rel.diff ~ dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.dbh.sd.test <- lme(agb.rel.diff ~ Management*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Tree density
density.tree.test <- lme(agb.rel.diff ~ density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.density.tree.test <- lme(agb.rel.diff ~ Management*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Mean height
height.mean.test <- lme(agb.rel.diff ~ height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.height.mean.test <- lme(agb.rel.diff ~ Management*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of height
height.sd.test <- lme(agb.rel.diff ~ height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

MNG.height.sd.test <- lme(agb.rel.diff ~ Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

#-----------------------------------------------------------------------------------#
#Precipitation as the weather metric
#-----------------------------------------------------------------------------------#
p.test <- lme(agb.rel.diff ~ sum, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.test <- lme(agb.rel.diff ~ sum*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")

#AGB
p.agb.test <- lme(agb.rel.diff ~ sum*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.agb.test <- lme(agb.rel.diff ~ sum*Management*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

#DBH mean
p.dbh.mean.test <- lme(agb.rel.diff ~ sum*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.dbh.mean.test <- lme(agb.rel.diff ~ sum*Management*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.diff ~ sum*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.dbh.sd.test <- lme(agb.rel.diff ~ sum*Management*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Tree density
p.density.tree.test <- lme(agb.rel.diff ~ sum*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.density.tree.test <- lme(agb.rel.diff ~ sum*Management*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Height mean
p.height.mean.test <- lme(agb.rel.diff ~ sum*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.height.mean.test <- lme(agb.rel.diff ~ sum*Management*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of height
p.height.sd.test <- lme(agb.rel.diff ~ sum*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

p.MNG.height.sd.test <- lme(agb.rel.diff ~ sum*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#Temperature as the weather metric
#-----------------------------------------------------------------------------------#
tair.test <- lme(agb.rel.diff ~ tair, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.test <- lme(agb.rel.diff ~ tair*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")

#AGB
tair.agb.test <- lme(agb.rel.diff ~ tair*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.agb.test <- lme(agb.rel.diff ~ tair*Management*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

#DBH mean
tair.dbh.mean.test <- lme(agb.rel.diff ~ tair*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.dbh.mean.test <- lme(agb.rel.diff ~ tair*Management*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of DBH section
tair.dbh.sd.test <- lme(agb.rel.diff ~ tair*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.dbh.sd.test <- lme(agb.rel.diff ~ tair*Management*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Tree density
tair.density.tree.test <- lme(agb.rel.diff ~ tair*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.density.tree.test <- lme(agb.rel.diff ~ tair*Management*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Height mean
tair.height.mean.test <- lme(agb.rel.diff ~ tair*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.height.mean.test <- lme(agb.rel.diff ~ tair*Management*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of height
tair.height.sd.test <- lme(agb.rel.diff ~ tair*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

tair.MNG.height.sd.test <- lme(agb.rel.diff ~ tair*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")


#-----------------------------------------------------------------------------------#
#VPD as the weather metric
#-----------------------------------------------------------------------------------#
VPD.test <- lme(agb.rel.diff ~ VPD, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.test <- lme(agb.rel.diff ~ VPD*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")

#AGB
VPD.agb.test <- lme(agb.rel.diff ~ VPD*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.agb.test <- lme(agb.rel.diff ~ VPD*Management*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")

#DBH mean
VPD.dbh.mean.test <- lme(agb.rel.diff ~ VPD*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.dbh.mean.test <- lme(agb.rel.diff ~ VPD*Management*dbh.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of DBH section
VPD.dbh.sd.test <- lme(agb.rel.diff ~ VPD*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.dbh.sd.test <- lme(agb.rel.diff ~ VPD*Management*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Tree density
VPD.density.tree.test <- lme(agb.rel.diff ~ VPD*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.density.tree.test <- lme(agb.rel.diff ~ VPD*Management*density.tree, random=list(Driver.set=~1), data = runs.late, method = "ML")

#Height mean
VPD.height.mean.test <- lme(agb.rel.diff ~ VPD*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.height.mean.test <- lme(agb.rel.diff ~ VPD*Management*height.mean, random=list(Driver.set=~1), data = runs.late, method = "ML")

#SD of height
VPD.height.sd.test <- lme(agb.rel.diff ~ VPD*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")

VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ VPD*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")


models <- list(agb.test, MNG.agb.test, dbh.mean.test, MNG.dbh.mean.test, dbh.sd.test, MNG.dbh.sd.test, density.tree.test, MNG.density.tree.test,
               height.mean.test, MNG.height.mean.test, height.sd.test, MNG.height.sd.test, 
               p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test,
               tair.test, tair.agb.test, tair.MNG.test, tair.MNG.agb.test, tair.dbh.sd.test, tair.MNG.dbh.sd.test, tair.density.tree.test, tair.MNG.density.tree.test,
               tair.height.sd.test, tair.MNG.height.sd.test, tair.height.mean.test, tair.MNG.height.mean.test, tair.dbh.mean.test, tair.MNG.dbh.mean.test,
               VPD.test, VPD.agb.test, VPD.MNG.test, VPD.MNG.agb.test, VPD.dbh.sd.test, VPD.MNG.dbh.sd.test, VPD.density.tree.test, VPD.MNG.density.tree.test,
               VPD.height.sd.test, VPD.MNG.height.sd.test, VPD.height.mean.test, VPD.MNG.height.mean.test, VPD.dbh.mean.test, VPD.MNG.dbh.mean.test) 


model.names <- c('agb', 'MNG*agb', 'dbh,mean', 'MNG*dbh.mean', 'dbh.sd.test', 'MNG*dbh.sd', 'density.tree', 'MNG*density.tree',
                 'height.mean', 'MNG*height.mean', 'height.sd', 'MNG*height.sd',
                 'precip','precip*agb', 'precip*MNG', 'precip*MNG*agb', 'precip*dbh.sd', 'precip*MNG*dbh.sd', 'precip*density.tree', 'precip*MNG*density.tree',
                 'precip*height.sd', 'precip*MNG*height.sd', 'precip*height.mean', 'precip*MNG*height.mean', 'precip*dbh.mean', 'precip*MNG*dbh.mean',
                 'Air.temp','Air.temp*agb', 'Air.temp*MNG', 'Air.temp*MNG*agb', 'Air.temp*dbh.sd', 'Air.temp*MNG*dbh.sd', 'Air.temp*density.tree', 'Air.temp*MNG*density.tree',
                 'Air.temp*height.sd', 'Air.temp*MNG*height.sd', 'Air.temp*height.mean', 'Air.temp*MNG*height.mean', 'Air.temp*dbh.mean', 'Air.temp*MNG*dbh.mean',
                 'VPD','VPD*agb', 'VPD*MNG', 'VPD*MNG*agb', 'VPD*dbh.sd', 'VPD*MNG*dbh.sd', 'VPD*density.tree', 'VPD*MNG*density.tree',
                 'VPD*height.sd', 'VPD*MNG*height.sd', 'VPD*height.mean', 'VPD*MNG*height.mean', 'VPD*dbh.mean', 'VPD*MNG*dbh.mean') 


diff.aic <- aictab(models, model.names)

diff.aic

#BIC of all models
diff.bic <-  bictab(models, model.names)

diff.bic