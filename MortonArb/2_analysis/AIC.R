#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(ggplot2)
library(nlme)

path.read <- "../data/"

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.diff),]

#--------------------------------#
#Here we look at the best random effects to use
#--------------------------------#

p.agb <- lme(agb.diff ~ sum*agb, data = runs.late)

p.agb.GCM <- lme(agb.diff ~ sum*agb, random=list(GCM=~1), data = runs.late)


p.agb.rcp <- lme(agb.diff ~ sum*agb, random=list(rcp=~1), data = runs.late)


p.agb.MNG <- lme(agb.diff ~ sum*agb, random=list(Management =~1), data = runs.late)


p.agb.GCM.rcp <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late)


p.agb.rcp.MNG <- lme(agb.diff ~ sum*agb, random=list(rcp=~1, Management=~1), data = runs.late)


p.agb.GCM.MNG <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, Management =~1), data = runs.late)


p.agb.GCM.rcp.MNG <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1, Management=~1), data = runs.late)

models <- list(p.agb.GCM,p.agb.rcp,p.agb.MNG,p.agb.GCM.rcp ,p.agb.rcp.MNG,p.agb.GCM.MNG, p.agb.GCM.rcp.MNG )

model.names <- c('p.agb.GCM','p.agb.rcp','p.agb.MNG','p.agb.GCM.rcp' ,'p.agb.rcp.MNG','p.agb.GCM.MNG', 'p.agb.GCM.rcp.MNG')

random.aic <- aictab(models, model.names)

random.aic

#BIC of all models
random.bic <-  bictab(models, model.names)

random.bic

#-------------------------------------------------------------------------#
#Comparing predictive models that include structual variable
#-------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.diff ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

p.agb.test <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.agb.test)
anova(p.agb.test)

p.MNG.agb.test <- lme(agb.diff ~ sum*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.test)
anova(p.MNG.agb.test)

p.MNG.test <- lme(agb.diff ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.diff ~ sum*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.diff ~ sum*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.diff ~ sum*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.diff ~ sum*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.diff ~ sum*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.diff ~ sum*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.diff ~ sum*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.diff ~ sum*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.diff ~ sum*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.diff ~ sum*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)


#Models that don't include GCM as a random effect
#p.GCM.agb.test <- lme(agb.diff ~ sum*GCM*agb, random=list(rcp=~1), data = runs.late, method = "ML" )
#summary(p.GCM.agb.test)
#anova(p.GCM.agb.test)

#p.GCM.test <- lme(agb.diff ~ sum*GCM, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.GCM.test)
#anova(p.GCM.test)

#p.MNG.GCM.test <- lme(agb.diff ~ sum*Management*GCM, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.MNG.GCM.test)
#anova(p.MNG.GCM.test)

#p.agb.MNG.GCM.test <- lme(agb.diff ~ sum*Management*GCM*agb, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.agb.MNG.GCM.test)
#anova(p.agb.MNG.GCM.test)

models <- list(p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
               #p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip','p.agb', 'p.MNG', 'p.MNG.agb', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
                 #'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

diff.aic <- aictab(models, model.names)

diff.aic

#BIC of all models
diff.bic <-  bictab(models, model.names)

diff.bic

#---------------------------------------------------------------------------------------------#
# Now working with the lag in agb
#---------------------------------------------------------------------------------------------#
library(AICcmodavg)

runs.late <- runs.late[!is.na(runs.late$agb.lag),]

#AGB section
p.test <- lme(agb.lag ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

p.agb.test <- lme(agb.lag ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.agb.test)
anova(p.agb.test)

p.MNG.agb.test <- lme(agb.lag ~ sum*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.test)
anova(p.MNG.agb.test)

p.MNG.test <- lme(agb.lag ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.lag ~ sum*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.lag ~ sum*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.lag ~ sum*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.lag ~ sum*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.lag ~ sum*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.lag ~ sum*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.lag ~ sum*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.lag ~ sum*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.lag ~ sum*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.lag ~ sum*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)


#Models that don't include GCM as a random effect
#p.GCM.agb.test <- lme(agb.lag ~ sum*GCM*agb, random=list(rcp=~1), data = runs.late, method = "ML" )
#summary(p.GCM.agb.test)
#anova(p.GCM.agb.test)

#p.GCM.test <- lme(agb.lag ~ sum*GCM, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.GCM.test)
#anova(p.GCM.test)

#p.MNG.GCM.test <- lme(agb.lag ~ sum*Management*GCM, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.MNG.GCM.test)
#anova(p.MNG.GCM.test)

#p.agb.MNG.GCM.test <- lme(agb.lag ~ sum*Management*GCM*agb, random=list(rcp=~1), data = runs.late, method = "ML")
#summary(p.agb.MNG.GCM.test)
#anova(p.agb.MNG.GCM.test)

models <- list(p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
               #p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip','p.agb', 'p.MNG', 'p.MNG.agb', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
                 #'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

lag.aic <- aictab(models, model.names)

lag.aic


#BIC of all models
lag.bic <-  bictab(models, model.names)

lag.bic

