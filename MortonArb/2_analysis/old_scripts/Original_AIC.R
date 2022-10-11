#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

path.read <- "../data/"

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.late$log.agb.rel.diff <- log(runs.late$agb.rel.diff + abs(min(runs.late$agb.rel.diff)) + 1)

runs.late$log.agb.diff <- log(runs.late$agb.diff + abs(min(runs.late$agb.diff)) + 1)

hist(runs.late$agb.diff)
hist(runs.late$agb.rel.diff)
hist(runs.late$agb.lag)
hist(runs.late$agb.rel.lag)
hist(runs.late$log.agb.diff)
hist(runs.late$log.agb.rel.diff)

#--------------------------------#
#Here we look at the best random effects to use
#--------------------------------#

p.agb.GCM <- lme(agb.diff ~ sum*agb, random=list(GCM=~1), data = runs.late)


p.agb.rcp <- lme(agb.diff ~ sum*agb, random=list(rcp=~1), data = runs.late)


p.agb.MNG <- lme(agb.diff ~ sum*agb, random=list(Management =~1), data = runs.late)

p.agb.GCM.rcp <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late)


p.agb.rcp.MNG <- lme(agb.diff ~ sum*agb, random=list(rcp=~1, Management=~1), data = runs.late)


p.agb.GCM.MNG <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, Management =~1), data = runs.late)


p.agb.GCM.rcp.MNG <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1, Management=~1), data = runs.late)

p.agb.GCM.rcp <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late)

p.agb <- lme(agb.diff ~ agb, random=list(year=~1), data = runs.late)

models <- list(p.agb.GCM,p.agb.rcp,p.agb.MNG,p.agb.GCM.rcp ,p.agb.rcp.MNG,p.agb.GCM.MNG, p.agb.GCM.rcp.MNG, p.agb.GCM.rcp.yr, p.agb, p.agb.yr, p.GCM.rcp.yr )

model.names <- c('p.agb.GCM','p.agb.rcp','p.agb.MNG','p.agb.GCM.rcp' ,'p.agb.rcp.MNG','p.agb.GCM.MNG', 'p.agb.GCM.rcp.MNG', 
                 'p.agb')

random.aic <- aictab(models, model.names)

random.aic

#BIC of all models
random.bic <-  bictab(models, model.names)

random.bic

#-------------------------------------------------------------------------#
#Comparing predictive models that include structual variable
#-------------------------------------------------------------------------#


#AGB section
p.test <- lme(agb.diff ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.agb.test <- lme(agb.diff ~ sum*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.agb.test)
anova(p.agb.test)
plot(p.agb.test)

p.MNG.agb.test <- lme(agb.diff ~ sum*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.test)
anova(p.MNG.agb.test)
plot(p.MNG.agb.test)

p.MNG.test <- lme(agb.diff ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)
plot(p.MNG.test)


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

#AGB section
p.test <- lme(agb.lag ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

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
plot(p.MNG.dbh.mean.test)


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




#--------------------------------------------------#
#Here we start with relative precip
#--------------------------------------------------#


#--------------------------------#
#Here we look at the best random effects to use
#--------------------------------#

p.agb.GCM <- lme(agb.diff ~ rel.precip*agb, random=list(GCM=~1), data = runs.late)


p.agb.rcp <- lme(agb.diff ~ rel.precip*agb, random=list(rcp=~1), data = runs.late)


p.agb.MNG <- lme(agb.diff ~ rel.precip*agb, random=list(Management =~1), data = runs.late)


p.agb.GCM.rcp <- lme(agb.diff ~ rel.precip*agb, random=list(GCM=~1, rcp=~1), data = runs.late)


p.agb.rcp.MNG <- lme(agb.diff ~ rel.precip*agb, random=list(rcp=~1, Management=~1), data = runs.late)


p.agb.GCM.MNG <- lme(agb.diff ~ rel.precip*agb, random=list(GCM=~1, Management =~1), data = runs.late)


p.agb.GCM.rcp.MNG <- lme(agb.diff ~ rel.precip*agb, random=list(GCM=~1, rcp=~1, Management=~1), data = runs.late)

p.agb <- lme(agb.diff ~ agb, random=list(year=~1), data = runs.late)

models <- list(p.agb.GCM,p.agb.rcp,p.agb.MNG,p.agb.GCM.rcp ,p.agb.rcp.MNG,p.agb.GCM.MNG, p.agb.GCM.rcp.MNG, p.agb.GCM.rcp.yr, p.agb, p.agb.yr, p.GCM.rcp.yr )

model.names <- c('p.agb.GCM','p.agb.rcp','p.agb.MNG','p.agb.GCM.rcp' ,'p.agb.rcp.MNG','p.agb.GCM.MNG', 'p.agb.GCM.rcp.MNG', 
                 'p.agb')
relp.random.aic <- aictab(models, model.names)

relp.random.aic

#BIC of all models
relp.random.bic <-  bictab(models, model.names)

relp.random.bic

#-------------------------------------------------------------------------#
#Comparing predictive models that include structual variable
#-------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.diff ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.agb.test <- lme(agb.diff ~ rel.precip*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.agb.test)
anova(p.agb.test)

p.MNG.agb.test <- lme(agb.diff ~ rel.precip*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.test)
anova(p.MNG.agb.test)

p.MNG.test <- lme(agb.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)
plot(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.diff ~ rel.precip*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.diff ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.diff ~ rel.precip*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.diff ~ rel.precip*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.diff ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.diff ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.diff ~ rel.precip*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.diff ~ rel.precip*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.diff ~ rel.precip*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.diff ~ rel.precip*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)

models <- list(p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip','p.agb', 'p.MNG', 'p.MNG.agb', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

relp.diff.aic <- aictab(models, model.names)

relp.diff.aic

#BIC of all models
relp.diff.bic <-  bictab(models, model.names)

relp.diff.bic

#---------------------------------------------------------------------------------------------#
# Now working with the lag in agb
#---------------------------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.lag ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.agb.test <- lme(agb.lag ~ rel.precip*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.agb.test)
anova(p.agb.test)

p.MNG.agb.test <- lme(agb.lag ~ rel.precip*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.test)
anova(p.MNG.agb.test)

p.MNG.test <- lme(agb.lag ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.lag ~ rel.precip*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.lag ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.lag ~ rel.precip*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.lag ~ rel.precip*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.lag ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.lag ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.lag ~ rel.precip*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.lag ~ rel.precip*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.lag ~ rel.precip*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.lag ~ rel.precip*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)

models <- list(p.test, p.agb.test, p.MNG.test, p.MNG.agb.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip','p.agb', 'p.MNG', 'p.MNG.agb', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

relp.lag.aic <- aictab(models, model.names)

relp.lag.aic


#BIC of all models
relp.lag.bic <-  bictab(models, model.names)

relp.lag.bic




#---------------------------------------------------------------------#
#Now we have relative agb
#---------------------------------------------------------------------#

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

#--------------------------------#
#Here we look at the best random effects to use
#--------------------------------#

p.GCM <- lme(agb.rel.diff ~ sum, random=list(GCM=~1), data = runs.late)


p.rcp <- lme(agb.rel.diff ~ sum, random=list(rcp=~1), data = runs.late)


p.MNG <- lme(agb.rel.diff ~ sum, random=list(Management =~1), data = runs.late)


p.GCM.rcp <- lme(agb.rel.diff ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late)


p.rcp.MNG <- lme(agb.rel.diff ~ sum, random=list(rcp=~1, Management=~1), data = runs.late)


p.GCM.MNG <- lme(agb.rel.diff ~ sum, random=list(GCM=~1, Management =~1), data = runs.late)


p.GCM.rcp.MNG <- lme(agb.rel.diff ~ sum, random=list(GCM=~1, rcp=~1, Management=~1), data = runs.late)

models <- list(p.GCM, p.rcp, p.MNG, p.GCM.rcp ,p.rcp.MNG, p.GCM.MNG, p.GCM.rcp.MNG, p.GCM.rcp.yr, p.yr)

model.names <- c('p.GCM','p.rcp','p.MNG','p.GCM.rcp' ,'p.rcp.MNG','p.GCM.MNG', 'p.GCM.rcp.MNG')

relagb.random.aic <- aictab(models, model.names)

relagb.random.aic

#BIC of all models
relagb.random.bic <-  bictab(models, model.names)

relagb.random.bic

#-------------------------------------------------------------------------#
#Comparing predictive models that include structual variable
#-------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.rel.diff ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.MNG.test <- lme(agb.rel.diff ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)
plot(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.diff ~ sum*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.rel.diff ~ sum*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.rel.diff ~ sum*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.rel.diff ~ sum*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.rel.diff ~ sum*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.rel.diff ~ sum*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.rel.diff ~ sum*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.rel.diff ~ sum*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.rel.diff ~ sum*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.rel.diff ~ sum*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)

models <- list(p.test, p.MNG.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip', 'p.MNG', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

relagb.diff.aic <- aictab(models, model.names)

relagb.diff.aic

#BIC of all models
relagb.diff.bic <-  bictab(models, model.names)

relagb.diff.bic
#---------------------------------------------------------------------------------------------#
# Now working with the lag in agb
#---------------------------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.rel.lag ~ sum, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.MNG.test <- lme(agb.rel.lag ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.lag ~ sum*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.rel.lag ~ sum*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.rel.lag ~ sum*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.rel.lag ~ sum*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.rel.lag ~ sum*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.rel.lag ~ sum*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.rel.lag ~ sum*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.rel.lag ~ sum*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.rel.lag ~ sum*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.rel.lag ~ sum*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)

models <- list(p.test, p.MNG.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip', 'p.MNG', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

relagb.lag.aic <- aictab(models, model.names)

relagb.lag.aic


#BIC of all models
relagb.lag.bic <-  bictab(models, model.names)

relagb.lag.bic





#----------------------------------------------------------------------------------------------------------------------#
#Here we use both realtive effects
#------------------------------------------#

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

#--------------------------------#
#Here we look at the best random effects to use
#--------------------------------#

p.GCM <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1), data = runs.late)


p.rcp <- lme(agb.rel.diff ~ rel.precip, random=list(rcp=~1), data = runs.late)


p.MNG <- lme(agb.rel.diff ~ rel.precip, random=list(Management =~1), data = runs.late)


p.GCM.rcp <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late)


p.rcp.MNG <- lme(agb.rel.diff ~ rel.precip, random=list(rcp=~1, Management=~1), data = runs.late)


p.GCM.MNG <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, Management =~1), data = runs.late)


p.GCM.rcp.MNG <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, rcp=~1, Management=~1), data = runs.late)

models <- list(p.GCM,p.rcp,p.MNG,p.GCM.rcp ,p.rcp.MNG,p.GCM.MNG, p.GCM.rcp.MNG)

model.names <- c('p.GCM','p.rcp','p.MNG','p.GCM.rcp' ,'p.rcp.MNG','p.GCM.MNG', 'p.GCM.rcp.MNG')

both.random.aic <- aictab(models, model.names)

both.random.aic

#BIC of all models
both.random.bic <-  bictab(models, model.names)

both.random.bic

#-------------------------------------------------------------------------#
#Comparing predictive models that include structual variable
#-------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)
plot(p.test)

p.MNG.test <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)
plot(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.diff ~ rel.precip*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.rel.diff ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.rel.diff ~ rel.precip*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.rel.diff ~ rel.precip*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.rel.diff ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)
plot(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)
plot(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.rel.diff ~ rel.precip*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.rel.diff ~ rel.precip*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.rel.diff ~ rel.precip*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.rel.diff ~ rel.precip*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)



models <- list(p.test, p.MNG.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip', 'p.MNG', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

both.diff.aic <- aictab(models, model.names)

both.diff.aic

#BIC of all models
both.diff.bic <-  bictab(models, model.names)

both.diff.bic


#---------------------------------------------------------------------------------------------#
# Now working with the lag in agb
#---------------------------------------------------------------------------------------------#
library(AICcmodavg)

#AGB section
p.test <- lme(agb.rel.lag ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

p.MNG.test <- lme(agb.rel.lag ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#SD of DBH section
p.dbh.sd.test <- lme(agb.rel.lag ~ rel.precip*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.test)
anova(p.dbh.sd.test)

p.MNG.dbh.sd.test <- lme(agb.rel.lag ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.sd.test)
anova(p.MNG.dbh.sd.test)


#SD of tree density
p.density.tree.test <- lme(agb.rel.lag ~ rel.precip*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.density.tree.test)
anova(p.density.tree.test)


p.MNG.density.tree.test <- lme(agb.rel.lag ~ rel.precip*Management*density.tree, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.density.tree.test)
anova(p.MNG.density.tree.test)

#SD of height
p.height.sd.test <- lme(agb.rel.lag ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)


p.MNG.height.sd.test <- lme(agb.rel.lag ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#Height mean
p.height.mean.test <- lme(agb.rel.lag ~ rel.precip*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.mean.test)
anova(p.height.mean.test)

p.MNG.height.mean.test <- lme(agb.rel.lag ~ rel.precip*Management*height.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.mean.test)
anova(p.MNG.height.mean.test)

#DBH mean
p.dbh.mean.test <- lme(agb.rel.lag ~ rel.precip*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.mean.test)
anova(p.dbh.mean.test)

p.MNG.dbh.mean.test <- lme(agb.rel.lag ~ rel.precip*Management*dbh.mean, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.dbh.mean.test)
anova(p.MNG.dbh.mean.test)



models <- list(p.test, p.MNG.test, p.dbh.sd.test, p.MNG.dbh.sd.test, p.density.tree.test, p.MNG.density.tree.test,
               p.height.sd.test, p.MNG.height.sd.test, p.height.mean.test, p.MNG.height.mean.test, p.dbh.mean.test, p.MNG.dbh.mean.test) 
#p.GCM.agb.test, p.GCM.test, p.MNG.GCM.test, p.agb.MNG.GCM.test)

model.names <- c('precip', 'p.MNG', 'p.dbh.sd', 'p.MNG.dbh.sd', 'p.density.tree', 'p.MNG.density.tree',
                 'p.height.sd', 'p.MNG.height.sd', 'p.height.mean', 'p.MNG.height.mean', 'p.dbh.mean', 'p.MNG.dbh.mean') 
#'p.GCM.agb', 'p.GCM', 'p.MNG.GCM', 'p.agb.MNG.GCM')

both.lag.aic <- aictab(models, model.names)

both.lag.aic


#BIC of all models
both.lag.bic <-  bictab(models, model.names)

both.lag.bic


#-------------------------------------------------------------------------#
# Evaluation of different aic metrics
#-------------------------------------------------------------------------#
#To clarify the original aic's use dagb and total yearly precip
#The "relp" aic's use dagb and relative yearly precip
#The "relagb" aic's use relative dagb and total yearly precip
#The "both" use both relative dagb and total yearly precip

#Looking at which are the best random effects to include for each style
#GCM and RCP are always at the top, but it's weight compared to GCM RCP and MNG shifts depending on using relative agb
#This makes sense since the main idea of using relative metrics is that it lessens the impact of differences in GCM
#However when only precipitation is made relative it doesn't really change the weights. Guess that means th rain is more similar than the agb across GCM's
random.aic; relp.random.aic; relagb.random.aic; both.random.aic

#The major differences between these options occur when using relative agb. There seems to be no real difference between 
#the relative precipitation and standard precipitation. Using relative agb gives the same top model "p.MNG.agb" and "p.agb"
#The order of the other models rankings changes to more structural variables, although this doesn't really change our final result of the best model
diff.aic; relp.diff.aic; relagb.diff.aic; both.diff.aic

p.MNG.agb.both <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)

p.height.sd.MNG <- lme(agb.rel.diff ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.MNG)
anova(p.height.sd.MNG)
plot(p.height.sd.MNG)

p.dbh.sd.MNG <- lme(agb.rel.diff ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.dbh.sd.MNG)
anova(p.dbh.sd.MNG)
plot(p.dbh.sd.MNG)

p.MNG.agb.both <- lme(agb.rel.lag1 ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)

p.MNG.agb.both <- lme(agb.rel.lag3 ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)

p.MNG.agb.both <- lme(agb.rel.lag5 ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)

p.MNG.agb.both <- lme(agb.rel.lag10 ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)

library(ggplot2)
ggplot(data=runs.late)+
        geom_point(aes(x= rel.precip, y = agb.rel.diff, color = Management))

runs.lag <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.lag <- runs.late[!is.na(runs.late$agb.rel.lag10),]

lag.MNG.agb.both <- lme(agb.rel.lag ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(lag.MNG.agb.both)
anova(lag.MNG.agb.both)
plot(lag.MNG.agb.both)

lag.height.sd.MNG <- lme(agb.rel.lag ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(lag.height.sd.MNG)
anova(lag.height.sd.MNG)
plot(lag.height.sd.MNG)

lag.dbh.sd.MNG <- lme(agb.rel.lag ~ rel.precip*Management*dbh.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(lag.dbh.sd.MNG)
anova(lag.dbh.sd.MNG)
plot(lag.dbh.sd.MNG)


plot(resid(p.MNG.agb.both)~ predict(p.MNG.agb.both))
abline(h=0, col="red")

hist(runs.late$agb.rel.diff)     
hist(runs.late$rel.precip)
hist(resid(p.MNG.agb.both))


plot(p.MNG.agb.both, type=c("p","smooth"), col.line=1)

plot(p.MNG.agb.orig, type=c("p","smooth"), col.line=1)

plot(p.MNG.agb.orig,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

plot(p.MNG.agb.rel.precip,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

plot(p.MNG.rel.agb,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

plot(p.MNG.agb.both,
     sqrt(abs(resid(.)))~fitted(.),
     type=c("p","smooth"), col.line=1)

qqnorm(p.MNG.agb.orig)
qqnorm(p.MNG.agb.both)

#----------------------------------------------------------#
# Now looking at a log transformation
#----------------------------------------------------------#


#runs.late <- runs.late[!is.na(runs.late$log.agb.rel.diff),]
#runs.late <- runs.late[!is.na(runs.late$log.agb.diff),]

#Looking at an aic comparision when using a time lag the results are very different.
#The basic style uses "p.MNG.agb" like the others but then all the relative metrics "p.height.sd" with "p.MNG.height.sd" as second 
lag.aic; relp.lag.aic; relagb.lag.aic; both.lag.aic

p.MNG.agb.orig <- lme(agb.lag ~ sum*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.orig)
anova(p.MNG.agb.orig)
plot(p.MNG.agb.orig)

p.MNG.rel.agb <- lme(agb.rel.lag ~ sum*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.rel.agb)
anova(p.MNG.rel.agb)
plot(p.MNG.rel.agb)

p.MNG.agb.rel.precip <- lme(agb.lag ~ rel.precip*Management*agb, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.rel.precip)
anova(p.MNG.agb.rel.precip)
plot(p.MNG.agb.rel.precip)

p.MNG.agb.both <- lme(agb.rel.lag ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.agb.both)
anova(p.MNG.agb.both)
plot(p.MNG.agb.both)


random.bic; relp.random.bic; relagb.random.bic; both.random.bic
diff.bic; relp.diff.bic; relagb.diff.bic; both.diff.bic;
lag.bic; relp.lag.bic; relagb.lag.bic; both.lag.bic



random.aic; random.bic; diff.aic; diff.bic; lag.aic; lag.bic 
relp.random.aic; relp.random.bic; relp.diff.aic; relp.diff.bic; relp.lag.aic; relp.lag.bic
relagb.random.aic; relagb.random.bic; relagb.diff.aic; relagb.diff.bic; relagb.lag.aic; relagb.lag.bic
both.random.aic; both.random.bic; both.diff.aic; both.diff.bic; both.lag.aic; both.lag.bic

