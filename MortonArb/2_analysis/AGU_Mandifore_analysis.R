library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

path.reg <- "../figures/regression/"
if(!dir.exists(path.reg)) dir.create(path.reg, recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "agb", sep=""))) dir.create(paste(path.reg, "agb", sep=""), recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "lai", sep=""))) dir.create(paste(path.reg, "lai", sep=""), recursive=T, showWarnings = F)

dat.precip <- read.csv("../Precip_Weather_Daily.csv")

dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$mean),]

dat.precip$no.rain <- ifelse(dat.precip$sum == 0, 1 ,0)

dat.year <- aggregate(sum~year+model+scenario, dat.precip, FUN = sum)

dat.year$rainless.days <- aggregate(no.rain~year+model+scenario, dat.precip, FUN = sum)[, "no.rain"]

path.read <- "../data/"

runs.all <- read.csv(paste0(path.read, "All_runs.csv"))

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

runs.year <- aggregate(cbind(tair, VPD, agb, lai, npp,soil.moist.deep, soil.moist.surf ,
                             density.tree, height.sd, height.mean, dbh.mean, dbh.sd)~year+Management+GCM+rcp, runs.all, FUN = mean)

runs.comb <- merge(runs.year, dat.year, by.x = c("year", "GCM", "rcp"), by.y = c("year", "model", "scenario"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))


for(i in 2:nrow(runs.comb)){
  #Difference between current year and previous year
  GCM <- runs.comb[i, "GCM"]
  rcp <- runs.comb[i, "rcp"]
  MNG <- runs.comb[i, "Management"]
  Year <- runs.comb[i, "year"]
  
  mean.precip <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "sum"])
  mean.VPD <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "VPD"])
  mean.tair <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "tair"])
  
  
  runs.comb[i, "rel.precip"] <- (runs.comb[i, "sum"] - mean.precip)/mean.precip
  runs.comb[i, "rel.VPD"] <- (runs.comb[i, "VPD"] - mean.VPD)/mean.VPD
  runs.comb[i, "rel.tair"] <- (runs.comb[i, "tair"] - mean.tair)/mean.tair
  
  if(Year > 2006 & Year < 2099){
    
    runs.comb[i, "diff.precip"] <- (runs.comb[i, "sum"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"]
    
    runs.comb[i, "agb.diff"] <- runs.comb[i, "agb"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"]  
    
    runs.comb[i, "agb.rel.diff"] <- (runs.comb[i, "agb"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"] 
    
    runs.comb[i, "agb.rel.lag1"] <- (runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"] - runs.comb[i, "agb"])/runs.comb[i, "agb"]  
    
    #Shifting agb up a year for easier regression
    runs.comb[i, "future.agb"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"]
    
    runs.comb[i, "lai.diff"] <- runs.comb[i, "lai"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
    
    runs.comb[i, "lai.rel.diff"] <- (runs.comb[i, "lai"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
    
    runs.comb[i, "lai.lag"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "lai"] - runs.comb[i, "lai"] 
    
    #Shifting agb up a year for easier regression
    runs.comb[i, "future.lai"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "lai"]
  }
  
}

runs.late <- runs.comb[runs.comb$year >= 2025, ]

write.csv(runs.late, paste0(path.read, "All_runs_yearly.csv"))

#--------------------------------------------------------------#
#Reading in our data if we have it already
#--------------------------------------------------------------#
path.read <- "../data/"

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

#------------------------------------------------------------------------#
#AIC to determine our best model
#------------------------------------------------------------------------#

library(AICcmodavg)

#AGB section
p.test <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

VPD.test <- lme(agb.rel.diff ~ rel.VPD, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.test)
anova(VPD.test)

tair.test <- lme(agb.rel.diff ~ rel.tair, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.test)
anova(tair.test)

MNG.test <- lme(agb.rel.diff ~ Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(MNG.test)
anova(MNG.test)

height.sd.test <- lme(agb.rel.diff ~ height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(height.sd.test)
anova(height.sd.test)

p.MNG.test <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

VPD.MNG.test <- lme(agb.rel.diff ~ rel.VPD*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.MNG.test)
anova(VPD.MNG.test)

tair.MNG.test <- lme(agb.rel.diff ~ rel.tair*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.MNG.test)
anova(tair.MNG.test)


#SD of height
p.height.sd.test <- lme(agb.rel.diff ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)

VPD.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.height.sd.test)
anova(VPD.height.sd.test)

tair.height.sd.test <- lme(agb.rel.diff ~ rel.tair*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.height.sd.test)
anova(tair.height.sd.test)


p.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.MNG.height.sd.test)
anova(VPD.MNG.height.sd.test)

tair.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.tair*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.MNG.height.sd.test)
anova(tair.MNG.height.sd.test)

tair.VPD.p.test <- lme(agb.rel.diff ~ rel.precip*rel.VPD*rel.tair, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.VPD.p.test)
anova(tair.VPD.p.test)

models <- list(p.test, VPD.test, tair.test, MNG.test, height.sd.test, p.MNG.test, VPD.MNG.test, tair.MNG.test, p.height.sd.test, VPD.height.sd.test, tair.height.sd.test,
               p.MNG.height.sd.test, VPD.MNG.height.sd.test, tair.MNG.height.sd.test, tair.VPD.p.test)

model.names <- c('p.test', 'VPD.test', 'tair.test', 'MNG.test', 'height.sd.test', 'p.MNG.test', 'VPD.MNG.test', 'tair.MNG.test', 'p.height.sd.test', 'VPD.height.sd.test', 'tair.height.sd.test',
                 'p.MNG.height.sd.test', 'VPD.MNG.height.sd.test', 'tair.MNG.height.sd.test', 'tair.VPD.p.test')

both.diff.aic <- aictab(models, model.names)

both.diff.aic

#BIC of all models
both.diff.bic <-  bictab(models, model.names)

both.diff.bic

#-------------------------------------------------#
# Actual model runs
#-------------------------------------------------#

#Just precip and management with relative precip
rel.MNG.agb.both <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.MNG.agb.both)
anova(rel.MNG.agb.both)
plot(rel.MNG.agb.both)

#Just precip and management with relative precip
rel.MNG.agb.both <- lme(agb.rel.diff ~ rel.VPD*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.MNG.agb.both)
anova(rel.MNG.agb.both)
plot(rel.MNG.agb.both)


#precip, management, and height sd
#THIS IS OUR BEST MODEL
rel.height.sd.MNG <- lme(agb.rel.diff ~ rel.VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.height.sd.MNG)
anova(rel.height.sd.MNG)
plot(rel.height.sd.MNG)


library(ggplot2)
ggplot(data=runs.late)+
  facet_wrap(~Management)+
  geom_point(aes(x= rel.VPD, y = agb.rel.diff, color = Management))

ggplot(data=runs.late)+
  facet_wrap(~Management)+
  geom_point(aes(x= rel.VPD, y = height.sd, color = Management))

ggplot(data=runs.late)+
  geom_point(aes(x = year, y= VPD, color = Management))+
  geom_smooth(aes(x = year, y= VPD, color = Management))


ggplot(data=runs.late)+
  geom_point(aes(x= rel.precip, y = agb.rel.diff, color = Management))

ggplot(data=runs.late)+
  geom_point(aes(x= rel.precip, y = agb.rel.lag5, color = Management))

#------------------------------------------------#
#Lag section
#------------------------------------------------#

runs.lag <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.lag <- runs.late[!is.na(runs.late$agb.rel.lag1),]

runs.lag$Management <- factor(runs.lag$Management, levels = c("None", "Gap", "Shelter", "Under"))

#Lag of one year
p.MNG.agb.lag1 <- lme(agb.rel.lag1 ~ rel.VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.lag, method = "ML")
summary(p.MNG.agb.lag1)
anova(p.MNG.agb.lag1)
plot(p.MNG.agb.lag1)


