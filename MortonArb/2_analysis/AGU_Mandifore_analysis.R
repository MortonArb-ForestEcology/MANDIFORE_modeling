#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script organizes our model data, runs AIC model selection, and does linear regression
# Inputs: Arboretum weather data and MANDIFORE arboretum case study data
# Outputs: 
# Notes: This is a compact script that will likely be broken up down the line
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

#--------------------------------------------------#
# This top section doesn't need to be run everytime. Read in the processed data in the next section
#--------------------------------------------------#
path.reg <- "../figures/regression/"
if(!dir.exists(path.reg)) dir.create(path.reg, recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "agb", sep=""))) dir.create(paste(path.reg, "agb", sep=""), recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "lai", sep=""))) dir.create(paste(path.reg, "lai", sep=""), recursive=T, showWarnings = F)

#Reading in weather data
dat.precip <- read.csv("../Precip_Weather_Daily.csv")

dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$mean),]

#Tracking days without rain
dat.precip$no.rain <- ifelse(dat.precip$sum == 0, 1 ,0)

dat.year <- aggregate(sum~year+model+scenario, dat.precip, FUN = sum)

dat.year$rainless.days <- aggregate(no.rain~year+model+scenario, dat.precip, FUN = sum)[, "no.rain"]

#Reading in the Mandifore data
path.read <- "../data/"

runs.all <- read.csv(paste0(path.read, "All_runs.csv"))

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

#Aggregating values by mean
runs.year <- aggregate(cbind(tair, VPD, agb, lai, npp,soil.moist.deep, soil.moist.surf ,
                             density.tree, height.sd, height.mean, dbh.mean, dbh.sd)~year+Management+GCM+rcp, runs.all, FUN = mean)

#Joining the precipitation metrics and mandifore output with one dataframe
runs.comb <- merge(runs.year, dat.year, by.x = c("year", "GCM", "rcp"), by.y = c("year", "model", "scenario"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

#Calculating our agb metrics for evaluation
for(i in 2:nrow(runs.comb)){
  
  #Difference between current year and previous year
  GCM <- runs.comb[i, "GCM"]
  rcp <- runs.comb[i, "rcp"]
  MNG <- runs.comb[i, "Management"]
  Year <- runs.comb[i, "year"]
  
  #Calculating the average weather for the first 12 years of model run
  mean.precip <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "sum"])
  mean.VPD <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "VPD"])
  mean.tair <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "tair"])
  
  #Calculating the relative difference in a metric from the mean
  runs.comb[i, "rel.precip"] <- (runs.comb[i, "sum"] - mean.precip)/mean.precip
  runs.comb[i, "rel.VPD"] <- (runs.comb[i, "VPD"] - mean.VPD)/mean.VPD
  runs.comb[i, "rel.tair"] <- (runs.comb[i, "tair"] - mean.tair)/mean.tair
  
  if(Year > 2006 & Year < 2099){
    
    #relative change in precip from one year to the next
    runs.comb[i, "diff.precip"] <- (runs.comb[i, "sum"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"]
    
    #Change in agb
    runs.comb[i, "agb.diff"] <- runs.comb[i, "agb"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"]  
    
    #Relative change in agb
    runs.comb[i, "agb.rel.diff"] <- (runs.comb[i, "agb"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"] 
    
    #Relative future change in agb by 1 year
    runs.comb[i, "agb.rel.lag1"] <- (runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"] - runs.comb[i, "agb"])/runs.comb[i, "agb"]  
    
    #Section for same calulation but lai. Currently not really used
    runs.comb[i, "lai.diff"] <- runs.comb[i, "lai"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
    
    runs.comb[i, "lai.rel.diff"] <- (runs.comb[i, "lai"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
    
    runs.comb[i, "lai.lag"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "lai"] - runs.comb[i, "lai"] 
    
  }
  
}

#Removing any years before/during management
runs.late <- runs.comb[runs.comb$year >= 2025, ]

write.csv(runs.late, paste0(path.read, "All_runs_yearly.csv"))

#--------------------------------------------------------------#
#Reading in our data if we have it already
#--------------------------------------------------------------#
path.read <- "../data/"

runs.late <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

runs.late$Management <- factor(runs.late$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.late$log.agb.rel.diff <- log(runs.late$agb.rel.diff + abs(min(runs.late$agb.rel.diff)) + 1)

runs.late$log.agb.diff <- log(runs.late$agb.diff + abs(min(runs.late$agb.diff)) + 1)

#------------------------------------------------------------------------#
#AIC to determine our best model
#------------------------------------------------------------------------#

library(AICcmodavg)

#Just relative precip
p.test <- lme(agb.rel.diff ~ rel.precip, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

#Just VPD
VPD.test <- lme(agb.rel.diff ~ VPD, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.test)
anova(VPD.test)

#Just relative VPD
rel.VPD.test <- lme(agb.rel.diff ~ rel.VPD, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.VPD.test)
anova(rel.VPD.test)

#Just relative temp
tair.test <- lme(agb.rel.diff ~ rel.tair, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.test)
anova(tair.test)

#Just Management
MNG.test <- lme(agb.rel.diff ~ Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(MNG.test)
anova(MNG.test)

#Just height.sd
height.sd.test <- lme(agb.rel.diff ~ height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(height.sd.test)
anova(height.sd.test)

#relative precip and management
p.MNG.test <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#VPD and management
VPD.MNG.test <- lme(agb.rel.diff ~ VPD*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.MNG.test)
anova(VPD.MNG.test)

#Relative vpd and management
rel.VPD.MNG.test <- lme(agb.rel.diff ~ rel.VPD*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.VPD.MNG.test)
anova(rel.VPD.MNG.test)

#Relative temp and management
tair.MNG.test <- lme(agb.rel.diff ~ rel.tair*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.MNG.test)
anova(tair.MNG.test)


#precip and height sd
p.height.sd.test <- lme(agb.rel.diff ~ rel.precip*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)

#VPD and height.sd
VPD.height.sd.test <- lme(agb.rel.diff ~ VPD*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.height.sd.test)
anova(VPD.height.sd.test)

#relative VPD and height sd
rel.VPD.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.VPD.height.sd.test)
anova(rel.VPD.height.sd.test)

#relative temp and height.sd
tair.height.sd.test <- lme(agb.rel.diff ~ rel.tair*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.height.sd.test)
anova(tair.height.sd.test)

#relative precip, mangement, and height.sd
p.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.precip*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#VPD, mangement, and height.sd
VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.MNG.height.sd.test)
anova(VPD.MNG.height.sd.test)

#relative VPD, mangement, and height.sd
rel.VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.VPD.MNG.height.sd.test)
anova(rel.VPD.MNG.height.sd.test)

#relative tair, mangement, and height.sd
tair.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.tair*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.MNG.height.sd.test)
anova(tair.MNG.height.sd.test)

#relative precip, relative VPD, and relative temp
tair.VPD.p.test <- lme(agb.rel.diff ~ rel.precip*rel.VPD*rel.tair, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(tair.VPD.p.test)
anova(tair.VPD.p.test)

models <- list(p.test, VPD.test, rel.VPD.test, tair.test, MNG.test, height.sd.test, p.MNG.test, VPD.MNG.test, rel.VPD.MNG.test,
               tair.MNG.test, p.height.sd.test, VPD.height.sd.test, rel.VPD.height.sd.test, tair.height.sd.test,
               p.MNG.height.sd.test, VPD.MNG.height.sd.test, rel.VPD.MNG.height.sd.test, tair.MNG.height.sd.test, tair.VPD.p.test)

model.names <- c('Precip', 'VPD', 'Relative.VPD', 'Air.temp', 'Manage', 'Height.sd', 'Precip*Manage', 'VPD*Manage', 'Relative.VPD*Manage',
                 'Air.temp*Manage', 'Precip*height.sd', 'VPD*height.sd', 'Relative*VPD.height.sd', 'Air.temp*height.sd',
                 'Precip*Manage*height.sd', 'VPD*Manage*height.sd', 'Relative.VPD*Manage*height.sd', 'Air.temp*Manage*height.sd', 'Air.temp*VPD*Precip')

both.diff.aic <- aictab(models, model.names)

both.diff.aic

#BIC of all models
both.diff.bic <-  bictab(models, model.names)

both.diff.bic

#Seems we have a minor preference for VPD instead of relative VPD. They are close enoguh I think we can pick off of story

#-------------------------------------------------#
# Actual model runs
#-------------------------------------------------#

#Just precip and management with relative precip
rel.MNG.agb.both <- lme(agb.rel.diff ~ rel.precip*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.MNG.agb.both)
anova(rel.MNG.agb.both)
plot(rel.MNG.agb.both)


#Just precip and management with relative precip
rel.MNG.agb.both <- lme(agb.rel.diff ~ VPD*Management, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(rel.MNG.agb.both)
anova(rel.MNG.agb.both)
plot(rel.MNG.agb.both)


#precip, management, and height sd
#THIS IS OUR BEST MODEL
VPD.height.sd.MNG <- lme(agb.rel.diff ~ VPD*Management*height.sd, random=list(GCM=~1, rcp=~1), data = runs.late, method = "ML")
summary(VPD.height.sd.MNG)
anova(VPD.height.sd.MNG)
plot(VPD.height.sd.MNG)

#------------------------------------------------------------------------#
# FIGURES SECTION
#------------------------------------------------------------------------#
path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Outline Figures"

library(ggplot2)

#Relative agb change vs VPD by management
png(width= 750, filename= file.path(path.figures, paste0('Relative_Agb_Change_to_VPD_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Relative Change in above ground biomass (AGB) to VPD increases by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = agb.rel.diff, color = rcp))+
  ylab("Relative change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Relative agb change vs time by management
png(width= 750, filename= file.path(path.figures, paste0('Relative_Agb_Change_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Relative Change in above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb.rel.diff, color = GCM, linetype = rcp))+
  ylab("Relative change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#AGB over time by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb, color = GCM, linetype = rcp))+
  ylab("AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#VPD increasing over time
png(width= 750, filename= file.path(path.figures, paste0('VPD_Increasing.png')))
ggplot(data=runs.late)+
  ggtitle("CMIP5 models show increasing VPD across two different emisisons scenarios")+
  facet_wrap(~rcp)+
  geom_point(aes(x = year, y= VPD, color = GCM))+
  geom_smooth(aes(x = year, y= VPD, color = GCM))+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#AGB over height sd by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=height.sd, y = agb, color = rcp))+
  ylab("AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Relative AGB over height sd by management
png(width= 800, filename= file.path(path.figures, paste0('Relative_change_in_AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Relative change in above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = height.sd, color = rcp))+
  ylab("Relative change in AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 14, face = "bold"))
dev.off()

#height sd vs VPD by Management
png(width= 750, filename= file.path(path.figures, paste0('Height_sd_vs_VPD_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Standard Deviation of Height vs. Vapor Pressure Deficit by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = height.sd, color = rcp))+
  ylab("SD of tree height")+
  xlab("VPD")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#height sd over time by Management
png(width= 750, filename= file.path(path.figures, paste0('Height_sd_vs_time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Standard Deviation of Height over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = height.sd, color= GCM, linetype = rcp))+
  ylab("SD of tree height")+
  xlab("year")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Boxplot I'm still figuring out
png(width= 750, filename= file.path(path.figures, paste0('Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Standard Deviation of Height by Management")+
  #facet_wrap(~Management)+
  geom_boxplot(aes(x=Management, y =height.sd))+
  ylab("Standard deviation of height")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


png(width= 750, filename= file.path(path.figures, paste0('Relative_change_in_Agb_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Relative change in agb by Management")+
  #facet_wrap(~Management)+
  geom_boxplot(aes(x=Management, y =agb.rel.diff))+
  ylab("Relative change in agb")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

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


