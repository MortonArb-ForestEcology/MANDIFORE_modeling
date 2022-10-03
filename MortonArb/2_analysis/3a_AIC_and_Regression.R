#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection, does linear regression, and creates figures
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: This script does not include any figures relating to loss event characterization
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)

path.reg <- "../figures/regression/"
if(!dir.exists(path.reg)) dir.create(path.reg, recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "agb", sep=""))) dir.create(paste(path.reg, "agb", sep=""), recursive=T, showWarnings = F)

#--------------------------------------------------------------#
#Reading in our data if we have it already
#--------------------------------------------------------------#
path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

#------------------------------------------------------------------------#
#AIC to determine our best model
#------------------------------------------------------------------------#

library(AICcmodavg)

#Just relative precip
p.test <- lme(agb.rel.diff ~ rel.precip, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(p.test)
anova(p.test)

#Just VPD
VPD.test <- lme(agb.rel.diff ~ VPD, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.test)
anova(VPD.test)

#Just relative VPD
rel.VPD.test <- lme(agb.rel.diff ~ rel.VPD, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(rel.VPD.test)
anova(rel.VPD.test)

#Just relative temp
tair.test <- lme(agb.rel.diff ~ rel.tair, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(tair.test)
anova(tair.test)

#Just Management
MNG.test <- lme(agb.rel.diff ~ Management, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(MNG.test)
anova(MNG.test)

#Just height.sd
height.sd.test <- lme(agb.rel.diff ~ height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(height.sd.test)
anova(height.sd.test)

#relative precip and management
p.MNG.test <- lme(agb.rel.diff ~ rel.precip*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(p.MNG.test)
anova(p.MNG.test)

#VPD and management
VPD.MNG.test <- lme(agb.rel.diff ~ VPD*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.MNG.test)
anova(VPD.MNG.test)

#Relative vpd and management
rel.VPD.MNG.test <- lme(agb.rel.diff ~ rel.VPD*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(rel.VPD.MNG.test)
anova(rel.VPD.MNG.test)

#Relative temp and management
tair.MNG.test <- lme(agb.rel.diff ~ rel.tair*Management, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(tair.MNG.test)
anova(tair.MNG.test)


#precip and height sd
p.height.sd.test <- lme(agb.rel.diff ~ rel.precip*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(p.height.sd.test)
anova(p.height.sd.test)

#VPD and height.sd
VPD.height.sd.test <- lme(agb.rel.diff ~ VPD*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.height.sd.test)
anova(VPD.height.sd.test)

#relative VPD and height sd
rel.VPD.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(rel.VPD.height.sd.test)
anova(rel.VPD.height.sd.test)

#relative temp and height.sd
tair.height.sd.test <- lme(agb.rel.diff ~ rel.tair*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(tair.height.sd.test)
anova(tair.height.sd.test)

#relative precip, mangement, and height.sd
p.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.precip*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(p.MNG.height.sd.test)
anova(p.MNG.height.sd.test)

#VPD, mangement, and height.sd
VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ VPD*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.MNG.height.sd.test)
anova(VPD.MNG.height.sd.test)

#relative VPD, mangement, and height.sd
rel.VPD.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.VPD*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(rel.VPD.MNG.height.sd.test)
anova(rel.VPD.MNG.height.sd.test)

#relative tair, mangement, and height.sd
tair.MNG.height.sd.test <- lme(agb.rel.diff ~ rel.tair*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(tair.MNG.height.sd.test)
anova(tair.MNG.height.sd.test)

#relative precip, relative VPD, and relative temp
tair.VPD.p.test <- lme(agb.rel.diff ~ rel.precip*rel.VPD*rel.tair, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(tair.VPD.p.test)
anova(tair.VPD.p.test)

models <- list(p.test, VPD.test, rel.VPD.test, tair.test, MNG.test, height.sd.test, p.MNG.test, VPD.MNG.test, rel.VPD.MNG.test,
               tair.MNG.test, p.height.sd.test, VPD.height.sd.test, rel.VPD.height.sd.test, tair.height.sd.test,
               p.MNG.height.sd.test, VPD.MNG.height.sd.test, rel.VPD.MNG.height.sd.test, tair.MNG.height.sd.test, tair.VPD.p.test)

model.names <- c('Precip', 'VPD', 'Proportional.VPD', 'Air.temp', 'Manage', 'Height.sd', 'Precip*Manage', 'VPD*Manage', 'Proportional.VPD*Manage',
                 'Air.temp*Manage', 'Precip*height.sd', 'VPD*height.sd', 'Proportional.VPD*height.sd', 'Air.temp*height.sd',
                 'Precip*Manage*height.sd', 'VPD*Manage*height.sd', 'Proportional.VPD*Manage*height.sd', 'Air.temp*Manage*height.sd', 'Air.temp*VPD*Precip')

both.diff.aic <- aictab(models, model.names)

both.diff.aic

#BIC of all models
both.diff.bic <-  bictab(models, model.names)

both.diff.bic

#Seems we have a minor preference for VPD instead of relative VPD. They are close enoguh I think we can pick off of story

#-------------------------------------------------#
# Actual model runs
#-------------------------------------------------#
#VPD, management, and height sd
#THIS IS OUR BEST MODEL
VPD.agb.MNG <- lme(agb.rel.diff ~ VPD*Management*agb, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.agb.MNG)
anova(VPD.agb.MNG)
plot(VPD.agb.MNG)

VPD.dbh.sd.MNG <- lme(agb.rel.diff ~ VPD*Management*dbh.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.dbh.sd.MNG)
anova(VPD.dbh.sd.MNG)
plot(VPD.dbh.sd.MNG)

VPD.height.sd.MNG <- lme(agb.rel.diff ~ VPD*Management*height.sd, random=list(Driver.set=~1), data = runs.late, method = "ML")
summary(VPD.height.sd.MNG)
anova(VPD.height.sd.MNG)
plot(VPD.height.sd.MNG)
#------------------------------------------------------------------------#
# FIGURES SECTION
#------------------------------------------------------------------------#
path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/Outline Figures"

library(ggplot2)
cbPalette  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Proportional agb change vs VPD by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_to_VPD_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) to VPD increases by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = agb.rel.diff, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Proportional agb change vs agb by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_to_AGB_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) to current AGB increases by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=agb, y = agb.rel.diff, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Proportional agb change vs time by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb.rel.diff, group = Driver.set, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#AGB over time by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb, group = Driver.set, color = rcp))+
  ylab("AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Example using a single GCM
png(width= 750, filename= file.path(path.figures, paste0('Sinlge_GCM_AGB_Over_Time_by_Management.png')))
ggplot(data=runs.late[runs.late$Driver.set == "BNU-ESM.rcp45",])+
  ggtitle("Example: Above ground biomass (AGB) over time by Management for BNU-ESM.rcp45")+
  #facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb, color = Management))+
  ylab("AGB")+
  theme(plot.title = element_text(size = 10, face = "bold"))
dev.off()


pdf(file= file.path(path.figures, paste0('AGB_Over_Time_Individual_GCM.pdf')))
for(DRIVE in unique(runs.late$Driver.set)){
  temp.fig <- ggplot(data=runs.late[runs.late$Driver.set == DRIVE,])+
    ggtitle(paste0(DRIVE,": Above ground biomass (AGB) over time by Management"))+
    #facet_wrap(~Management)+
    geom_line(aes(x=year, y = agb, color = Management))+
    ylab("AGB")+
    theme(plot.title = element_text(size = 10, face = "bold"))
  print(temp.fig)
}
dev.off()

runs.late$total.precip <- runs.late$sum
runs.late$air.temp <- runs.late$tair
runs.long <- tidyr::gather(runs.late, var, values, air.temp, total.precip, VPD, factor_key=TRUE)

#VPD increasing over time
png(width= 750, filename= file.path(path.figures, paste0('Weather_Changing.png')))
ggplot(data=runs.long)+
  ggtitle("CMIP5 models weather metrics across two different emisisons scenarios")+
  facet_grid(var~rcp, scales = "free")+
  geom_point(aes(x = year, y= values, color = GCM))+
  geom_smooth(aes(x = year, y= values, color = GCM))+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#AGB over height sd by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=height.sd, y = agb, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=height.sd, y=agb, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Proportional AGB over height sd by management
png(width= 800, filename= file.path(path.figures, paste0('Proportional_change_in_AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=height.sd, y = agb.rel.diff, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=height.sd, y=agb.rel.diff, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("Proportional change in AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 14, face = "bold"))
dev.off()


#Proportional AGB over AGB by management
png(width= 800, filename= file.path(path.figures, paste0('Proportional_change_in_AGB_vs_AGB_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in above ground biomass (AGB) vs. AGB by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=agb, y = agb.rel.diff, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=agb, y=agb.rel.diff, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("Proportional change in AGB")+
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
  geom_line(aes(x=year, y = height.sd, group= Driver.set, color = rcp))+
  ylab("SD of tree height")+
  xlab("year")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Distribution of height.sd
png(width= 750, filename= file.path(path.figures, paste0('Dist_of_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Distribution of Standard Deviation of Height by Management")+
  facet_wrap(~Management)+
  geom_histogram(aes(x =height.sd))+
  ylab("Frequency")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Distribution of proportional change in agb
png(width= 750, filename= file.path(path.figures, paste0('Proportional_change_in_Agb_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in agb by Management")+
  facet_wrap(~Management)+
  geom_histogram(aes(x =agb.rel.diff))+
  xlab("Proportional change in agb")+
  ylab("Frequency")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Boxplot section
runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

box.df <- data.frame()
for(DRIVE in unique(runs.comb$Driver.set)){
  for(MNG in unique(runs.comb$Management)){
    temp <- runs.comb[runs.comb$Driver.set == DRIVE & runs.comb$Management == MNG, ]
    temp$height.sd.diff <- mean(temp[temp$year >= 2089, "height.sd"]) - mean(temp[temp$year <= 2017, "height.sd"])
    temp$dbh.sd.diff <-  mean(temp[temp$year >= 2089, "dbh.sd"]) - mean(temp[temp$year <= 2017, "dbh.sd"])
    temp$agb.change <-  mean(temp[temp$year >= 2089, "agb"]) - mean(temp[temp$year <= 2017, "agb"])
    out.df <- data.frame(unique(temp$Driver.set),unique(temp$Management), unique(temp$height.sd.diff), unique(temp$dbh.sd.diff), unique(temp$agb.change))
    colnames(out.df) <- c("Driver.set","Management" ,"height.sd.diff", "dbh.sd.diff", "agb.change")
    box.df <- rbind(box.df, out.df)
  }
}

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_height_sd_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in SD of tree height from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=height.sd.diff))+
  ylab("SD of tree height")+
  xlab("Management")
dev.off()

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_dbh_sd_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in SD of DBH from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=dbh.sd.diff))+
  ylab("SD of DBH")+
  xlab("Management")
dev.off()

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_AGB_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in AGB from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=agb.change))+
  ylab("Aboveground biomass")+
  xlab("Management")
dev.off()