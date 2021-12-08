#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script crates plots and tables exploring drought periods impact on variables of interest
# Inputs: Drought and preicpitaiton dataframes from 1a_Drought_Analysis
# Outputs:
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(nlme)
library(ggplot2)
drought.df <- read.csv("../Resilience_dataframe.csv")

drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))

drought.df$year <- lubridate::year(drought.df$D.start)

dry.list <- list()
dry.check <- lme(flag.2sig ~ Management-1, random = list(rcp = ~1, GCM = ~1), data = drought.df)
sum <- summary(dry.check)
df.eff <- as.data.frame(sum$tTable)
df.eff$Fixedeff <- rownames(df.eff)
df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
dry.list[[paste("flag.2sig", " -1")]]$Var <- paste("flag.2sig", " -1")
dry.list[[paste("flag.2sig", " -1")]]$Fixedeff <- df.eff$Fixedeff
dry.list[[paste("flag.2sig", " -1")]]$Equation <- df.eff$Equaiton
dry.list[[paste("flag.2sig", " -1")]]$Value <- df.eff$Value
dry.list[[paste("flag.2sig", " -1")]]$pvalue <- df.eff$`p-value`

dry.check <- lme(flag.2sig ~ Management, random = list(rcp = ~1, GCM = ~1), data = drought.df)
sum <- summary(dry.check)
df.eff <- as.data.frame(sum$tTable)
df.eff$Fixedeff <- rownames(df.eff)
df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
dry.list[[paste("flag.2sig")]]$Var <- paste("flag.2sig")
dry.list[[paste("flag.2sig")]]$Fixedeff <- df.eff$Fixedeff
dry.list[[paste("flag.2sig")]]$Equation <- df.eff$Equaiton
dry.list[[paste("flag.2sig")]]$Value <- df.eff$Value
dry.list[[paste("flag.2sig")]]$pvalue <- df.eff$`p-value`
summary(dry.check)

dry.list <- list()
Dry.variables <- c("flag.2sig", #True flase on whetehr a dry period or drought occured
                        "agb.local.diff", #The difference in the lowest dip in agb over 15 months after drought
                        "agb.pcent.local.diff", #The percentage difference in the lowest dip in agb over 15 months after drought
                   "agb.min.local") #The low point after 15 months
for(COL in Dry.variables){
  #lme regression that can cycle through all of our variables of interest.
  lm.test <- lme(eval(substitute(j ~ Management-1, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df)
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  dry.list[[paste(COL, " -1")]]$Var <- paste(COL, " -1")
  dry.list[[paste(COL, " -1")]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste(COL, " -1")]]$Equation <- df.eff$Equaiton
  dry.list[[paste(COL, " -1")]]$Value <- df.eff$Value
  dry.list[[paste(COL, " -1")]]$pvalue <- df.eff$`p-value`
  
  #One set is for the general signifigance and this set is for sigfignace relative to no management
  lm.test <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df)
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  dry.list[[paste(COL)]]$Var <- paste(COL)
  dry.list[[paste(COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste(COL)]]$Equation <- df.eff$Equaiton
  dry.list[[paste(COL)]]$Value <- df.eff$Value
  dry.list[[paste(COL)]]$pvalue <- df.eff$`p-value`
}

dat.dry <- dplyr::bind_rows(dry.list)
write.csv(dat.dry, "../LME_of_dry_periods.csv", row.names = F)


var.list <- list()
Resilience.variables <- c("check.recov") #Drop in agb after a dry period
for(COL in Resilience.variables){
  #This is what lucien is checking if Management has a signifigant effect on the drop in agb
  lm.test <- lme(eval(substitute(j ~ Management-1, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df[drought.df$flag.2sig == T,])
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  var.list[[paste(COL, " -1")]]$Var <- paste(COL, " -1")
  var.list[[paste(COL, " -1")]]$Fixedeff <- df.eff$Fixedeff
  var.list[[paste(COL, " -1")]]$Equation <- df.eff$Equaiton
  var.list[[paste(COL, " -1")]]$Value <- df.eff$Value
  var.list[[paste(COL, " -1")]]$pvalue <- df.eff$`p-value`
  
  #One set is for the general signifigance and this set is for sigfignace relative to no management
  lm.test <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df[drought.df$flag.2sig == T,])
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  var.list[[paste(COL)]]$Var <- paste(COL)
  var.list[[paste(COL)]]$Fixedeff <- df.eff$Fixedeff
  var.list[[paste(COL)]]$Equation <- df.eff$Equaiton
  var.list[[paste(COL)]]$Value <- df.eff$Value
  var.list[[paste(COL)]]$pvalue <- df.eff$`p-value`
}

dat.resilience <- dplyr::bind_rows(var.list)

write.csv(dat.resilience, "../LME_of_resilience.csv", row.names = F)


var.list <- list()
Recovery.variables <- c("days_of_recovery", #How long it takes to recover
                        "agb.recov.diff", #The difference between past 10 years agb and min agb following the end of drought and before recovery
                        "agb.pcent.recov.diff", #The percentage difference between past 10 years agb and min agb following the end of drought and before recovery
                        "agb.min.recov", #The min agb following the end of drought and before recovery
                        "recov.dbh.mean.mean",
                        "recov.dbh.sd.mean",
                        "recov.height.mean.mean",
                        "recov.height.sd.mean",
                        "recov.density.tree.mean",
                        "recov.nee.mean",
                        "recov.soil.moist.surf.mean",
                        "recov.soil.moist.deep.mean") #The min nee following the lowest point of agb before recovery
for(COL in Recovery.variables){
  #lme regression that can cycle through all of our variables of interest.
  lm.test <- lme(eval(substitute(j ~ Management-1, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df[drought.df$flag.2sig == T & drought.df$check.recov == T,])
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  var.list[[paste(COL, " -1")]]$Var <- paste(COL, " -1")
  var.list[[paste(COL, " -1")]]$Fixedeff <- df.eff$Fixedeff
  var.list[[paste(COL, " -1")]]$Equation <- df.eff$Equaiton
  var.list[[paste(COL, " -1")]]$Value <- df.eff$Value
  var.list[[paste(COL, " -1")]]$pvalue <- df.eff$`p-value`
  
  #One set is for the general signifigance and this set is for sigfignace relative to no management
  lm.test <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(rcp=~1, GCM=~1), data = drought.df[drought.df$flag.2sig == T & drought.df$check.recov == T,])
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(as.character(sum$terms)[2], "~", as.character(sum$terms)[3])
  var.list[[paste(COL)]]$Var <- paste(COL)
  var.list[[paste(COL)]]$Fixedeff <- df.eff$Fixedeff
  var.list[[paste(COL)]]$Equation <- df.eff$Equaiton
  var.list[[paste(COL)]]$Value <- df.eff$Value
  var.list[[paste(COL)]]$pvalue <- df.eff$`p-value`
}

dat.lme <- dplyr::bind_rows(var.list)

write.csv(dat.lme, "../LME_of_recovery.csv", row.names = F)

drought.df$flag.2sig <- ifelse(drought.df$flag.2sig, "Drought", "Dry period")

png(file.path('../figures', 'Days_since_rain_vs_drought.png'))
ggplot(drought.df)+
  geom_histogram(aes(x = days_since_rain, fill =flag.2sig, color = flag.2sig))+
  ggtitle("Proportion of droughts instead of dry periods by duration of period")
dev.off()  



#Checking porportion of signifigant droughts
summary(drought.df$flag.4sig)

#Looking only aat signifigant results
sig <- drought.df[drought.df$flag.2sig == T, ]

sig <- sig[sig$D.end <= as.Date("2094-12-01"),]

table(sig$Management)

median(sig$days_of_recovery, na.rm = T)
mean(sig$days_of_recovery, na.rm = T)
sd(sig$days_of_recovery, na.rm = T)
min(sig$days_of_recovery, na.rm = T)
max(sig$days_of_recovery, na.rm = T)

#number of unique drought with a 4x significant drought dip
length(unique(sig$Drought.period))

summary(sig$recov.Date)

png(file.path('../figures', 'Days_of_recovery_distribution.png'))
ggplot(sig) + 
  geom_histogram(aes(x=days_of_recovery, fill = Management, color = Management)) +
  ggtitle("Distribtuion of the length of recovery period in days (with mean line)")+
  geom_vline(aes(xintercept = mean(sig$days_of_recovery, na.rm = T)))
dev.off()

png(file.path('../figures', 'Drought_recovery_over_years.png'))
ggplot(sig, aes(x=year, color = check.recov, fill = check.recov)) + 
  facet_wrap(~rcp)+
  geom_histogram() +
  ggtitle("Porportion of droughts that recovered each year")
dev.off()


#norecov <- sig[sig$check.recov == F,]

png(file.path('../figures', 'Nonrecovery_lowpoints.png'))
ggplot(sig) + 
  facet_grid(~rcp)+
  geom_point(aes(x=resil.month.ultimate, y = resil.min.ultimate, color = check.recov)) +
  ggtitle("Year of lowest point for non-recovered scenarios") +
  xlab("Date of lowest point of agb following drought")+
  ylab("agb at lowest point")
dev.off()


png(file.path('../figures', 'Past_soil_vs._pcent_change_in_agb.png'))
ggplot(sig) + 
  facet_grid(~rcp)+
  geom_point(aes(x=past.soil.moist.mean, y = resil.pcent.diff, color = check.recov)) +
  stat_smooth(aes(x=past.soil.moist.mean, y=resil.pcent.diff, color=check.recov), method="lm", alpha=0.2) +
  ggtitle("Past soil moisture vs. pcent change in difference") +
  xlab("Mean deep soil moisture 10 years before drought")+
  ylab("Percentage of agb difference")
dev.off()


png(file.path('../figures', 'Nee_vs._pcent_change_in_agb.png'))
ggplot(sig) + 
  facet_grid(~rcp)+
  geom_point(aes(x=past.nee.mean, y = resil.pcent.diff, color = check.recov)) +
  stat_smooth(aes(x=past.nee.mean, y=resil.pcent.diff, color=check.recov), method="lm", alpha=0.2) +
  ggtitle("Past nee vs. pcent change in difference") +
  xlab("Mean nee 10 years before drought")+
  ylab("Percentage of agb difference")
dev.off()

png(file.path('../figures', 'End_date_of_Drought_vs._pcent_change_in_agb.png'))
ggplot(sig) + 
  #facet_grid(~rcp)+
  geom_point(aes(x=D.end, y = resil.pcent.diff, color = check.recov)) +
  stat_smooth(aes(x=D.end, y=resil.pcent.diff, color=check.recov), method="lm", alpha=0.2) +
  ggtitle("End Date of Drought vs. pcent change in difference") +
  xlab("End Date of Drought")+
  ylab("Percentage of agb difference")
dev.off()

png(file.path('../figures', 'Compounding_dry_periods.png'))
ggplot(sig, aes(x=prev.dry.period, color = check.recov, fill = check.recov)) + 
  #facet_wrap(~rcp)+
  geom_histogram() +
  ggtitle("Porportion of droughts that recovered based on previous dry periods")+
  xlab("Number of previous dry periods (14 days without rain)")
dev.off()

png(file.path('../figures', 'Compounding_droughts_MNG.png'))
ggplot(sig, aes(x=prev.drought, color = check.recov, fill = check.recov)) + 
  facet_wrap(~Management)+
  geom_histogram() +
  ggtitle("Porportion of droughts that recovered based on previous dry droughts")+
  xlab("Number of previous droughts (14 days without rain that cause a signifigant drop in agb)")
dev.off()

png(file.path('../figures', 'AGB vs.recovery.png'))
ggplot(sig, aes(x=agb.mean, color = check.recov, fill = check.recov)) + 
  facet_wrap(~Management)+
  geom_histogram() +
  ggtitle("Porportion of droughts that recovered based mean agb for 10 years before drought")+
  xlab("mean agb for 10 years before drought")
dev.off()

ggplot(sig) + 
  #facet_grid(~rcp)+
  geom_point(aes(x=prev.drought, y = resil.pcent.diff, color = check.recov)) +
  stat_smooth(aes(x=prev.drought, y= resil.pcent.diff, color= check.recov), method="lm", alpha=0.2) +
  ggtitle("Number of Droughts vs. pcent change in difference") +
  xlab("Number of preiovus droughts")+
  ylab("Percentage of agb difference")



png(file.path('../figures', 'Past_soil_moisture_vs_number_droughts.png'))
ggplot(sig) + 
  #facet_grid(~rcp)+
  geom_point(aes(x=prev.drought, y = past.soil.moist.mean, color = check.recov)) +
  stat_smooth(aes(x=prev.drought, y= past.soil.moist.mean, color= check.recov), method="lm", alpha=0.2) +
  ggtitle("Number of Droughts vs. 10yr mean of deep soil moisture") +
  xlab("Number of preiovus droughts")+
  ylab("mean deep soil moisture for 10 years before drought")
dev.off()
#--------------------------------------#
#Linear regression of resilience for dry conditions
#--------------------------------------#
library(nlme)
drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))

drought.df$year <- year(drought.df$D.start)
#This is what lucien is checking if Management has a signifigant effect on the drop in agb
lm.test <- lme(resil.pcent.diff ~ Management-1, random=list(rcp=~1, GCM=~1, year=~1), data=drought.df[drought.df$D.start >= as.Date("2025-01-01") & drought.df$flag.2sig == T & is.na(drought.df$recov.Date) == F,])
summary(lm.test)
anova(lm.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
lm.2test <- lme(resil.pcent.diff ~ Management, random=list(rcp=~1, GCM=~1, year =~1), data=drought.df[drought.df$D.start >= as.Date("2025-01-01") & drought.df$flag.2sig == T& is.na(drought.df$recov.Date) == F,])
summary(lm.2test)
anova(lm.2test)

library(ggplot2)
ggplot(data=drought.df[drought.df$D.start >= as.Date("2025-01-01") & drought.df$flag.2sig == T & is.na(drought.df$recov.Date) == F,]) +
  facet_wrap(~Management) +
  geom_point(aes(x=, y=resil.diff, color=GCM), size=0.5, alpha=0.5) +
  stat_smooth(aes(x=year, y=resil.diff, color=GCM, fill=GCM), method="lm", alpha=0.2) 


#--------------------------------------------#
#Here we only look at signifignt results for drought
#--------------------------------------------#
#This is what lucien is checking if Management has a signifigant effect on the drop in agb
sig.test <- lme(resil.diff ~ Management-1, random=list(rcp=~1, GCM=~1, year=~1), data=sig)
summary(sig.test)
anova(sig.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
sig.2test <- lme(resil.diff ~ Management, random=list(rcp=~1, GCM=~1, year=~1), data=sig)
summary(sig.2test)
anova(sig.2test)

#--------------------------------------------#
#Here we do linear regression for time to recovery. Only including sucessful recoveries. 
#Not sure how to account for failed recovery?
#-------------------------------------------#
recov.sig <- sig[!is.na(sig$days_of_recovery),]

#This is what lucien is checking if Management has a signifigant effect on the drop in agb
sig.test <- lme(days_of_recovery ~ Management-1, random=list(rcp=~1, GCM=~1), data=recov.sig)
summary(sig.test)
anova(sig.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
sig.2test <- lme(days_of_recovery ~ Management, random=list(rcp=~1, GCM=~1), data=recov.sig)
summary(sig.2test)
anova(sig.2test)

#This section creates a graphic that highlights drought windows
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought_window.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    if(nrow(temp.end) > 0){
      for(i in 1:nrow(temp.end)){
        END <- as.Date(temp.end[i, "Date"])
        STR <- END - temp.end[i, "days_since_rain"]
        if(length(STR) > 0){
          df.var <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                               runs.all$Date >= (STR-lubridate::years(1)) & runs.all$Date <= (END + lubridate::years(2)), ]
          
          print(
            ggplot() +
              ggtitle(paste(MOD, RCP, (STR-lubridate::years(1)), (END + lubridate::years(5)))) +
              geom_line(data=df.var, aes(x=Date, y=agb, color = Management))+
              geom_vline(xintercept = as.Date("2070-07-01"), linetype = 4) +
              geom_rect(aes(xmin = STR, xmax = END, ymin = -Inf, ymax = Inf), alpha = 0.4)
          )
        }
      }
    }
  }
}
dev.off()

#Where the Exploratory figures begin

library(ggplot2)
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought_monthtest.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$GCM == MOD & dat.end$rcp == RCP, ]
    Enddates <- as.Date(temp.end[, "Date"])
    if(length(Enddates) > 0){
      print(
        ggplot() +
          ggtitle(paste(MOD, RCP)) +
          geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
          geom_vline(xintercept = Enddates, linetype = 4) 
        #geom_rect(aes(xmin = Strdates, xmax = Enddates, ymin = -Inf, ymax = Inf, fill = letters[1:4]), alpha = 0.4)
      )
    }
  }
}
dev.off()


library(ggplot2)
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    Enddates <- as.Date(temp.end[, "Date"])
    Strdates <- as.Date(temp.end$Date) - temp.end$days_since_rain
    if(length(Strdates) > 0){
      print(
        ggplot() +
          ggtitle(paste(MOD, RCP)) +
          geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
          #geom_vline(xintercept = Enddates, linetype = 4) +
          geom_rect(aes(xmin = Strdates, xmax = Enddates, ymin = -Inf, ymax = Inf, fill = letters[1:4]), alpha = 0.4)
      )
    }
  }
}
dev.off()
