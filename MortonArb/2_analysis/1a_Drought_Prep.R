#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)
dat.precip <- read.csv("../Precip_Weather_Daily.csv")

dat.precip$days_since_rain <- NA

dat.precip <- dat.precip[!is.na(dat.precip$mean),]


drought <- 0
for(i in 1:nrow(dat.precip)){
  if(dat.precip[i, "mean"] == 0){
    drought <- drought + 1
  } else {
    drought <- 0
  }
  dat.precip[i, "days_since_rain"] <- drought
} 

dat.precip$Drought_flag <-  ifelse(dat.precip$mean == 0, 1, 0)

write.csv(dat.precip, "../Drought_Weather_Daily.csv", row.names = F)


library(ggplot2)
png(file.path('../figures', 'Days_since_rain.png'))
ggplot(dat.precip, aes(days_since_rain))+
  geom_bar( color = "black")+
  scale_y_log10()+
  ggtitle("Log transformed days since rain")+
  geom_vline(xintercept = 14)
dev.off()

dat.precip <- read.csv("../Drought_Weather_Daily.csv")

#Creating a dataframe that contains the end date of every drought
#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.precip)){
  if(dat.precip[i, "days_since_rain"] == 0 & dat.precip[i-1,"Drought_flag"] == 1){
    dat.end <- rbind(dat.end, dat.precip[i-1,])
  }
}

dat.end$D.start <- as.Date(dat.end$Date) - dat.end$days_since_rain
library(lubridate)

dat.end$season <- NA
for(i in 1:nrow(dat.end)){ 
  if(month(dat.end[i, "D.start"]) >= 3 & month(dat.end[i, "D.start"]) <= 5){
    dat.end[i, "season"] <- "Spring"
  } else if(month(dat.end[i, "D.start"]) >= 6 & month(dat.end[i, "D.start"]) <= 8){
    dat.end[i, "season"] <- "Summer"
  } else if(month(dat.end[i, "D.start"]) >= 9 & month(dat.end[i, "D.start"]) <= 11){
    dat.end[i, "season"] <- "Fall"
  } else {
    dat.end[i, "season"] <- "Winter"
  }
}

write.csv(dat.end, "../Drought_Periods_End.csv", row.names = F)
 
#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(lubridate)
dat.precip <- read.csv("../Precip_Weather_Daily.csv")

dat.precip$month <- lubridate::month(dat.precip$Date)
dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$mean),]

dat.month <- aggregate(sum~month+year+model+scenario, dat.precip, FUN = sum)

dat.flag <- data.frame()
for(MOD in unique(dat.month$model)){
  for(RCP in unique(dat.month[dat.month$model == MOD, "scenario"])){
    df <- dat.month[dat.month$model == MOD & dat.month$scenario == RCP,]
    df$past.mean <- mean(df[df$year >= min(df$year) & df$year <= (min(df$year) + 10), "sum"])
    df$past.sd <- sd(df[df$year >= min(df$year) & df$year <= (min(df$year) + 10), "sum"])
    dat.flag <- rbind(dat.flag, df) 
  }
}

dat.flag$Date <- lubridate::ymd(paste(dat.flag$year, dat.flag$month, "01", sep = "-"))


for(k in 1:nrow(dat.flag)){
  dat.flag[k, "roll.sum"] <- mean(dat.flag[dat.flag$model == dat.flag[k, "model"] & dat.flag$scenario == dat.flag[k, "scenario"] &
                                             dat.flag$Date >= (dat.flag[k, "Date"] %m-% years(1)) & dat.flag$Date <= dat.flag[k, "Date"], "sum"])
}

drought <- 0
for(i in 1:nrow(dat.flag)){
  drought <- ifelse(dat.flag[i, "sum"] < dat.flag[i, "past.mean"] -2*dat.flag[i, "past.sd"], drought + 1, 0)
  dat.flag[i, "drought_months"] <- drought
} 


write.csv(dat.flag, "../Drought_Weather_Monthly.csv", row.names = F)


library(ggplot2)
png(file.path('../figures', 'Drought_months.png'))
ggplot(dat.flag, aes(drought_months))+
  geom_bar( color = "black")+
  scale_y_log10()+
  ggtitle("Consecutive months of drought")+
  geom_vline(xintercept = 14)
dev.off()

dat.flag <- read.csv("../Drought_Weather_Monthly.csv")

#Creating a dataframe that contains the end date of every drought
#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.flag)){
  if(dat.flag[i, "drought_months"] == 0 & dat.flag[i-1,"drought_months"] >= 1){
    dat.end <- rbind(dat.end, dat.flag[i-1,])
  }
}

dat.end$D.start <- as.Date(dat.end$Date) %m-% months(dat.end$drought_months)


dat.end$season <- NA
for(i in 1:nrow(dat.end)){ 
  if(month(dat.end[i, "D.start"]) >= 3 & month(dat.end[i, "D.start"]) <= 5){
    dat.end[i, "season"] <- "Spring"
  } else if(month(dat.end[i, "D.start"]) >= 6 & month(dat.end[i, "D.start"]) <= 8){
    dat.end[i, "season"] <- "Summer"
  } else if(month(dat.end[i, "D.start"]) >= 9 & month(dat.end[i, "D.start"]) <= 11){
    dat.end[i, "season"] <- "Fall"
  } else {
    dat.end[i, "season"] <- "Winter"
  }
}

write.csv("../Drought_Periods_End_Monthly.csv", row.names = F)

dat.month <- read.csv("../Drought_Periods_End_Monthly.csv")

#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(lubridate)

path.read <- "../data/"
runs.all <- read.csv(paste0(path.read, "All_runs_mod.csv"))

runs.start <- runs.all[runs.all$Date <= "2025-01-01",]

vpd.avg <- aggregate(VPD~month+rcp, runs.start, FUN = mean)
vpd.avg$sd <- aggregate(VPD~month+rcp, runs.start, FUN = sd)[, "VPD"]

for(RCP in unique(runs.all$rcp)){
  for(MON in unique(runs.all$month)){
    runs.all[runs.all$month == MON & runs.all$rcp == RCP, "vpd.flag"] <- 
      ifelse(runs.all[runs.all$month == MON & runs.all$rcp == RCP, "VPD"] > 
               (vpd.avg[vpd.avg$month == MON & vpd.avg$rcp == RCP, "VPD"] +3*vpd.avg[vpd.avg$month == MON & vpd.avg$rcp == RCP, "sd"]) ,T,F)
  }
}

dat.end <- runs.all[runs.all$vpd.flag == T, ]

dat.end$season <- NA
for(i in 1:nrow(dat.end)){ 
  if(month(dat.end[i, "month"]) >= 3 & month(dat.end[i, "month"]) <= 5){
    dat.end[i, "season"] <- "Spring"
  } else if(month(dat.end[i, "month"]) >= 6 & month(dat.end[i, "month"]) <= 8){
    dat.end[i, "season"] <- "Summer"
  } else if(month(dat.end[i, "month"]) >= 9 & month(dat.end[i, "month"]) <= 11){
    dat.end[i, "season"] <- "Fall"
  } else {
    dat.end[i, "season"] <- "Winter"
  }
}

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "month", "VPD", "Date", "vpd.flag", "season")]

dat.end <- dat.end[dat.end$Date >= as.Date("2025-01-01"),]

write.csv(dat.end, "../Drought_Periods_VPD.csv", row.names = F)

dat.vpd <- dat.end

#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)
library(lubridate)

path.read <- "../data/"
runs.all <- read.csv(paste0(path.read, "All_runs_mod.csv"))

runs.start <- runs.all[runs.all$Date <= "2025-01-01",]

swc.avg <- aggregate(soil.moist.deep~month+rcp+GCM, runs.start, FUN = mean)
swc.avg$sd <- aggregate(soil.moist.deep~month+rcp+GCM, runs.start, FUN = sd)[, "soil.moist.deep"]
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all$rcp)){
    for(MON in unique(runs.all$month)){
      runs.all[runs.all$GCM == MOD & runs.all$month == MON & runs.all$rcp == RCP, "swc.flag"] <- 
        ifelse(runs.all[runs.all$GCM == MOD & runs.all$month == MON & runs.all$rcp == RCP, "soil.moist.deep"] < 
                 (swc.avg[swc.avg$GCM == MOD & swc.avg$month == MON & swc.avg$rcp == RCP, "soil.moist.deep"] +3*swc.avg[swc.avg$GCM == MOD & swc.avg$month == MON & swc.avg$rcp == RCP, "sd"]) ,T,F)
    }
  }
}

#dat.end <- runs.all[runs.all$swc.flag == T, ]
dat.end <- runs.all[runs.all$soil.moist.deep <= .25, ]


dat.end$season <- NA
for(i in 1:nrow(dat.end)){ 
  if(month(dat.end[i, "month"]) >= 3 & month(dat.end[i, "month"]) <= 5){
    dat.end[i, "season"] <- "Spring"
  } else if(month(dat.end[i, "month"]) >= 6 & month(dat.end[i, "month"]) <= 8){
    dat.end[i, "season"] <- "Summer"
  } else if(month(dat.end[i, "month"]) >= 9 & month(dat.end[i, "month"]) <= 11){
    dat.end[i, "season"] <- "Fall"
  } else {
    dat.end[i, "season"] <- "Winter"
  }
}

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "month", "soil.moist.deep", "Date", "season")]

dat.end <- dat.end[dat.end$Date >= as.Date("2025-01-01"),]

write.csv(dat.end, "../Drought_Periods_SWC.csv", row.names = F)

dat.swc <- dat.end

#---------------------------------------------------------------------------------------------#
#Area for comparison between drought metrics
#---------------------------------------------------------------------------------------------#

hist(runs.all$VPD)
hist(runs.all$soil.moist.deep)


dat.precip <- read.csv("../Drought_Periods_End.csv")
dat.precip <- dat.precip[dat.precip$days_since_rain >= 14,]

dat.month <- read.csv("../Drought_Periods_End_Monthly.csv")

dat.vpd <- read.csv("../Drought_Periods_VPD.csv")

dat.swc <- read.csv("../Drought_Periods_SWC.csv")

hist(dat.swc$soil.moist.deep)

hist(dat.vpd$VPD)
