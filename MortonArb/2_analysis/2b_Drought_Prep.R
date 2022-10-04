#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)
path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/output/"

dat.precip <- read.csv(file.path(path.google, "Precip_Weather_Daily.csv"))

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

write.csv(dat.precip, "../data/Drought_Weather_Daily.csv", row.names = F)


library(ggplot2)
png(file.path('../figures', 'Days_since_rain.png'))
ggplot(dat.precip, aes(days_since_rain))+
  geom_bar( color = "black")+
  scale_y_log10()+
  ggtitle("Log transformed days since rain")+
  geom_vline(xintercept = 14)
dev.off()

dat.precip <- read.csv("../data/Drought_Weather_Daily.csv")

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

write.csv(dat.end, "../data/Drought_Periods_End.csv", row.names = F)
 
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
dat.precip <- read.csv(file.path(path.google, "Precip_Weather_Daily.csv"))

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


write.csv(dat.flag, "../data/Drought_Weather_Monthly.csv", row.names = F)


library(ggplot2)
png(file.path('../figures', 'Drought_months.png'))
ggplot(dat.flag, aes(drought_months))+
  geom_bar( color = "black")+
  scale_y_log10()+
  ggtitle("Consecutive months of drought")+
  geom_vline(xintercept = 14)
dev.off()

dat.flag <- read.csv("../data/Drought_Weather_Monthly.csv")

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

write.csv("../data/Drought_Periods_End_Monthly.csv", row.names = F)

dat.month <- read.csv("../data/Drought_Periods_End_Monthly.csv")

#----------------------------------------------------------------------------------------------------------------------#
# Using yearly mean deep soil moisture
#----------------------------------------------------------------------------------------------------------------------#
path.read <- "../data/"
runs.all <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

hist(runs.all$VPD)

vpd.avg <- aggregate(VPD~rcp, runs.all, FUN = mean)
vpd.avg$sd <- aggregate(VPD~rcp, runs.all, FUN = sd)[, "VPD"]

for(i in 1:nrow(runs.all)){
  runs.all[i, "vpd.flag"] <- ifelse(runs.all[i, "VPD"] > 
                                      (vpd.avg[vpd.avg$rcp == runs.all[i,"rcp"], "VPD"] + 2*vpd.avg[vpd.avg$rcp == runs.all[i,"rcp"], "sd"]) ,T,F)
}
dat.end <- runs.all[runs.all$vpd.flag == T, ]

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "VPD", "vpd.flag")]

dat.end <- dat.end[dat.end$year <= 2095,]

write.csv(dat.end, "../Drought_Periods_VPD.csv", row.names = F)

dat.vpd <- dat.end

#----------------------------------------------------------------------------------------------------------------------#
# Using yearly mean deep soil moisture
#----------------------------------------------------------------------------------------------------------------------#

hist(runs.all$soil.moist.deep)

swc.deep.avg <- aggregate(soil.moist.deep~rcp, runs.all, FUN = mean)
swc.deep.avg$sd <- aggregate(soil.moist.deep~rcp, runs.all, FUN = sd)[, "soil.moist.deep"]

for(i in 1:nrow(runs.all)){
  runs.all[i, "swc.deep.flag"] <- ifelse(runs.all[i, "soil.moist.deep"] < 
                                      (swc.deep.avg[swc.deep.avg$rcp == runs.all[i,"rcp"], "soil.moist.deep"] - 2*swc.deep.avg[swc.deep.avg$rcp == runs.all[i,"rcp"], "sd"]) ,T,F)
}
dat.end <- runs.all[runs.all$swc.deep.flag == T, ]

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "soil.moist.deep", "swc.deep.flag")]

dat.end <- dat.end[dat.end$year <= 2095,]


write.csv(dat.end, "../data/Drought_Periods_SWC.deep.csv", row.names = F)

dat.swc.deep <- dat.end

#----------------------------------------------------------------------------------------------------------------------#
# Using yearly mean surface soil moisture
#----------------------------------------------------------------------------------------------------------------------#

hist(runs.all$soil.moist.surf)

swc.surf.avg <- aggregate(soil.moist.surf~rcp, runs.all, FUN = mean)
swc.surf.avg$sd <- aggregate(soil.moist.surf~rcp, runs.all, FUN = sd)[, "soil.moist.surf"]

for(i in 1:nrow(runs.all)){
  runs.all[i, "swc.surf.flag"] <- ifelse(runs.all[i, "soil.moist.surf"] < 
                                           (swc.surf.avg[swc.surf.avg$rcp == runs.all[i,"rcp"], "soil.moist.surf"] - 2*swc.surf.avg[swc.surf.avg$rcp == runs.all[i,"rcp"], "sd"]) ,T,F)
}
dat.end <- runs.all[runs.all$swc.surf.flag == T, ]

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "soil.moist.surf", "swc.surf.flag")]

dat.end <- dat.end[dat.end$year <= 2095,]


write.csv(dat.end, "../data/Drought_Periods_SWC.surf.csv", row.names = F)

dat.swc.surf <- dat.end

#----------------------------------------------------------------------------------------------------------------------#
# Using yearly mean air temp
#----------------------------------------------------------------------------------------------------------------------#

hist(runs.all$tair)

tair.avg <- aggregate(tair~rcp, runs.all, FUN = mean)
tair.avg$sd <- aggregate(tair~rcp, runs.all, FUN = sd)[, "tair"]

for(i in 1:nrow(runs.all)){
  runs.all[i, "tair.flag"] <- ifelse(runs.all[i, "tair"] > 
                                      (tair.avg[tair.avg$rcp == runs.all[i,"rcp"], "tair"] + 2*tair.avg[tair.avg$rcp == runs.all[i,"rcp"], "sd"]) ,T,F)
}
dat.end <- runs.all[runs.all$tair.flag == T, ]

dat.end <- dat.end[,c("GCM", "rcp", "Management", "year", "tair", "tair.flag")]

dat.end <- dat.end[dat.end$year <= 2095,]

write.csv(dat.end, "../data/Drought_Periods_tair.csv", row.names = F)

dat.tair <- dat.end

#---------------------------------------------------------------------------------------------#
#Area for comparison between drought metrics
#---------------------------------------------------------------------------------------------#
#dat.vpd <- read.csv("../data/Drought_Periods_VPD.csv")

#dat.swc.deep <- read.csv("../data/Drought_Periods_SWC.deep.csv")

#dat.swc.surf <- read.csv("../data/Drought_Periods_SWC.surf.csv")

#dat.tair <- read.csv("../data/Drought_Periods_tair.csv")

hist(runs.all$sum)
hist(runs.all$rainless.days)

runs.drought <- runs.all[runs.all$vpd.flag == T & runs.all$swc.deep.flag == T & runs.all$swc.surf.flag ==T & runs.all$tair.flag == T,]
