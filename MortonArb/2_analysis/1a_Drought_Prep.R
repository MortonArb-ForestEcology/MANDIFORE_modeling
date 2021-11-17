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

dat.plot <- dat.precip[dat.precip$days_since_rain > 0 & dat.precip$days_since_rain <75 ,]
library(ggplot2)
ggplot(dat.plot, aes(days_since_rain))+
  geom_bar(width=0.1, color = "black")

dat.precip <- read.csv("../Drought_Weather_Daily.csv")

#Creating a dataframe that contains the end date of every drought
#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.precip)){
  if(dat.precip[i, "days_since_rain"] == 0 & nchar(dat.precip[i-1, "days_since_rain"]) > 1){
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
 