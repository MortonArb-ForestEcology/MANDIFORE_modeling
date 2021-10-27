#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: To check if the scaling of precipitation matches the ED2 output
# Inputs: ED2 Morton Arb site data
#         Aggregated daily precip data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#

#Pulling in the Mandifore data
path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)
library(dplyr)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis/")

dat.precip <- read.csv("../Precip_Weather_Daily")

dat.precip$month <- lubridate::month(dat.precip$Date)

dat.precip$year <- lubridate::year(dat.precip$Date)

#dat.leap <- dat.precip[dat.precip$Date == as.Date("2024-02-29"),]

#This is when the daily rain gets converted to monthly. This is the final conversion step where daily is summed by month
dat.sum <- aggregate(sum~month+year+model+scenario, dat.precip, FUN = sum)

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "precipf", "Management")]

dat.compare <- merge(dat.sum, dat.merge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.compare$diff <- dat.compare$sum - dat.compare$precipf

#Creating a date object so we can easily plot each month
dat.compare$Date <- lubridate::ymd(paste(dat.compare$year, dat.compare$month, "15", sep = "-"))

library(ggplot2)
png(width=9, height=8, units="in", res=600, filename= file.path("../Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.compare, aes(x=Date, y=diff, color = model))+
  ggtitle("DIfference between Precipitation")
  dev.off()

  
#Looking at percent difference instead of absolute
dat.compare$pcent.diff <- (dat.compare$sum/dat.compare$precipf - 1) * 100
  
png(width=9, height=8, units="in", res=600, filename= file.path("../Pcent_Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.compare, aes(x=Date, y=pcent.diff, color = model))+
  ggtitle("% Difference between Precipitation (Aggregated/ED output)")
dev.off()
dat.compare <- read.csv("../Precip_Comparision.csv")




#Testing zone

month <- 1:12
dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24

convert <- dataframe(month, sec2mo)

dat.precip$sec2mo <- convert$sec2mo[match(dat.precip$month, convert$month)]

dat.precip$con <- dat.precip$


dat.temp$month <- lubridate::month(dat.temp$Date)

dat.temp$year <- lubridate::year(dat.temp$Date)

#This is when the daily rain gets converted to monthly. This is the final conversion step where daily is summed by month
dat.sum <- aggregate(mean~month+year+model+scenario, dat.temp, FUN = mean)

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "tair", "Management")]

dat.compare <- merge(dat.sum, dat.merge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.compare$diff <- dat.compare$mean - dat.compare$tair

dat.compare$pcent.diff <- (dat.compare$mean/dat.compare$tair - 1) * 100

ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.compare, aes(x=Date, y=diff, color = model))+
  ggtitle("DIfference between Temp")
dev.off()

ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.compare, aes(x=Date, y=pcent.diff, color = model))+
  ggtitle("% Difference between Temp (Aggregated/ED output)")
dev.off()
