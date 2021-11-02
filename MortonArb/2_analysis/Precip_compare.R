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
dat.sum <- aggregate(value~month+year+model+scenario, dat.precip, FUN = mean)

dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24

dat.sum$value <- dat.sum$value * sec2mo[dat.sum$month]

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "precipf", "Management")]

dat.compare <- merge(dat.sum, dat.merge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.compare$diff <- dat.compare$value - dat.compare$precipf

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

dat.bad <- dat.compare[dat.compare$pcent.diff > 20 | dat.compare$pcent.diff < -20,]


ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_point(data=dat.compare, aes(x=sum, y=precipf, color = model))+
  ggtitle("Daily Aggregate (sum) vs. monthly ED2 (precipf)")
dev.off()

ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_point(data=dat.compare, aes(x=precipf, y=diff, color = model))+
  geom_smooth(data=dat.compare, aes(x=precipf, y=diff, color = model))+
  ggtitle("Daily Aggregate (sum) vs. monthly ED2 (precipf)")



dat.temp$month <- lubridate::month(dat.temp$Date)

dat.temp$year <- lubridate::year(dat.temp$Date)

#dat.leap <- dat.precip[dat.precip$Date == as.Date("2024-02-29"),]

#This is when the daily rain gets converted to monthly. This is the final conversion step where daily is summed by month
dat.tsum <- aggregate(value~month+year+model+scenario, dat.temp, FUN = mean)

dat.tmerge <- runs.all[, c("month", "year", "GCM", "rcp", "tair", "Management")]

dat.tcompare <- merge(dat.tsum, dat.tmerge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.tcompare$diff <- dat.tcompare$value - dat.tcompare$tair

#Creating a date object so we can easily plot each month
dat.tcompare$Date <- lubridate::ymd(paste(dat.tcompare$year, dat.tcompare$month, "15", sep = "-"))

library(ggplot2)
png(width=9, height=8, units="in", res=600, filename= file.path("../Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.tcompare, aes(x=Date, y=diff, color = model))+
  ggtitle("DIfference between Temperature")
dev.off()

dat.tsum$value <- dat.tsum$value - 273.15
dat.tmerge$tair <- dat.tmerge$tair - 273.15

#Looking at percent difference instead of absolute
dat.tcompare$pcent.diff <- (dat.tcompare$value/dat.tcompare$tair - 1) * 100

png(width=9, height=8, units="in", res=600, filename= file.path("../Pcent_Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.tcompare, aes(x=Date, y=pcent.diff, color = model))+
  ggtitle("% Difference between Temperature (Aggregated/ED output)")
dev.off()
dat.tcompare <- read.csv("../Temp_Comparision.csv")
















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



# Get a list of everything we have to work with
dat.old <- "C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/met_ed.v3/MortonArb/ACCESS1-0_rcp45_bc.tdm/"

mods.old <- dir(dat.old)
mods.old <- mods.old[1:length(mods.old)-1]
old.df <- data.frame()
for(MON in mods.old){
  old.test_filename <- file.path(dat.old, MON)
  old.file.h5 <- H5File$new(old.test_filename, mode = "r+")
  df <- old.file.h5[["prate"]]
  h5rain <- df[1:df$dims[1],1,1]
  temp <- data.frame(MON, mean(h5rain))
  old.df <- rbind(old.df, temp)
}


old.df$year <- substr(old.df$MON, 1, 4)
old.df$month <- substr(old.df$MON, 5, 7)
colnames(old.df) <- c("MON", "old.mean", "year", "month")

old.df$month <- car::recode(old.df$month, "'JAN'='01'; 'FEB'='02'; 'MAR'='03'; 'APR'='04'; 'MAY'='05'; 'JUN'='06'; 'JUL'='07';'AUG'='08';'SEP'='09';'OCT'='10'; 'NOV'='11'; 'DEC' = '12'")

dat.h5 <- "C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/1_runs/MortonArb_ed_runs.v3/MortArb_All_ACCESS1-0_rcp45_statCO2_MgmtNone/analy/"
mods.new <- dir(dat.h5)

new.df <- data.frame()
for(MON in mods.new){
  test_filename <- file.path(dat.h5, MON)
  file.h5 <- H5File$new(test_filename, mode = "r+")
  if("MMEAN_PCPG_PY" %in% file.h5$names){
    new.h5 <- file.h5[["MMEAN_PCPG_PY"]]
    temp <- data.frame(MON, new.h5[1])
    new.df <- rbind(new.df,temp)
  }
}

new.df$year <- substr(new.df$MON,48, 51)
new.df$month <- as.integer(substr(new.df$MON,53, 54))

colnames(new.df) <- c("MON", "new.mean", "month", "year")

dat.compare <- merge(new.df, old.df, by.x = c("month", "year"), by.y= c("month", "year"))

dat.compare$diff <- dat.compare$old.mean - dat.compare$new.mean

#Creating a date object so we can easily plot each month
dat.compare$Date <- lubridate::ymd(paste(dat.compare$year, dat.compare$month, "15", sep = "-"))

library(ggplot2)
png(width=9, height=8, units="in", res=600, filename= file.path("../Difference_In_Precip.png"))
ggplot() +
  geom_line(data=dat.compare, aes(x=Date, y=diff))+
  ggtitle("DIfference between Precipitation rate (kg2/m2/sec)")+
  xlab("Date")+
  ylab("Absolute difference")
dev.off()


#Looking at percent difference instead of absolute
dat.compare$pcent.diff <- (dat.compare$old.mean/dat.compare$new.mean - 1) * 100

png(width=9, height=8, units="in", res=600, filename= file.path("../Pcent_Difference_In_Precip.png"))
ggplot() +
  geom_line(data=dat.compare, aes(x=Date, y=pcent.diff))+
  ggtitle("% Difference between Precipitation rate (kg2/m2/sec) (Aggregated/ED output)")+
  xlab("Date")+
  ylab("% difference (kg/m2/sec)")
dev.off()
dat.compare <- read.csv("../Precip_Comparision.csv")

dat.bad <- dat.compare[dat.compare$pcent.diff > 20 | dat.compare$pcent.diff < -20,]

png(width=9, height=8, units="in", res=600, filename= file.path("../Daily_v_Monthly_preicp.png"))
ggplot() +
  geom_point(data=dat.compare, aes(x=old.mean, y=new.mean))+
  ggtitle("Daily Aggregate (old.mean) vs. monthly ED2 (new.mean)")+
  xlab("Daily Aggregate mean (kg2/m2/sec)")+
  ylab("Monthly ED2 mean (kg/m2/sec)")
  
dev.off()

png(width=9, height=8, units="in", res=600, filename= file.path("../Monthly_v_Difference.png"))
ggplot() +
  geom_point(data=dat.compare, aes(x=new.mean, y=diff))+
  geom_smooth(data=dat.compare, aes(x=new.mean, y=diff))+
  ggtitle("ED2 monthly mean (kg2/m2/sec vs. difference")+
  xlab("Monthly ED2 mean (kg/m2/sec)")+
  ylab("Absolute difference")
dev.off()

