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

#-------------------------------------#
# Here is where I do the conversion in the style of ED2
# This means I am taking the monthly mean precipitation and converting it using manual coding (ignoring leap year)
# Takes the mean of the kg2/m2/sec for every month and then converts that mean into kg2/m2/mo without accounting for leap year.
# Later I do a full converion that sums the subdaily up to monthly and then converts while accounting for leaf year
#-------------------------------------#

dat.mean <- aggregate(mean~month+year+model+scenario, dat.precip, FUN = mean)

dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24

dat.mean$mean <- dat.mean$mean * sec2mo[dat.mean$month]

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "precipf", "Management")]

dat.EDcompare <- merge(dat.mean, dat.merge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.EDcompare$diff <- dat.EDcompare$mean - dat.EDcompare$precipf

#Creating a date object so we can easily plot each month
dat.EDcompare$Date <- lubridate::ymd(paste(dat.EDcompare$year, dat.EDcompare$month, "15", sep = "-"))

library(ggplot2)
png(width=9, height=8, units="in", res=600, filename= file.path("../Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.EDcompare, aes(x=Date, y=diff, color = model))+
  ggtitle("DIfference between Precipitation")
  dev.off()

  
#Looking at percent difference instead of absolute
dat.EDcompare$pcent.diff <- (dat.EDcompare$mean/dat.EDcompare$precipf - 1) * 100
  
png(width=9, height=8, units="in", res=600, filename= file.path("../Pcent_Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_line(data=dat.EDcompare, aes(x=Date, y=pcent.diff, color = model))+
  ggtitle("% Difference between Precipitation (Aggregated/ED output)")
dev.off()

ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_point(data=dat.EDcompare, aes(x=mean, y=precipf, color = model))+
  ggtitle("Daily Aggregate (sum) vs. monthly ED2 (precipf)")
dev.off()

ggplot() +
  facet_wrap(~model, scales = "free_y")+
  geom_point(data=dat.EDcompare, aes(x=precipf, y=diff, color = model))+
  geom_smooth(data=dat.EDcompare, aes(x=precipf, y=diff, color = model))+
  ggtitle("Daily Aggregate (sum) vs. monthly ED2 (precipf)")

write.csv(dat.EDcompare, "../Mean_Precip_Comparision.csv",  row.names = F)

#-------------------------------------#
# Here is where I do the conversion fully?
# This means I am taking the subdaily resolution and summing it up. The conversion is 60*60 to go seconds to hours
# Convert the kg2/m2/sec to kg2/m2/hour and then sum up to daily and then monthly while accounting for leaf year
# This is not how ED does it's converisons. THere will always be a mismatch if I do it this way. This is for documentation
#-------------------------------------#
dat.sum <- aggregate(sum~month+year+model+scenario, dat.precip, FUN = sum)

dat.sum$sum <- dat.sum$sum * 60 * 60

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "precipf", "Management")]

#Seeing if there is a difference in the two methods
#There is!!
dat.convert <- merge(dat.sum, dat.mean, by.x = c("month", "year", "model", "scenario"))
dat.convert$diff <- dat.convert$mean - dat.convert$sum

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

write.csv(dat.compare, "../Sum_Precip_Comparision.csv",  row.names = F)


#--------------------------------------------------#
#Here is where I compare the raw ED2 output with the ED input
# These do not match which is where I see the problem in the mismatch
# However I don't understand the iin between step and am at a loss
#--------------------------------------------------#

library("hdf5r")
# Pullling from the netcdf data that is used as ED output
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

#Comparing this to the ED2 output
#THEY DO NOT MATCH
old.df$MON <- "ACCESS1-0"
old.df$scenario <- "rcp45"
dat.oldmatch <- merge(old.df, dat.merge, by.x = c("month", "year", "MON", "scenario"), by.y= c("month", "year", "GCM", "rcp"))
dat.oldmatch$old.mean <- dat.oldmatch$old.mean * sec2mo[dat.oldmatch$month]
dat.oldmatch$diff <- dat.oldmatch$old.mean - dat.oldmatch$precipf

dat.oldmatch$Date <- lubridate::ymd(paste(dat.oldmatch$year, dat.oldmatch$month, "15", sep = "-"))

write.csv(dat.oldmatch, "../Input_Output_Precip_Comparision.csv", row.names = F)

png(width=9, height=8, units="in", res=600, filename= file.path("../Shell_Difference_In_Precip.png"))
ggplot() +
  geom_line(data=dat.oldmatch, aes(x=Date, y=diff))+
  ggtitle("DIfference between Precipitation rate (kg2/m2/sec)")+
  xlab("Date")+
  ylab("Absolute difference")
dev.off()


#Looking at percent difference instead of absolute
dat.oldmatch$pcent.diff <- (dat.oldmatch$old.mean/dat.oldmatch$precipf - 1) * 100

png(width=9, height=8, units="in", res=600, filename= file.path("../Shell_Pcent_Difference_In_Precip.png"))
ggplot() +
  geom_line(data=dat.oldmatch, aes(x=Date, y=pcent.diff))+
  ggtitle("% Difference between Precipitation rate (kg2/m2/mo) (Aggregated/ED output)")+
  xlab("Date")+
  ylab("% difference (kg/m2/mo)")
dev.off()



png(width=9, height=8, units="in", res=600, filename= file.path("../Shell_Daily_v_Monthly_preicp.png"))
ggplot() +
  geom_point(data=dat.oldmatch, aes(x=old.mean, y=precipf))+
  ggtitle("Daily Aggregate (old.mean) vs. monthly ED2 (new.mean)")+
  xlab("Daily Aggregate mean (kg2/m2/sec)")+
  ylab("Monthly ED2 mean (kg/m2/sec)")
dev.off()

png(width=9, height=8, units="in", res=600, filename= file.path("../Shell_Monthly_v_Difference.png"))
ggplot() +
  geom_point(data=dat.oldmatch, aes(x=precipf, y=diff))+
  geom_smooth(data=dat.oldmatch, aes(x=precipf, y=diff))+
  ggtitle("ED2 monthly mean (kg2/m2/sec vs. difference")+
  xlab("Monthly ED2 mean (kg/m2/sec)")+
  ylab("Absolute difference")
dev.off()


# Pullling from the netcdf data that is immediately after the ED2 output
# This is the full netcdf, not the csv that we extract to work with
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

colnames(new.df) <- c("MON", "new.mean", "year",  "month")

#Here I compare the netcdf that contains the ED output to the ED output. THEY MATCH
#THESE MATCH WITHIN REASON (are all off by e-13 or less)
new.df$MON <- "ACCESS1-0"
new.df$scenario <- "rcp45"
dat.newmatch <- merge(new.df, dat.merge, by.x = c("month", "year", "MON", "scenario"), by.y= c("month", "year", "GCM", "rcp"))
dat.newmatch$new.mean <- dat.newmatch$new.mean * sec2mo[dat.newmatch$month]
dat.newmatch$diff <- dat.newmatch$new.mean - dat.newmatch$precipf

dat.prepost <- merge(new.df, old.df, by.x = c("month", "year"), by.y= c("month", "year"))

dat.prepost$diff <- dat.prepost$old.mean - dat.prepost$new.mean



#Creating a date object so we can easily plot each month
dat.prepost$Date <- lubridate::ymd(paste(dat.prepost$year, dat.prepost$month, "15", sep = "-"))

library(ggplot2)
#png(width=9, height=8, units="in", res=600, filename= file.path("../Difference_In_Precip.png"))
ggplot() +
  facet_wrap(~month)+
  geom_line(data=dat.prepost, aes(x=Date, y=diff))+
  ggtitle("DIfference between Precipitation rate (kg2/m2/sec)")+
  xlab("Date")+
  ylab("Absolute difference")
dev.off()

old.df.year <- aggregate(old.mean~year+MON+scenario, data = old.df,FUN = sum)
new.df.year <- aggregate(new.mean~year+MON+scenario, data = new.df,FUN = sum)
dat.year<- merge(old.df.year, new.df.year, by.x = c( "year", "MON", "scenario"))

dat.year$diff <- dat.year$old.mean-dat.year$new.mean
dat.year$pcent.diff <- (dat.year$old.mean/dat.year$new.mean -1)*100

#Looking at percent difference instead of absolute
dat.prepost$pcent.diff <- (dat.prepost$old.mean/dat.prepost$new.mean - 1) * 100

#png(width=9, height=8, units="in", res=600, filename= file.path("../Pcent_Difference_In_Precip.png"))
ggplot() +
  geom_line(data=dat.year[dat.year$year >2007,], aes(x=as.numeric(year), y=pcent.diff))+
  ggtitle("% Difference between Precipitation rate (kg2/m2/sec) (Aggregated/ED output)")+
  xlab("Date")+
  ylab("% difference (kg/m2/sec)")
dev.off()

write.csv(dat.prepost, "../NCDF_Precip_Comparision.csv", row.names = F)

#png(width=9, height=8, units="in", res=600, filename= file.path("../Daily_v_Monthly_preicp.png"))
ggplot() +
  facet_wrap(~month, scales = "free")+
  geom_point(data=dat.prepost, aes(x=old.mean, y=new.mean))+
  geom_abline(yintercept=0,slope=1, color = "red") +
  ggtitle("Daily Aggregate (old.mean) vs. monthly ED2 (new.mean)")+
  xlab("Daily Aggregate mean (kg2/m2/sec)")+
  ylab("Monthly ED2 mean (kg/m2/sec)")

dev.off()

ggplot()+
  facet_wrap(~month, scales = "free")+
  geom_vline(xintercept =0)+
  geom_histogram(data = dat.prepost, aes(x = diff))

#png(width=9, height=8, units="in", res=600, filename= file.path("../Monthly_v_Difference.png"))
ggplot() +
  geom_point(data=dat.prepost, aes(x=new.mean, y=diff))+
  geom_smooth(data=dat.prepost, aes(x=new.mean, y=diff))+
  ggtitle("ED2 monthly mean (kg2/m2/sec vs. difference")+
  xlab("Monthly ED2 mean (kg/m2/sec)")+
  ylab("Absolute difference")
dev.off()

#------------------------------------------------------------------#
#This section shows how the two different ncdf results compare to the aggregates that I made
#None of them match
#------------------------------------------------------------------#

dat.2mean <- aggregate(mean~month+year+model+scenario, dat.precip, FUN = mean)

dat.2newmatch <- merge(new.df, dat.2mean, by = c("month", "year"))

dat.2newmatch$diff <- dat.2newmatch$new.mean - dat.2newmatch$mean

dat.2oldmatch <- merge(old.df, dat.2mean, by = c("month", "year"))

dat.2oldmatch$diff <- dat.2oldmatch$old.mean - dat.2oldmatch$mean


#These do not match well at all because of the conversion issues. 
#Shows that the sum method is going to be defunct
dat.sum <- aggregate(sum~month+year+model+scenario, dat.precip, FUN = sum)

dat.sum$sum <- dat.sum$sum * 60 * 60

dat.newmatch <- merge(new.df, dat.sum, by = c("month", "year"))

dat.newmatch$new.mean <- dat.newmatch$new.mean * sec2mo[dat.newmatch$month]

dat.newmatch$diff <- dat.newmatch$new.mean - dat.newmatch$sum

dat.oldmatch <- merge(old.df, dat.sum, by = c("month", "year"))

dat.oldmatch$old.mean <-dat.oldmatch$old.mean * sec2mo[dat.oldmatch$month]

dat.oldmatch$diff <- dat.oldmatch$old.mean - dat.oldmatch$sum

#------------------------#
#Testing zone
#------------------------#

#Testing with temp to see if conversion is the main problem (it is not, temp is also off)
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


