#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script creates dataframes focused on drought and precipitaiton to be used in analaysis scripts
# Inputs: ED2 Morton Arb site data
# Outputs: Dataframes focusing on daily precipitaiton weather and periods of drought
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#

path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis/")
dat.met <- read.csv("../Full_Weather_Daily.csv")
dat.met <- dat.met[dat.met$model %in% unique(runs.all$GCM),]

#Setting up droguht tracking

dat.precip <- dat.met[dat.met$var == "precipitation_flux",]
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


#Adding a date and month object for matching
for(i in 1:nrow(dat.precip)){
  dat.precip[i, "Date"] <- as.Date(dat.precip[i, "yday"], origin = paste(dat.precip[i, "year"], "-01-01", sep = ""))
  dat.precip[i, "month"] <- lubridate::month(dat.precip[i, "Date"])
}

write.csv(dat.precip, "../Precip_Weather_Daily", row.names = F)

dat.precip <- read.csv("../Precip_Weather_Daily")

#Defining the start of a drought for flagging
#We need to pick how many days before a drought "begins" currently using 9 but this should be discussed
#dat.start <- dat.precip[dat.precip$days_since_rain == 10,]
#for(i in 1:nrow(dat.start)){
#  dat.start[i, "Date"] <- as.Date(dat.start[i, "yday"], origin = paste(dat.start[i, "year"], "-01-01", sep = ""))
#  dat.start[i, "month"] <- lubridate::month(dat.start[i, "Date"])
#}


#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.precip)){
  if(dat.precip[i, "days_since_rain"] == 0 & nchar(dat.precip[i-1, "days_since_rain"]) > 1){
    dat.end <- rbind(dat.end, dat.precip[i-1,])
  }
}


write.csv(dat.end, "../Drought_Weather_Daily", row.names = F)
