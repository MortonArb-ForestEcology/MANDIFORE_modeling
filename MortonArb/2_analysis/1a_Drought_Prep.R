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
dat.met <- read.csv("../Aggregate_Weater_Daily.csv")
#dat.met <- dat.met[dat.met$model %in% unique(runs.all$GCM),]

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

dat.precip$Drought_flag <-  ifelse(dat.precip$mean == 0, 1, 0)

write.csv(dat.precip, "../Precip_Weather_Daily", row.names = F)


dat.precip <- read.csv("../Precip_Weather_Daily")

#Creating a dataframe that contains the end date of every drought
#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.precip)){
  if(dat.precip[i, "days_since_rain"] == 0 & nchar(dat.precip[i-1, "days_since_rain"]) > 1){
    dat.end <- rbind(dat.end, dat.precip[i-1,])
  }
}


write.csv(dat.end, "../Drought_Weather_Daily", row.names = F)
