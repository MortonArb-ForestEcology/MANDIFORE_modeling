#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script organizes our model data
# Inputs: Arboretum weather data and MANDIFORE arboretum case study data
# Outputs: ED2 output now summarized to yearly and including precipitation data
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)
library(readbulk)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"


#----------------------------------------------------------------#
#Reading in weather data
#----------------------------------------------------------------#
dat.precip <- read.csv(file.path(path.google, "processed_data/Precip_Weather_Daily.csv"))

dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$mean),]

#Tracking days without rain
dat.precip$no.rain <- ifelse(dat.precip$sum == 0, 1 ,0)

dat.year <- aggregate(sum~year+model+scenario, dat.precip, FUN = sum)

dat.year$rainless.days <- aggregate(no.rain~year+model+scenario, dat.precip, FUN = sum)[, "no.rain"]

#--------------------------------------------------------------#
#Reading in the Mandifore data
#--------------------------------------------------------------#
runs.all <- read_bulk(directory = file.path(path.google, "output"), extension = "Site.csv", header = TRUE)
summary(runs.all)

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

#REMOVING MIROC_ESM_CHEM BECUSE WE DONT HAVE RCP 45 data
runs.all <- runs.all[runs.all$GCM != "MIROC-ESM-CHEM",]

#Aggregating values by mean
runs.year <- aggregate(cbind(tair, VPD, agb, lai, npp,soil.moist.deep, soil.moist.surf ,
                             density.tree, height.sd, height.mean, dbh.mean, dbh.sd)~year+Management+GCM+rcp, runs.all, FUN = mean)

#Joining the precipitation metrics and mandifore output with one dataframe
runs.comb <- merge(runs.year, dat.year, by.x = c("year", "GCM", "rcp"), by.y = c("year", "model", "scenario"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

#Calculating our agb metrics for evaluation
for(i in 2:nrow(runs.comb)){
  
  #Difference between current year and previous year
  GCM <- runs.comb[i, "GCM"]
  rcp <- runs.comb[i, "rcp"]
  MNG <- runs.comb[i, "Management"]
  Year <- runs.comb[i, "year"]
  
  #Calculating the average weather for the first 12 years of model run
  mean.precip <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2020, "sum"])
  mean.VPD <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2020, "VPD"])
  mean.tair <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2020, "tair"])
  
  #Calculating the relative difference in a metric from the mean
  runs.comb[i, "rel.precip"] <- (runs.comb[i, "sum"] - mean.precip)/mean.precip
  runs.comb[i, "rel.VPD"] <- (runs.comb[i, "VPD"] - mean.VPD)/mean.VPD
  runs.comb[i, "rel.tair"] <- (runs.comb[i, "tair"] - mean.tair)/mean.tair
  
  if(Year > 2006 & Year < 2098){
    
    #relative change in precip from one year to the next
    runs.comb[i, "diff.precip"] <- (runs.comb[i, "sum"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"]
    
    #Change in agb
    runs.comb[i, "agb.diff"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"]- runs.comb[i, "agb"]  
    
    #Relative change in agb
    runs.comb[i, "agb.rel.diff"] <- (runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"]- runs.comb[i, "agb"])/runs.comb[i, "agb"] 
    
  }else if(Year>2098){ #This section exists for the last two years where we can't calculate the lead because the model run ends
    #relative change in precip from one year to the next
    runs.comb[i, "diff.precip"] <- (runs.comb[i, "sum"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"]

    #Change in agb
    runs.comb[i, "agb.diff"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"]- runs.comb[i, "agb"]  
    
    #Relative change in agb
    runs.comb[i, "agb.rel.diff"] <- NA
    
  }
  
}

#Adding harvest flag markers
for(i in 1:nrow(runs.comb)){
  if(runs.comb[i, "year"]< 2020){
    runs.comb[i, "harvest"] <- "Pre-harvest"
  }else if(runs.comb[i, "year"]>= 2020 & runs.comb[i, "year"]<= 2024){
    runs.comb[i, "harvest"] <- "Harvest"
  } else {
    runs.comb[i, "harvest"] <- "Post-harvest"
  }
}

write.csv(runs.comb, paste0("../data/All_runs_yearly.csv"), row.names = F)
