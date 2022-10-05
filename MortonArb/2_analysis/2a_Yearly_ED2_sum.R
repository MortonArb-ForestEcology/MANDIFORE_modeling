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
# dat.precip <- read.csv(file.path(path.google, "processed_data/Precip_Weather_Daily.csv"))
dat.precip <- read.csv(file.path(path.google, "processed_data/Met_Precip_Daily.csv"))

dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$precip.mean),]

#Tracking days without rain
dat.precip$no.rain <- ifelse(dat.precip$precip.mean == 0, 1 ,0)

dat.year <- aggregate(precip.mean~year+model+scenario,data=dat.precip, FUN = mean)
dat.year$precip.total <- dat.year$precip.mean*365*24*60*60 # should now be in mm/yr

# dat.year <- aggregate(sum~year+model+scenario, dat.precip, FUN = sum)

dat.year$rainless.days <- aggregate(no.rain~year+model+scenario, dat.precip, FUN = sum)[, "no.rain"]

#--------------------------------------------------------------#
#Reading in the Mandifore data
#--------------------------------------------------------------#
runs.all <- read_bulk(directory = file.path(path.google, "output"), extension = "Site.csv", header = TRUE)
summary(runs.all)

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

#REMOVING MIROC_ESM_CHEM BECUSE WE DONT HAVE RCP 45 data
runs.all <- runs.all[runs.all$GCM != "MIROC-ESM-CHEM" & runs.all$year>2006,]

#Aggregating values by mean
runs.year <- aggregate(cbind(tair, VPD, agb, lai, npp,soil.moist.deep, soil.moist.surf ,
                             density.tree, height.sd, height.mean, dbh.mean, dbh.sd)~year+Management+GCM+rcp, runs.all, FUN = mean)

#Joining the precipitation metrics and mandifore output with one dataframe
runs.comb <- merge(runs.year, dat.year, by.x = c("year", "GCM", "rcp"), by.y = c("year", "model", "scenario"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))
runs.comb$harvest <- ifelse(runs.comb$year<2020, "pre-harvest", ifelse(runs.comb$year %in% 2020:2024, "harvest", "post-harvest")) 

#Calculating our agb metrics for evaluation
runs.comb[,c("agb.diff", "agb.rel.diff", "agb.diff.future", "agb.rel.diff.future")] <- NA
for(GCM in unique(runs.comb$GCM)){
  for(RCP in unique(runs.comb$rcp[runs.comb$GCM==GCM])){
    for(MGMT in unique(runs.comb$Management[runs.comb$GCM==GCM & runs.comb$rcp==RCP])){
      run.rows <- which(runs.comb$Management==MGMT & runs.comb$rcp==RCP & runs.comb$GCM==GCM)
      rows.ref <- which(runs.comb$Management==MGMT & runs.comb$rcp==RCP & runs.comb$GCM==GCM & runs.comb$year<2020)
      
      #Calculating the average weather for the first 12 years of model run
      mean.precip <- mean(runs.comb[rows.ref, "precip.total"])
      mean.VPD <- mean(runs.comb[rows.ref, "VPD"])
      mean.tair <- mean(runs.comb[rows.ref, "tair"])
      
      #Calculating the relative difference in a metric from the mean
      # This percent difference only works when the 0 has a real value; doesn't work for temp!
      runs.comb[run.rows, "diff.precip"] <- runs.comb[run.rows, "precip.total"] - mean.precip
      runs.comb[run.rows, "rel.precip"] <- (runs.comb[run.rows, "precip.total"] - mean.precip)/mean.precip
      runs.comb[run.rows, "diff.VPD"] <- runs.comb[run.rows, "VPD"] - mean.VPD
      runs.comb[run.rows, "rel.VPD"] <- (runs.comb[run.rows, "VPD"] - mean.VPD)/mean.VPD
      runs.comb[run.rows, "diff.tair"] <- runs.comb[run.rows, "tair"] - mean.tair
      
      # Calculating running change in AGB
      dat.run <- runs.comb[run.rows,]
      
      for(YR in 2007:2099){
        
        agb.now <- dat.run$agb[dat.run$year==YR]
        if(YR>2007){
          agb.past <- dat.run$agb[dat.run$year==(YR-1)]
          dat.run$agb.diff[dat.run$year==YR] <- agb.now - agb.past
          dat.run$agb.rel.diff[dat.run$year==YR] <- (agb.now - agb.past)/agb.past
        }
        
        if(YR<2099){ # If we're not at the end; add the future state
          agb.future <- dat.run$agb[dat.run$year==(YR+1)]
          dat.run$agb.diff.future[dat.run$year==YR] <- agb.future - agb.now
          dat.run$agb.rel.diff.future[dat.run$year==YR] <- (agb.future - agb.now)/agb.now
        }
      }
      runs.comb[run.rows, c("agb.diff", "agb.rel.diff", "agb.diff.future", "agb.rel.diff.future")] <- dat.run[,c("agb.diff", "agb.rel.diff", "agb.diff.future", "agb.rel.diff.future")]
      
    }
  }
}


write.csv(runs.comb, paste0("../data/All_runs_yearly.csv"), row.names = F)
write.csv(runs.comb, file.path(path.google, "processed_data/All_runs_yearly.csv"), row.names = F)
