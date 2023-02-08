#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script performs a cleaned up version of our Superimposed epoch analysis
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: This is a cleaner version of the anlysis scripts found in 6a_SEA_analysis.R
#----------------------------------------------------------------------------------------------------------------------#
library(ggplot2)
library(nlme)
library(multcomp)
library(dplyr)
#------------------------------------------------------------------------#

#path.google <- "~/Library/CloudStorage/GoogleDrive-crollinson@mortonarb.org/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

path.figures <- file.path(path.google, "Drought and heat analysis/Figures/SEA figures/")

runs.yr <- read.csv(file.path(path.google, "processed_data/All_runs_yearly.csv"))
runs.yr$Management <- factor(runs.yr$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.yr$RCP.name <- car::recode(runs.yr$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.yr$RCP.name <- factor(runs.yr$RCP.name, levels=c("Low Emmissions", "High Emissions"))
runs.yr$loss.event.20 <- ifelse(runs.yr$agb.rel.diff.future<=-0.2, 1, 0)
summary(runs.yr)

#Counting individual instances of a crash beginning
for(i in 5:nrow(runs.yr)){
  GCM <- runs.yr[i, "GCM"]
  RCP <- runs.yr[i, "rcp"]
  MNG <- runs.yr[i, "Management"]
  YR <- runs.yr[i, "year"]
  if(YR >= 2025){
    prev.20 <- runs.yr[runs.yr$GCM == GCM & runs.yr$rcp == RCP & runs.yr$Management == MNG & runs.yr$year == YR-1 , "loss.event.20"]
    runs.yr[i, "nonseq.loss.event.20"] <- ifelse((runs.yr[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

runs.yr$crash <- ifelse(runs.yr$nonseq.loss.event.20==T, 1, 0)
#runs.yr <- runs.yr[runs.yr$year>=2025,]

#-----------------------------------------------------------#
# Creating a summary table on the frequency of crashes by RCP, GCM, and Management
#-----------------------------------------------------------#

rcp.freq.df <- runs.yr[runs.yr$nonseq.loss.event.20 == T & !is.na(runs.yr$nonseq.loss.event.20) & runs.yr$year>=2025,] %>%
  group_by(rcp, nonseq.loss.event.20) %>%
  summarize(Freq=n())

rcp.freq.df

GCM.freq.df <- runs.yr[runs.yr$nonseq.loss.event.20 == T & !is.na(runs.yr$nonseq.loss.event.20) & runs.yr$year>=2025,] %>%
  group_by(GCM, nonseq.loss.event.20) %>%
  summarize(Freq=n())

GCM.freq.df

MNG.freq.df <- runs.yr[runs.yr$nonseq.loss.event.20 == T & !is.na(runs.yr$nonseq.loss.event.20) & runs.yr$year>=2025,] %>%
  group_by(Management, nonseq.loss.event.20) %>%
  summarize(Freq=n())

MNG.freq.df


#---------------------------------------------------#
# Here is why I start converting some of Christy's old script for our purposes
# This script pulls out duplicate situations into a different data.frame and adds them back at thend
# Duplicates refer to when one year can be a -1 year lag for one drought and a -5 for another
# group.crash.lag = time lag for GROUP of conditions with at least ONE RUN crashing (This was previously group.crash.lag)
# group.crash.lag.check --> Y/N indicating which set actually crashed (This was previously group.crash.lag.check)
# ind.crash.lag = time lag for individual management condition which crashed
#---------------------------------------------------#
# Extreme crash years
dups.df <- data.frame()
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "group.crash.lag"] <- NA
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "group.crash.lag.check"] <- "N"
    for(MNG in unique(runs.yr$Management)){
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "ind.crash.lag"] <- NA
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      for(YR in crash.event){
        
        temp.df <- data.frame()
        #Flagging when only one of the management scenarios has a crash
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "ind.crash.lag"] <- -5
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "group.crash.lag.check"] <- "Y"
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "ind.crash.lag"] <- -5
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "group.crash.lag.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "ind.crash.lag"] <- -4
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "group.crash.lag.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "ind.crash.lag"] <- -4
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "group.crash.lag.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "ind.crash.lag"] <- -3
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "group.crash.lag.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "ind.crash.lag"] <- -3
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "group.crash.lag.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "ind.crash.lag"] <- -2
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "group.crash.lag.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "ind.crash.lag"] <- -2
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "group.crash.lag.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "ind.crash.lag"] <- -1
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "group.crash.lag.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "ind.crash.lag"] <- -1
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "group.crash.lag.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "ind.crash.lag"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "ind.crash.lag"] <- 0
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "group.crash.lag.check"] <- "Y"
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "ind.crash.lag"] <- 0
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "group.crash.lag.check"] <- "Y"
        }
        
        dups.df <- rbind(dups.df, temp.df)
      }
    }
  }
}
runs.yr <- runs.yr[runs.yr$year>=2025,]

#Adding the duplicate values back into the data frame
runs.yr <- rbind(runs.yr, dups.df)

runs.fill <- data.frame()
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "group.crash.lag"] <- NA
    for(MNG in unique(runs.yr$Management)){
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      for(YR in crash.event){
        temp.df <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$year >= (YR-5) & runs.yr$year <= (YR),]
        temp.df$crash.year <- YR
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-5), "group.crash.lag"] <- -5
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-4), "group.crash.lag"] <- -4
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-3), "group.crash.lag"] <- -3
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-2), "group.crash.lag"] <- -2
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-1), "group.crash.lag"] <- -1
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == YR, "group.crash.lag"] <- 0
        runs.fill <- rbind(runs.fill, temp.df)
        
      }
    }
  }
}

summary(runs.fill)

