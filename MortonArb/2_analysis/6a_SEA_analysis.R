#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script performs our Superimposed epoch analysis
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: Parts of this are taking from the Drought_04_Analyses_DroughtResponse.R Script by Christy Rollinson
#----------------------------------------------------------------------------------------------------------------------#
library(ggplot2)
library(nlme)
library(multcomp)
#------------------------------------------------------------------------#

path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"


runs.yr <- read.csv(file.path(path.google, "processed_data/All_runs_yearly.csv"))
runs.yr$Management <- factor(runs.yr$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.yr$RCP.name <- car::recode(runs.yr$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.yr$RCP.name <- factor(runs.yr$RCP.name, levels=c("Low Emmissions", "High Emissions"))
runs.yr$crash <- ifelse(runs.yr$agb.rel.diff.future<=-0.2, 1, 0)
runs.yr <- runs.yr[runs.yr$year>=2025,]
summary(runs.yr)

#---------------------------------------------------#
# Here is why I start converting some of Christy's old script for our purposes
#---------------------------------------------------#
# Extreme crash years
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    for(MNG in unique(runs.yr$Management)){
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      
      
      # There's gotta be a better way to do the lag designation, but this works
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"] <- NA
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-5)] <- -5
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-4)] <- -4
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-3)] <- -3
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-2)] <- -2
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-1)] <- -1
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% crash.event] <- 0
      
    }
  }
}
    
summary(runs.yr)

