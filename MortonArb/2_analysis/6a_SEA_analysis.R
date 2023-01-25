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
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"] <- NA
    for(MNG in unique(runs.yr$Management)){
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      

      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-5)] <- -5
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-4)] <- -4
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-3)] <- -3
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-2)] <- -2
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-1)] <- -1
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% crash.event] <- 0
  
      
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

# Trying to re-center drought event & recovery; 
# I'm not sure exactly how to convert this for our data.

#----------------------------------------------------------------#
# Lucien doesn't understand this segment
#----------------------------------------------------------------#
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    for(YR in unique(runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$lag.crash==0,"year"])){
      
      val.cent <- mean(runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$year %in% (YR-5):(YR-1) & runs.yr$lag.crash<0 & !is.na(runs.yr$lag.crash),"agb"], na.rm=T)
      
      runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$year %in% (YR-5):(YR+0),"agb.extreme"] <- runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$year %in% (YR-5):(YR+0),"agb"] - val.cent
    
      } # end years 
    
  } # end cores
}
summary(runs.yr)


# --------------
# Running the calculation
# --------------
# Setting up a table to stick our output
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
met.var <- c("tair", "VPD", "precip.total", "rel.VPD", "rel.precip", "diff.tair")

drought.resp <- data.frame(lag=rep(-5:0),
                           VAR=rep(unique(met.var), each=length(-5:0)),
                           #MNG=rep(unique(runs.yr$Management), each = 6),
                           estimate=NA,
                           std.err=NA,
                           t.stat=NA,
                           p.val=NA)

  for(COL in met.var){
    
      mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(one.crash)*Management-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$one.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
      
      mod.sum <- summary(mod.lag)
      # mod.sum$tTable
      
      drought.resp[drought.resp$VAR==COL,"estimate"] <- mod.sum$tTable[,"Value"]
      drought.resp[drought.resp$VAR==COL,"std.err"] <- mod.sum$tTable[,"Std.Error"]
      drought.resp[drought.resp$VAR==COL,"t.stat"] <- mod.sum$tTable[,"t-value"]
      drought.resp[drought.resp$VAR==COL,"p.val"] <- mod.sum$tTable[,"p-value"]
      
  }
summary(drought.resp)

ggplot(data=drought.resp) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val>=0.001,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val<0.001,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val<0.001 & drought.resp$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val>=0.001 & drought.resp$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))


