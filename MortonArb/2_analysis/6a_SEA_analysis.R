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
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash.check"] <- "N"
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
      
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"] <- NA
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-5)] <- "Y"
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-4)] <- "Y"
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-3)] <- "Y"
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-2)] <- "Y"
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% (crash.event-1)] <- "Y"
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "one.crash.check"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG , "year"] %in% crash.event] <- "Y"
      

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

runs.yr$one.crash.check <- ifelse(is.na(runs.yr$one.crash.check), "N", runs.yr$one.crash.check)
# --------------
# Running the calculation
# --------------
#Working with non-relative weather metrics
# Calculation is looking at scenarios where one management crashed and is comparing the ones that didn't crash and those that did
#I'm not labeling which management experienced a crash here so I think I'm missing something to flesh this out. 
#Currently this is comparing across periods in time where one management crashed and others didn't but I'm not flagging which crashed and which didn't
#How can I incorporate? I could add whether a crash occurred as a factor through the lag labeling. But then we are going threeway which is crazy
met.var <- c("tair", "VPD", "precip.total")
df.lag.metxmng <- data.frame()
for(COL in met.var){
    
    mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(one.crash)*as.factor(Management)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$one.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
      
    output <- summary(mod.lag)
      
    lag.list.metxmng <- list()
    lag.list.metxmng[[paste(COL)]]$VAR <- COL
    lag.list.metxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
    lag.list.metxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
    lag.list.metxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
    lag.list.metxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
    lag.list.metxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
    temp.lag.metxmng <- dplyr::bind_rows(lag.list.metxmng)
    temp.lag.metxmng$lag <- c(-5,-4,-3,-2,-1,0, NA, NA, NA, rep(unique(-4:0), times = 3))
    temp.lag.metxmng$MNG <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap", rep(c("Under", "Shelter", "Gap"), each = 5))
      
    df.lag.metxmng <- rbind(df.lag.metxmng, temp.lag.metxmng)
  }
summary(df.lag.metxmng)

ggplot(data=df.lag.metxmng ) +
  facet_wrap(MNG~VAR, scales = "free_y") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val<0.05 & df.lag.metxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val>=0.05 & df.lag.metxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))


#Looking at relative weather
# Calculation is looking at scenarios where one management crashed and is comparing the ones that didn't crash and those that did
relmet.var <- c("rel.precip", "diff.tair", "rel.VPD")
df.lag.relxmng <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(one.crash)*as.factor(Management)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$one.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.relxmng <- list()
  lag.list.relxmng[[paste(COL)]]$VAR <- COL
  lag.list.relxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.relxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.relxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.relxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.relxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.relxmng <- dplyr::bind_rows(lag.list.relxmng)
  temp.lag.relxmng$lag <- c(-5,-4,-3,-2,-1,0, NA, NA, NA, rep(unique(-4:0), times = 3))
  temp.lag.relxmng$MNG <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap", rep(c("Under", "Shelter", "Gap"), each = 5))
  
  df.lag.relxmng <- rbind(df.lag.relxmng, temp.lag.relxmng)
}
summary(df.lag.relxmng)

ggplot(data=df.lag.relxmng ) +
  facet_wrap(MNG~VAR, scales = "free_y") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val<0.05 & df.lag.relxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val>=0.05 & df.lag.relxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))



#Looking at structural metrics on their own
# Calculation is looking at scenarios where one management crashed and is comparing the ones that didn't crash and those that did
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.struc <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.struc <- list()
  lag.list.struc[[paste(COL)]]$VAR <- COL
  lag.list.struc[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.struc[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.struc[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.struc[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.struc[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.struc <- dplyr::bind_rows(lag.list.struc)
  temp.lag.struc$lag <- c(-5,-4,-3,-2,-1, 0)

  df.lag.struc <- rbind(df.lag.struc, temp.lag.struc)
}
summary(df.lag.struc)

ggplot(data=df.lag.struc ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val<0.05 & df.lag.struc$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val>=0.05 & df.lag.struc$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))



#Looking at structural metrics with management
# Calculation is looking at scenarios where one management crashed and is comparing the ones that didn't crash and those that did
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.strucxmng <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(one.crash)*as.factor(Management)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$one.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.strucxmng <- list()
  lag.list.strucxmng[[paste(COL)]]$VAR <- COL
  lag.list.strucxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.strucxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.strucxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.strucxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.strucxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.strucxmng <- dplyr::bind_rows(lag.list.strucxmng)
  temp.lag.strucxmng$lag <- c(-5,-4,-3,-2,-1,0, NA, NA, NA, rep(unique(-4:0), times = 3))
  temp.lag.strucxmng$MNG <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap", rep(c("Under", "Shelter", "Gap"), each = 5))
  
  df.lag.strucxmng <- rbind(df.lag.strucxmng, temp.lag.strucxmng)
}
summary(df.lag.strucxmng)

ggplot(data=df.lag.strucxmng ) +
  facet_wrap(MNG~VAR, scales = "free_y") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val<0.05 & df.lag.strucxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val>=0.05 & df.lag.strucxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

