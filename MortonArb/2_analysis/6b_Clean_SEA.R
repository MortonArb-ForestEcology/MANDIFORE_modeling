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
path.google <- "~/Google Drive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

# path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

path.figures <- file.path(path.google, "Drought and heat analysis/Figures/SEA figures/")

runs.yr <- read.csv(file.path(path.google, "processed_data/All_runs_yearly.csv"))
runs.yr$Management <- factor(runs.yr$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.yr$RCP.name <- car::recode(runs.yr$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.yr$RCP.name <- factor(runs.yr$RCP.name, levels=c("Low Emmissions", "High Emissions"))
runs.yr$loss.event.20 <- ifelse(runs.yr$agb.rel.diff<=-0.2, 1, 0)
summary(runs.yr)
head(runs.yr)

#Counting individual instances of a crash beginning
# CR Note: this was a hard-coded 5:nrow(), but I don't htink that was necessary nor functional since it only skipped the first entries for 1 GCM/RCP combo
for(i in 1:nrow(runs.yr)){ 
  GCM <- runs.yr[i, "GCM"]
  RCP <- runs.yr[i, "rcp"]
  MNG <- runs.yr[i, "Management"]
  YR <- runs.yr[i, "year"]
  if(YR > 2025){ # Need to skip 2025 because this code would reference 2025 off of 2024, which was a management year; this would mess with our analyses
    prev.20 <- runs.yr[runs.yr$GCM == GCM & runs.yr$rcp == RCP & runs.yr$Management == MNG & runs.yr$year == YR-1 , "loss.event.20"]
    runs.yr[i, "nonseq.loss.event.20"] <- ifelse((runs.yr[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

runs.yr$crash <- ifelse(runs.yr$nonseq.loss.event.20==T, 1, 0)
summary(runs.yr)
#runs.yr <- runs.yr[runs.yr$year>=2025,]

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
    # Setting up some dummy columns
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "group.crash.lag"] <- NA
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "group.crash.lag.check"] <- "N"
    
    # For each RCP/GCM, checking for crash in each managment
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
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "ind.crash.lag"] <- "crash"
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "group.crash.lag.check"] <- "Y"
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "ind.crash.lag"] <- "crash"
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "group.crash.lag.check"] <- "Y"
        }
        
        dups.df <- rbind(dups.df, temp.df)
      }
    }
  }
}
summary(dups.df)
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
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == YR, "group.crash.lag"] <- "crash"
        runs.fill <- rbind(runs.fill, temp.df)
        
      }
    }
  }
}

runs.fill$ind.crash.lag <- factor(runs.fill$ind.crash.lag, levels=c("-5", "-4", "-3", "-2", "-1", "crash"))
summary(runs.fill)

time.weath.agg <- aggregate(cbind(diff.tair, rel.precip, rel.VPD)~ind.crash.lag, data = runs.fill[!is.na(runs.fill$ind.crash.lag),], FUN = mean, na.action = NULL)
time.weath.agg[, c("diff.tair.sd", "rel.precip.sd", "rel.VPD.sd")] <- aggregate(cbind(diff.tair, rel.precip, rel.VPD)~ind.crash.lag, data = runs.fill[!is.na(runs.fill$ind.crash.lag),], FUN = sd, na.action = NULL)[, c("diff.tair", "rel.precip", "rel.VPD")]

write.csv(time.weath.agg, file.path(path.google, "processed_data/Time_by_relweather.csv"), row.names = F)


#-----------------------------------------------------#
# Here is where we start running the analysis to make figures
#-----------------------------------------------------#
#-----------------------------------------------------#
# Looking at relative weather before a crash
# ind.crash.lag = time lag for individual management which crashed
# We include the crash year for this evaluation because we are working with temperature
#-----------------------------------------------------#
summary(runs.fill)

raw.met.tair <- ggplot(data=runs.fill[!is.na(runs.fill$ind.crash.lag),], aes(x=ind.crash.lag, y=diff.tair, group=Management), position=dodge) +
  geom_errorbar(aes(color=Management), stat="summary", fun.y="sd", size=1.25, alpha=0.5) +
  geom_line(aes(color=Management), stat="summary", fun="mean", size=1.5) +
  geom_point(aes(color=Management), stat="summary", fun="mean", size=2) +
  scale_color_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme_bw() + theme(axis.title.x=element_blank(), panel.spacing.y = unit(2, "lines"))

raw.met.precip <- ggplot(data=runs.fill[!is.na(runs.fill$ind.crash.lag),], aes(x=ind.crash.lag, y=rel.precip, group=Management), position=dodge) +
  geom_errorbar(aes(color=Management), stat="summary", fun.y="sd", size=1.25, alpha=0.5) +
  geom_line(aes(color=Management), stat="summary", fun="mean", size=1.5) +
  geom_point(aes(color=Management), stat="summary", fun="mean", size=2) +
  scale_color_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme_bw() + theme(axis.title.x=element_blank(), panel.spacing.y = unit(2, "lines"))

raw.met.vpd <- ggplot(data=runs.fill[!is.na(runs.fill$ind.crash.lag),], aes(x=ind.crash.lag, y=rel.VPD, group=Management), position=dodge) +
  geom_errorbar(aes(color=Management), stat="summary", fun.y="sd", size=1.25, alpha=0.5) +
  geom_line(aes(color=Management), stat="summary", fun="mean", size=1.5) +
  geom_point(aes(color=Management), stat="summary", fun="mean", size=2) +
  scale_color_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme_bw() + theme(axis.title.x=element_blank(), panel.spacing.y = unit(2, "lines"))

png(file.path(path.figures, "SEA_RelWeather_TimeMgmt_RawDat.png"), width=12, height=8, units="in", res=220)
cowplot::plot_grid(raw.met.tair, raw.met.precip, raw.met.vpd, ncol=2)
dev.off()



relmet.var <- c("rel.precip", "diff.tair", "rel.VPD")
df.lag.relmetxind <- data.frame()
df.ano.relmetxind <- data.frame()
df.time.relmetxind <- data.frame()
df.mgmt.relmetxind <- data.frame()
df.plot.relmetxind <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5")*relevel(Management, "None")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  anova(mod.lag)

  df.ano <- data.frame(anova(mod.lag))
  # output <- summary(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  rownames(df.ano) <- NULL
  
  df.ano.relmetxind <- rbind(df.ano.relmetxind, df.ano)
  
  # This permutation of the analysis will let us get the random effects adjusted means & SEs for each time/mgmt group to make a clean figure; do NOT interpret the p-values as they'll just indicate difference from 0
  # NOTE: The random effects make HUGE differences on the directionality of means (e.g. w/o R.E. SHelter has *higher* has diff.tair, but when including R.E. it's *lower*)
  # --> this means that it's important to be showing the mixed model estiamtes, BUT the SE associated with each point are similar and very large and make it really hard to tease out the stat sig that we describe
  mod.plot <- nlme::lme(eval(substitute(j ~ relevel(ind.crash.lag, "-5")*relevel(Management, "None")- relevel(ind.crash.lag, "-5") - relevel(Management, "None") -1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  

  df.plot <- data.frame(summary(mod.plot)$tTable)
  df.plot$time <- c("-5", "-4", "-3", "-2", "-1", "crash")
  df.plot$Management <- rep(c("None", "Under", "Shelter", "Gap"), each=6)
  df.plot$VAR <- COL
  rownames(df.plot) <- NULL
  
  df.plot.relmetxind <- rbind(df.plot.relmetxind, df.plot)
  
    
  # Doing the univariate analysis & saving output
  # time to crash
  mod.time <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1, Management=~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  
  df.time <- data.frame(summary(mod.time)$tTable)
  df.time$comp <- c("-5", "-4", "-3", "-2", "-1", "crash")
  df.time$VAR <- COL
  rownames(df.time) <- NULL
  
  df.time.relmetxind <- rbind(df.time.relmetxind, df.time)
  
  # management
  mod.mgmt <- nlme::lme(eval(substitute(j ~ relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  
  
  df.mgmt <- data.frame(summary(mod.mgmt)$tTable)
  df.mgmt$comp <- c("None", "Under", "Shelter", "Gap")
  df.mgmt$VAR <- COL
  rownames(df.mgmt) <- NULL
  
  df.mgmt.relmetxind <- rbind(df.mgmt.relmetxind, df.mgmt)
  
  # eff.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  # mod.lag.eff <- summary(mod.lag)$tTable
}

df.ano.relmetxind
# summary(df.lag.relmetxind)

# df.ano.relmetxind <- df.ano.relmetxind[,c(6,5,1,2,3,4)]
df.ano.relmetxind <- df.ano.relmetxind[,c("VAR", "comp", "numDF", "denDF", "F.value", "p.value")]
df.ano.relmetxind$comp <- gsub("relevel", "", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub('"', "", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub('[(]', "", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub('[)]', "", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub("as.factorind.crash.lag, -5", "Time", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub("Management, None", "Harvest Scenario", df.ano.relmetxind$comp)
#IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
df.ano.relmetxind$p.value <- round(df.ano.relmetxind$p.value, 3)
# View(df.ano.relmetxind)
df.ano.relmetxind

df.time.relmetxind$p.value <- round(df.time.relmetxind$p.value, 3)
df.time.relmetxind

df.mgmt.relmetxind$p.value <- round(df.mgmt.relmetxind$p.value, 3)
df.mgmt.relmetxind

write.csv(df.ano.relmetxind, file.path(path.google, "Drought and heat analysis", "Mixed effects models results/SEA_ANOVA_RelMet_TimeMgmt.csv"), row.names = F)
write.csv(df.time.relmetxind, file.path(path.google, "Drought and heat analysis", "Mixed effects models results/SEA_ANOVA_RelMet_Time.csv"), row.names = F)
write.csv(df.mgmt.relmetxind, file.path(path.google, "Drought and heat analysis", "Mixed effects models results/SEA_ANOVA_RelMet_Mgmt.csv"), row.names = F)



df.plot.relmetxind$time <- factor(df.plot.relmetxind$time, levels=c("-5", "-4", "-3", "-2", "-1", "crash"))
ggplot(data=df.plot.relmetxind, aes(x=time, y=Value, group=Management, color=Management)) +
  facet_wrap(~VAR, scales="free_y", ncol=2) +
  geom_line(size=1.5) + 
  geom_point(size=2) +
  geom_errorbar(aes(x=time, ymin=Value-Std.Error, ymax=Value+Std.Error, group=Management, color=Management), size=1.5, alpha=0.5) +
  scale_color_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme_bw() + theme(axis.title.x=element_blank(), panel.spacing.y = unit(2, "lines"))




# #-----------------------------------------------------#
# # Looking at relative weather before a crash to make a figure
# # ind.crash.lag = time lag for individual management which crashed
# # We include the crash year for this evaluation because we are working with temperature
# # This is just to create a figure so we can investigate the directionality of our variables
# #-----------------------------------------------------#
# df.lag.rel <- data.frame()
# for(COL in relmet.var){
#   
#   # Updating this to be compared to year 5; then need to add the value to the others to get the absolute magnitudes down the road
#   mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
#   # mod.lag2 <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "crash")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
#   # output2 <- summary(mod.lag2)
#   
#   output <- summary(mod.lag)
#   lag.list.rel <- list()
#   lag.list.rel[[paste(COL)]]$VAR <- COL
#   lag.list.rel[[paste(COL)]]$Comp <- rownames(output$tTable)
#   lag.list.rel[[paste(COL)]]$estimate <- output$tTable[,"Value"]
#   lag.list.rel[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
#   lag.list.rel[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
#   lag.list.rel[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
#   temp.lag.rel <- dplyr::bind_rows(lag.list.rel)
#   temp.lag.rel$lag <- c("-5", -1, -2, -3, -4, "crash")
#   
#   df.lag.rel <- rbind(df.lag.rel, temp.lag.rel)
#   
# }
# output <- summary(mod.lag)
# 
# summary(df.lag.rel)
# 
# plot.rel <- ggplot(data=df.lag.rel[df.lag.rel$lag!="-5",] ) +
#   facet_wrap(~VAR, scales = "free_y") +
#   geom_bar(data=df.lag.rel[df.lag.rel$lag!="-5",], aes(x=as.factor(lag), y=estimate), stat="identity") +
#   geom_errorbar(data=df.lag.rel[df.lag.rel$lag!="-5",], aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
#   theme(panel.spacing = unit(0, "lines"),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"))+
#   scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1, "crash")))+
#   ggtitle("Relative weather before crashes")
# 
# png(paste0(path.figures, "RelWeather_before_crash_hist.png"), width=12, height=8, units="in", res=220)
#   plot.rel
# dev.off()

#-----------------------------------------------------#
# Looking at structure before a crash
# ind.crash.lag = time lag for individual management which crashed
# We exclude the year of crash
#-----------------------------------------------------#
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.strucxind <- data.frame()
df.ano.strucxind <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag) & runs.fill$ind.crash.lag!="crash",], na.action = na.omit)
  anova(mod.lag)
  
  df.ano <- data.frame(anova(mod.lag))
  output <- summary(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  rownames(df.ano) <- NULL
  
  df.ano.strucxind <- rbind(df.ano.strucxind, df.ano)
  
}

df.ano.strucxind
df.ano.strucxind <- df.ano.strucxind[,c("VAR", "comp", "numDF", "denDF", "F.value", "p.value")]
df.ano.strucxind$comp <- gsub("relevel", "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub('"', "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub('[(]', "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub('[)]', "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub("as.factorind.crash.lag, -5", "Time", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub("Management, None", "Harvest Scenario", df.ano.strucxind$comp)
#IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
df.ano.strucxind$p.value <- round(df.ano.strucxind$p.value, 3)
df.ano.strucxind
#df.ano.strucx <- df.ano.strucxind[df.ano.strucxind$VAR== "tree.dbh.sd",]
write.csv(df.ano.strucxind, file.path(path.google, "Drought and heat analysis", "Mixed effects models results/SEA_ANOVA_Structure_TimeMgmt.csv"), row.names = F)

# 
# #-----------------------------------------------------#
# # Looking at Structure before a crash to make a figure
# # ind.crash.lag = time lag for individual management which crashed
# # We include the crash year for this evaluation because we are working with temperature
# # This is just to create a figure so we can investigate the directionality of our variables
# #-----------------------------------------------------#
# df.lag.struc <- data.frame()
# for(COL in struc.var){
#   
#   # This one will get statistical difference relative to the start
#   mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag) & runs.fill$ind.crash.lag != "crash",], na.action = na.omit)
#   
#   # # This one will show means etc. & get assess stat sig as difference from 0, which isn't inherently meaningful because everything should be non-0; the way to show this would be breakign down by the anovas above
#   # mod.lag2 <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-5")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag) & runs.fill$ind.crash.lag != "crash",], na.action = na.omit)
#   # output2 <- summary(mod.lag2)
#   
#   output <- summary(mod.lag)
#   lag.list.struc <- list()
#   lag.list.struc[[paste(COL)]]$VAR <- COL
#   lag.list.struc[[paste(COL)]]$Comp <- rownames(output$tTable)
#   lag.list.struc[[paste(COL)]]$estimate <- output$tTable[,"Value"]
#   lag.list.struc[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
#   lag.list.struc[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
#   lag.list.struc[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
#   temp.lag.struc <- dplyr::bind_rows(lag.list.struc)
#   temp.lag.struc$lag <- c("-5", -1, -2, -3, -4)
#   
#   df.lag.struc <- rbind(df.lag.struc, temp.lag.struc)
#   
# }
# # output <- summary(mod.lag)
# 
# summary(df.lag.struc)
# 
# plot.struc <- ggplot(data=df.lag.struc[df.lag.struc$lag!="-5",] ) +
#   facet_wrap(~VAR, scales = "free_y") +
#   geom_bar(data=df.lag.struc[df.lag.struc$lag!="-5",], aes(x=as.factor(lag), y=estimate), stat="identity") +
#   geom_errorbar(data=df.lag.struc[df.lag.struc$lag!="-5",], aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
#   theme(panel.spacing = unit(0, "lines"),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"))+
#   scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
#   ggtitle("Structure before crashes")
# 
# png(paste0(path.figures, "Structure_before_crash_hist.png"), width=12, height=8, units="in", res=220)
#   plot.struc
# dev.off()
# 
# #-----------------------------------------------------#
# # Looking for structural differences between the conditions that crashed and those that didn't by Management
# # Looking if those differences vary by Management
# # group.crash.lag = time lag for GROUP of conditions with at least ONE RUN crashing
# # group.crash.lag.check --> Y/N indicating which set actually crashed
# #-----------------------------------------------------#
# df.lag.strucxcrashxmng <- data.frame()
# df.ano.strucxcrashxmng <- data.frame()
# for(COL in struc.var){
#   
#   # Checkign to see if there's anything if we move MGMT to FIXED
#   mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(group.crash.lag), "-1")*relevel(as.factor(group.crash.lag.check), "N")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$group.crash.lag) & runs.fill$group.crash.lag!="crash",], na.action = na.omit)
#   anova(mod.lag)
#   
#   df.ano <- anova(mod.lag)
#   output <- summary(mod.lag)
#   df.ano$comp <- rownames(df.ano)
#   df.ano$VAR <- COL
#   rownames(df.ano) <- NULL
#   
#   lag.list.strucxcrashxmng <- list()
#   lag.list.strucxcrashxmng[[paste(COL)]]$VAR <- COL
#   lag.list.strucxcrashxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
#   lag.list.strucxcrashxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
#   lag.list.strucxcrashxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
#   lag.list.strucxcrashxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
#   lag.list.strucxcrashxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
#   temp.lag.strucxcrashxmng <- dplyr::bind_rows(lag.list.strucxcrashxmng)
#   temp.lag.strucxcrashxmng$lag <- c(NA, -2,-3,-4,-5, NA, NA, NA, NA, rep(unique(-2:-5), times = 4), NA, NA, NA, rep(unique(-2:-5), times = 3))
#   temp.lag.strucxcrashxmng$crash <- c(NA, NA, NA, NA, NA, "Y", NA, NA, NA, "Y", "Y", "Y", "Y", rep(c(NA), each = 12), rep(c("Y"), each = 15))
#   temp.lag.strucxcrashxmng$Management <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap", NA, NA, NA, NA, rep(c("Under", "Shelter", "Gap"), each = 4), "Under", "Shelter", "Gap", rep(c("Under", "Shelter", "Gap"), each = 4))
#   
#   df.lag.strucxcrashxmng <- rbind(df.lag.strucxcrashxmng, temp.lag.strucxcrashxmng)
#   df.ano.strucxcrashxmng <- rbind(df.ano.strucxcrashxmng, df.ano)
#   
# }
# summary(df.ano.strucxcrashxmng)
# df.ano.strucxcrashxmng <- df.ano.strucxcrashxmng[,c(6,5,1,2,3,4)]
# 
# df.ano.strucxcrashxmng$comp <- gsub("(group.crash.lag)", "Time", df.ano.strucxcrashxmng$comp)
# df.ano.strucxcrashxmng$comp <- gsub("Time.check", "CrashY/N", df.ano.strucxcrashxmng$comp)
# df.ano.strucxcrashxmng$comp <- gsub("-1", "", df.ano.strucxcrashxmng$comp)
# df.ano.strucxcrashxmng$comp <- gsub("None", "", df.ano.strucxcrashxmng$comp)
# df.ano.strucxcrashxmng$comp <- gsub("relevel", "", df.ano.strucxcrashxmng$comp)
# 
# #IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
# df.ano.strucxcrashxmng$`p-value` <- round(df.ano.strucxcrashxmng$`p-value`, 5)
# 
# #df.ano.strucx <- df.ano.strucxcrashxmng[df.ano.strucxcrashxmng$VAR== "density.tree",]
# 
# write.csv(df.ano.strucxcrashxmng, file.path(path.google, "processed_data/strucxcrashxmng_anova.csv"), row.names = F)
# 
# plot.strucxmng <- ggplot(data=df.lag.strucxcrashxmng ) +
#   facet_wrap(~VAR, scales = "free_y") +
#   geom_bar(data=df.lag.struc, aes(x=as.factor(lag), y=estimate), stat="identity") +
#   geom_errorbar(data=df.lag.struc, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
#   theme(panel.spacing = unit(0, "lines"),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"))+
#   scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
#   ggtitle("Structure before crashes")
# 
# plot.mngxcrash <- ggplot(data=runs.fill) +
#   facet_wrap(~VAR, scales = "free_y") +
#   geom_bar(data=df.lag.struc, aes(x=as.factor(lag), y=estimate), stat="identity") +
#   geom_errorbar(data=df.lag.struc, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
#   theme(panel.spacing = unit(0, "lines"),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"))+
#   scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
#   ggtitle("Structure before crashes")
# 
# dat.strucxmng <- runs.fill[!is.na(runs.fill$group.crash.lag), c("year", "Management", "GCM", "rcp", struc.var, "group.crash.lag", "ind.crash.lag", "group.crash.lag.check")]
# #Just to make "lag" a common name for merging purposes
# colnames(dat.strucxmng) <- c("year", "Management", "GCM", "rcp", struc.var, "lag", "ind.crash.lag", "group.crash.lag.check")
# summary(dat.strucxmng)
# 
# #Making the format wide so that we can facet our different relative weather variables
# dat.strucxmng <- tidyr::gather(dat.strucxmng, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)
# 
# #Merging the frames and marking significance
# dat.strucxmng <- merge(dat.strucxmng, df.lag.strucxcrashxmng, all.x=T,)
# dat.strucxmng$sig[!is.na(dat.strucxmng$p.val)] <- ifelse(dat.strucxmng$p.val[!is.na(dat.strucxmng$p.val)]<0.05, "sig", "n.s.")
# dat.strucxmng$sig <- as.factor(dat.strucxmng$sig)
# summary(dat.strucxmng)
# 
# dat.strucxmng$VAR <- car::recode(dat.strucxmng$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
#                              'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")
# 
# plot.strucxmng <- ggplot(data=dat.strucxmng[!is.na(dat.strucxmng$lag),]) +
#   facet_grid(VAR~Management, scales="free_y") +
#   geom_boxplot(aes(x=as.factor(lag), y=value, fill =group.crash.lag.check, color = group.crash.lag.check)) +
#   scale_x_discrete(name="Loss event Lag") +
#   scale_y_continuous(name="Difference") +
#   theme(legend.position = "top",
#         legend.key = element_rect(fill=NA),
#         panel.spacing = unit(0, "lines"),
#         panel.grid = element_blank(),
#         panel.background=element_rect(fill=NA, color="black"))
# 
# png(paste0(path.figures, "StrucxMNG_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
#   plot.strucxmng
# dev.off()