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

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/SEA figures/"

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
  if(YR != 2007){
    prev.20 <- runs.yr[runs.yr$GCM == GCM & runs.yr$rcp == RCP & runs.yr$Management == MNG & runs.yr$year == YR-1 , "loss.event.20"]
    runs.yr[i, "nonseq.loss.event.20"] <- ifelse((runs.yr[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

runs.yr$crash <- ifelse(runs.yr$nonseq.loss.event.20==T, 1, 0)
runs.yr <- runs.yr[runs.yr$year>=2025,]

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
      

      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-5)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-5)]), -5, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-5)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-4)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-4)]), -4, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-4)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-3)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-3)]), -3, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-3)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-2)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-2)]), -2, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-2)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-1)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-1)]), -1, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-1)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-0)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-0)]), 0, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "year"] %in% (crash.event-0)])
      
      
      # There's gotta be a better way to do the lag designation, but this works
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"] <- NA
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-5)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-5)]), -5, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-5)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-4)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-4)]), -4, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-4)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-3)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-3)]), -3, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-3)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-2)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-2)]), -2, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-2)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-1)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-1)]), -1, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-1)])
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-0)] <- 
        ifelse(is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-0)]), 0, runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"][runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "year"] %in% (crash.event-0)])
      
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
runs.yr$one.crash.check <- ifelse(is.na(runs.yr$one.crash.check), "N", runs.yr$one.crash.check)

# Trying to re-center drought event & recovery; 
# I'm not sure exactly how to convert this for our data.
full.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd", "tair", "VPD", "precip.total", "rel.precip", "diff.tair", "rel.VPD")

#----------------------------------------------------------------#
# Lucien doesn't understand this segment
#----------------------------------------------------------------#
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    for(MNG in unique(runs.yr$Management)){
      for(YR in unique(runs.yr[!is.na(runs.yr$one.crash) & runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$one.crash==0,"year"])){
        for(VAR in full.var){
        
          val.cent <- mean(runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$Management==MNG & runs.yr$year %in% (YR-5):(YR-1) & runs.yr$one.crash<0 & !is.na(runs.yr$one.crash), VAR], na.rm=T)
          
          runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$Management==MNG & runs.yr$year %in% (YR-5):(YR+0), paste0(VAR,".extreme")] <- runs.yr[runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$Management==MNG & runs.yr$year %in% (YR-5):(YR+0), VAR] - val.cent
        } 
      } 
    } 
  } 
}
summary(runs.yr)


# --------------
# Running the calculation
# --------------
#-----------------------------------------------------#
# Looking at structural metrics on their own
# Struc.var ~ only crashes-1
#-----------------------------------------------------#
struc.var <- c("agb.extreme", "density.tree.extreme", "tree.dbh.mean.extreme", "tree.dbh.sd.extreme")
df.lag.struc <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.struc <- list()
  lag.list.struc[[paste(COL)]]$VAR <- COL
  lag.list.struc[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.struc[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.struc[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.struc[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.struc <- dplyr::bind_rows(lag.list.struc)
  temp.lag.struc$lag <- c(-5,-4,-3,-2,-1, 0)
  
  df.lag.struc <- rbind(df.lag.struc, temp.lag.struc)
}
summary(df.lag.struc)

plot.struc <- ggplot(data=df.lag.struc ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val<0.05 & df.lag.struc$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.struc[!is.na(df.lag.struc$p.val) & df.lag.struc$p.val>=0.05 & df.lag.struc$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))+
  ggtitle("Structure in the years before a crash")

png(paste0(path.figures, "Structure_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.struc
dev.off()

dat.struc <- runs.yr[!is.na(runs.yr$lag.crash), c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.struc) <- c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag")
summary(dat.struc)

#Making the format wide so that we can facet our different structural variables
dat.struc <- tidyr::gather(dat.struc, VAR, value, agb.extreme:tree.dbh.sd.extreme, factor_key=TRUE)

#Merging the frames and marking significance
dat.struc <- merge(dat.struc, df.lag.struc, all.x=T)
dat.struc$sig[!is.na(dat.struc$p.val)] <- ifelse(dat.struc$p.val[!is.na(dat.struc$p.val)]<0.05, "sig", "n.s.")
dat.struc$sig <- as.factor(dat.struc$sig)
summary(dat.struc)
dat.struc$VAR <- car::recode(dat.struc$VAR, "'agb.extreme'='AGB'; 'density.tree.extreme'='Tree Density'; 
                             'tree.dbh.mean.extreme'='Mean DBH'; 'tree.dbh.sd.extreme'='SD of DBH'")


plot.struc2 <- ggplot(data=dat.struc[!is.na(dat.struc$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "Structure_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.struc2
dev.off()

write.csv(dat.struc, file.path(path.google, "processed_data/Strucutural_before_crashes.csv"))
#-----------------------------------------------------#
# Looking at weather metrics on their own
# met.var ~ only crashes-1
#-----------------------------------------------------#
met.var <- c("tair.extreme", "VPD.extreme", "precip.total.extreme")
df.lag.met <- data.frame()
for(COL in met.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.met <- list()
  lag.list.met[[paste(COL)]]$VAR <- COL
  lag.list.met[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.met[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.met[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.met[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.met[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.met <- dplyr::bind_rows(lag.list.met)
  temp.lag.met$lag <- c(-5,-4,-3,-2,-1,0)

  df.lag.met <- rbind(df.lag.met, temp.lag.met)
}
summary(df.lag.met)

df.lag.met$lag <- factor(df.lag.met$lag, levels = c("-5", "-4", "-3", "-2", "-1", "0"))

plot.met <- ggplot(data=df.lag.met ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.met[!is.na(df.lag.met$p.val) & df.lag.met$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.met[!is.na(df.lag.met$p.val) & df.lag.met$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.met[!is.na(df.lag.met$p.val) & df.lag.met$p.val<0.05 & df.lag.met$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.met[!is.na(df.lag.met$p.val) & df.lag.met$p.val>=0.05 & df.lag.met$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "Weather_before_crash_hist.png"), width=12, height=8, units="in", res=220)
 plot.met
dev.off()

dat.met <- runs.yr[!is.na(runs.yr$lag.crash), c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.met) <- c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag")
summary(dat.met)

#Making the format wide so that we can facet our different weather variables
dat.met <- tidyr::gather(dat.met, VAR, value, tair.extreme:precip.total.extreme, factor_key=TRUE)

#Merging the frames and marking significance
dat.met <- merge(dat.met, df.lag.met, all.x=T)
dat.met$sig[!is.na(dat.met$p.val)] <- ifelse(dat.met$p.val[!is.na(dat.met$p.val)]<0.05, "sig", "n.s.")
dat.met$sig <- as.factor(dat.met$sig)
summary(dat.met)
dat.met$VAR <- car::recode(dat.met$VAR, "'tair.extreme'='Air Temp C'; 'VPD.extreme'='VPD (PA)'; 
                             'precip.total.extreme'='Total Precip (mm)'")

plot.met2 <- ggplot(data=dat.met[!is.na(dat.met$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "Weather_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.met2
dev.off()

write.csv(dat.met, file.path(path.google, "processed_data/Weather_before_crashes.csv"))
#-----------------------------------------------------#
# Looking at relative weather metrics interacting with management
# relmet.var ~ only crash-1
#-----------------------------------------------------#
relmet.var <- c("rel.precip.extreme", "diff.tair.extreme", "rel.VPD.extreme")
df.lag.rel <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
  output <- summary(mod.lag)
  
  lag.list.rel <- list()
  lag.list.rel[[paste(COL)]]$VAR <- COL
  lag.list.rel[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.rel[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.rel[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.rel[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.rel[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.rel <- dplyr::bind_rows(lag.list.rel)
  temp.lag.rel$lag <- c(-5,-4,-3,-2,-1,0)

  df.lag.rel <- rbind(df.lag.rel, temp.lag.rel)
}
summary(df.lag.rel)

plot.rel <- ggplot(data=df.lag.rel ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.rel[!is.na(df.lag.rel$p.val) & df.lag.rel$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.rel[!is.na(df.lag.rel$p.val) & df.lag.rel$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.rel[!is.na(df.lag.rel$p.val) & df.lag.rel$p.val<0.05 & df.lag.rel$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.rel[!is.na(df.lag.rel$p.val) & df.lag.rel$p.val>=0.05 & df.lag.rel$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "RelWeather_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.rel
dev.off()

dat.rel <- runs.yr[!is.na(runs.yr$lag.crash), c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.rel) <- c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag")
summary(dat.rel)

#Making the format wide so that we can facet our different relative weather variables
dat.rel <- tidyr::gather(dat.rel, VAR, value, rel.precip.extreme:rel.VPD.extreme, factor_key=TRUE)

#Merging the frames and marking significance
dat.rel <- merge(dat.rel, df.lag.rel, all.x=T)
dat.rel$sig[!is.na(dat.rel$p.val)] <- ifelse(dat.rel$p.val[!is.na(dat.rel$p.val)]<0.05, "sig", "n.s.")
dat.rel$sig <- as.factor(dat.rel$sig)
summary(dat.rel)

dat.rel$VAR <- car::recode(dat.rel$VAR, "'diff.tair.extreme'='Air Temp Difference C'; 'rel.VPD.extreme'='Relative VPD (PA)'; 
                             'rel.precip.extreme'='Relative Precip (mm)'")

plot.rel2 <- ggplot(data=dat.rel[!is.na(dat.rel$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "RelWeather_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.rel2
dev.off()
  
write.csv(dat.rel, file.path(path.google, "processed_data/Relweather_before_crashes.csv"))
#-----------------------------------------------------#
# Starting to look at interaction with Management
#-----------------------------------------------------#

#-----------------------------------------------------#
# Looking at structure metrics interacting with management
# struc.var ~ only crashes*Management-1
#-----------------------------------------------------#
df.lag.strucxmng <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$lag.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
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

df.lag.strucxmng$VAR <- car::recode(df.lag.strucxmng$VAR, "'agb.extreme'='AGB'; 'density.tree.extreme'='Tree Density'; 
                             'tree.dbh.mean.extreme'='Mean DBH'; 'tree.dbh.sd.extreme'='SD of DBH'")

plot.strucxmng <- ggplot(data=df.lag.strucxmng ) +
  facet_grid(VAR~MNG, scales = "free_y") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val<0.05 & df.lag.strucxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.strucxmng[!is.na(df.lag.strucxmng$p.val) & df.lag.strucxmng$p.val>=0.05 & df.lag.strucxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

#This figure has multiple issues resulting from the model output. I'm not sure how to deal with them but I want the figure to show the issue
png(paste0(path.figures, "Struc_X_MNG_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.strucxmng
dev.off()

#-----------------------------------------------------#
# Looking at weather metrics interacting with management
# met.var ~ any crash*Management-1
#-----------------------------------------------------#
met.var <- c("tair.extreme", "VPD.extreme", "precip.total.extreme")
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


#-----------------------------------------------------#
# Looking at relative weather metrics interacting with management
# relmet.var ~ any crash*Management-1
#-----------------------------------------------------#
relmet.var <- c("rel.precip.extreme", "diff.tair.extreme", "rel.VPD.extreme")
df.lag.relxmng <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*as.factor(Management)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.yr[!is.na(runs.yr$one.crash) & runs.yr$rcp == RCP,], na.action = na.omit)
  
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
