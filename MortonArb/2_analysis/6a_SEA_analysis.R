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
  if(YR >= 2025){
    prev.20 <- runs.yr[runs.yr$GCM == GCM & runs.yr$rcp == RCP & runs.yr$Management == MNG & runs.yr$year == YR-1 , "loss.event.20"]
    runs.yr[i, "nonseq.loss.event.20"] <- ifelse((runs.yr[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

runs.yr$crash <- ifelse(runs.yr$nonseq.loss.event.20==T, 1, 0)
#runs.yr <- runs.yr[runs.yr$year>=2025,]

#---------------------------------------------------#
# Here is why I start converting some of Christy's old script for our purposes
#---------------------------------------------------#
# Extreme crash years
dups.df <- data.frame()
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"] <- NA
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash.check"] <- "N"
    for(MNG in unique(runs.yr$Management)){
      runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG, "lag.crash"] <- NA
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      for(YR in crash.event){
        
        temp.df <- data.frame()
        #Flagging when only one of the management scenarios has a crash
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "lag.crash"] <- -5
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-5), "one.crash.check"] <- "Y"
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "lag.crash"] <- -5
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-5), "one.crash.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "lag.crash"] <- -4
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-4), "one.crash.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "lag.crash"] <- -4
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-4), "one.crash.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "lag.crash"] <- -3
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-3), "one.crash.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "lag.crash"] <- -3
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-3), "one.crash.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "lag.crash"] <- -2
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-2), "one.crash.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "lag.crash"] <- -2
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-2), "one.crash.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "lag.crash"] <- -1
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR-1), "one.crash.check"] <- "Y"
          
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "lag.crash"] <- -1
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR-1), "one.crash.check"] <- "Y"
        }
        
        if(!is.na(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "lag.crash"])){
          temp.store <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR),]
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "lag.crash"] <- 0
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "crash.year"] <- YR
          temp.store[temp.store$rcp == RCP & temp.store$GCM == GCM & temp.store$Management == MNG & temp.store$year == (YR), "one.crash.check"] <- "Y"
          temp.df <- rbind(temp.df, temp.store)
        } else{
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "lag.crash"] <- 0
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "crash.year"] <- YR
          runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$year == (YR), "one.crash.check"] <- "Y"
        }

        dups.df <- rbind(dups.df, temp.df)
      }
    }
  }
}
runs.yr <- runs.yr[runs.yr$year>=2025,]

runs.yr <- rbind(runs.yr, dups.df)

runs.fill <- data.frame()
for(RCP in unique(runs.yr$rcp)){
  for(GCM in unique(runs.yr$GCM)){
    runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM, "one.crash"] <- NA
    for(MNG in unique(runs.yr$Management)){
      crash.event <- unique(runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$Management == MNG & runs.yr$crash==1, "year"])
      crash.event <- sort(crash.event)
      for(YR in crash.event){
        temp.df <- runs.yr[runs.yr$rcp == RCP & runs.yr$GCM == GCM & runs.yr$year >= (YR-5) & runs.yr$year <= (YR),]
        temp.df$crash.year <- YR
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-5), "one.crash"] <- -5
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-4), "one.crash"] <- -4
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-3), "one.crash"] <- -3
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-2), "one.crash"] <- -2
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == (YR-1), "one.crash"] <- -1
        temp.df[temp.df$rcp == RCP & temp.df$GCM == GCM & temp.df$year == YR, "one.crash"] <- 0
        runs.fill <- rbind(runs.fill, temp.df)
        
      }
    }
  }
}

summary(runs.fill)

# Trying to re-center drought event & recovery; 
# I'm not sure exactly how to convert this for our data.
full.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd", "tair", "VPD", "precip.total", "rel.precip", "diff.tair", "rel.VPD")

# --------------
# Running the calculation
# --------------
#-----------------------------------------------------#
# Looking at structural metrics on their own
# Struc.var ~ only crashes-1
#-----------------------------------------------------#
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.struc <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
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

dat.struc <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.struc) <- c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag")
summary(dat.struc)

#Making the format wide so that we can facet our different structural variables
dat.struc <- tidyr::gather(dat.struc, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)

#Merging the frames and marking significance
dat.struc <- merge(dat.struc, df.lag.struc, all.x=T)
dat.struc$sig[!is.na(dat.struc$p.val)] <- ifelse(dat.struc$p.val[!is.na(dat.struc$p.val)]<0.05, "sig", "n.s.")
dat.struc$sig <- as.factor(dat.struc$sig)
summary(dat.struc)
dat.struc$VAR <- car::recode(dat.struc$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
                             'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")


plot.struc2 <- ggplot(data=dat.struc[!is.na(dat.struc$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("red2")) +
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
met.var <- c("tair", "VPD", "precip.total")
df.lag.met <- data.frame()
for(COL in met.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
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

dat.met <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.met) <- c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag")
summary(dat.met)

#Making the format wide so that we can facet our different weather variables
dat.met <- tidyr::gather(dat.met, VAR, value, tair:precip.total, factor_key=TRUE)

#Merging the frames and marking significance
dat.met <- merge(dat.met, df.lag.met, all.x=T)
dat.met$sig[!is.na(dat.met$p.val)] <- ifelse(dat.met$p.val[!is.na(dat.met$p.val)]<0.05, "sig", "n.s.")
dat.met$sig <- as.factor(dat.met$sig)
summary(dat.met)
dat.met$VAR <- car::recode(dat.met$VAR, "'tair'='Air Temp C'; 'VPD'='VPD (PA)'; 
                             'precip.total'='Total Precip (mm)'")

plot.met2 <- ggplot(data=dat.met[!is.na(dat.met$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("red2")) +
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
relmet.var <- c("rel.precip", "diff.tair", "rel.VPD")
df.lag.rel <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
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

dat.rel <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.rel) <- c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag")
summary(dat.rel)

#Making the format wide so that we can facet our different relative weather variables
dat.rel <- tidyr::gather(dat.rel, VAR, value, rel.precip:rel.VPD, factor_key=TRUE)

#Merging the frames and marking significance
dat.rel <- merge(dat.rel, df.lag.rel, all.x=T)
dat.rel$sig[!is.na(dat.rel$p.val)] <- ifelse(dat.rel$p.val[!is.na(dat.rel$p.val)]<0.05, "sig", "n.s.")
dat.rel$sig <- as.factor(dat.rel$sig)
summary(dat.rel)

dat.rel$VAR <- car::recode(dat.rel$VAR, "'diff.tair'='Air Temp Difference C'; 'rel.VPD'='Relative VPD (PA)'; 
                             'rel.precip'='Relative Precip (mm)'")

plot.rel2 <- ggplot(data=dat.rel[!is.na(dat.rel$lag),]) +
  facet_grid(VAR~., scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
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
#-----------------------------------------------------#

#-----------------------------------------------------#
# struc.var ~ as.factor(lag.crash)*Management
#-----------------------------------------------------#
df.ano.strucxmngsimple <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  df.ano.strucxmngsimple <- rbind(df.ano.strucxmngsimple, df.ano)
  
}

summary(df.ano.strucxmngsimple)

write.csv(df.ano.strucxmngsimple, file.path(path.google, "processed_data/strucxmngtest_simple_anova.csv"), row.names = F)


#-----------------------------------------------------#
# struc.var ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management
#-----------------------------------------------------#
df.lag.strucxmng <- data.frame()
df.ano.strucxmng <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  
  lag.list.strucxmng <- list()
  lag.list.strucxmng[[paste(COL)]]$VAR <- COL
  lag.list.strucxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.strucxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.strucxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.strucxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.strucxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.strucxmng <- dplyr::bind_rows(lag.list.strucxmng)
  temp.lag.strucxmng$lag <- c(rep(unique(-5:0), times = 4))
  temp.lag.strucxmng$Management <- c(rep(c("None", "Under", "Shelter", "Gap"), each = 6))
  
  df.lag.strucxmng <- rbind(df.lag.strucxmng, temp.lag.strucxmng)
  df.ano.strucxmng <- rbind(df.ano.strucxmng, df.ano)
  
}
summary(df.lag.strucxmng)
summary(df.ano.strucxmng)


plot.strucxmng <- ggplot(data=df.lag.strucxmng ) +
  facet_grid(VAR~Management, scales = "free_y") +
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

dat.strucxmng <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.strucxmng) <- c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag")
summary(dat.strucxmng)

#Making the format wide so that we can facet our different relative weather variables
dat.strucxmng <- tidyr::gather(dat.strucxmng, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)

#Merging the frames and marking significance
dat.strucxmng <- merge(dat.strucxmng, df.lag.strucxmng, all.x=T,)
dat.strucxmng$sig[!is.na(dat.strucxmng$p.val)] <- ifelse(dat.strucxmng$p.val[!is.na(dat.strucxmng$p.val)]<0.05, "sig", "n.s.")
dat.strucxmng$sig <- as.factor(dat.strucxmng$sig)
summary(dat.strucxmng)

dat.strucxmng$VAR <- car::recode(dat.strucxmng$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
                             'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")

plot.rel2 <- ggplot(data=dat.strucxmng[!is.na(dat.strucxmng$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "StrucxMNG_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.rel2
dev.off()

write.csv(dat.strucxmng, file.path(path.google, "processed_data/StrucxMNG_before_crashes.csv"))

write.csv(df.ano.strucxmng, file.path(path.google, "processed_data/StrucxMNG_anova.csv"), row.names = F)


#-----------------------------------------------------#
# struc.var ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)
#-----------------------------------------------------#
df.lag.strucxmngtest <- data.frame()
df.ano.strucxmngtest <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  
  lag.list.strucxmngtest <- list()
  lag.list.strucxmngtest[[paste(COL)]]$VAR <- COL
  lag.list.strucxmngtest[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.strucxmngtest[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.strucxmngtest[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.strucxmngtest[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.strucxmngtest[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.strucxmngtest <- dplyr::bind_rows(lag.list.strucxmngtest)
  temp.lag.strucxmngtest$lag <- c(NA,NA,NA,NA,rep(unique(-4:0), times = 4))
  temp.lag.strucxmngtest$Management <- c("None", "Under", "Shelter", "Gap" ,rep(c("None", "Under", "Shelter", "Gap"), each = 5))
  
  df.lag.strucxmngtest <- rbind(df.lag.strucxmngtest, temp.lag.strucxmngtest)
  df.ano.strucxmngtest <- rbind(df.ano.strucxmngtest, df.ano)
  
}
summary(df.lag.strucxmngtest)
summary(df.ano.strucxmngtest)


plot.strucxmngtest <- ggplot(data=(df.lag.strucxmngtest)) +
  facet_grid(VAR~Management, scales = "free_y") +
  geom_bar(data=df.lag.strucxmngtest[!is.na(df.lag.strucxmngtest$p.val) & df.lag.strucxmngtest$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.strucxmngtest[!is.na(df.lag.strucxmngtest$p.val) & df.lag.strucxmngtest$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.strucxmngtest[!is.na(df.lag.strucxmngtest$p.val) & df.lag.strucxmngtest$p.val<0.05 & df.lag.strucxmngtest$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.strucxmngtest[!is.na(df.lag.strucxmngtest$p.val) & df.lag.strucxmngtest$p.val>=0.05 & df.lag.strucxmngtest$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

#This figure has multiple issues resulting from the model output. I'm not sure how to deal with them but I want the figure to show the issue
png(paste0(path.figures, "Struc_X_MNG_test_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.strucxmngtest
dev.off()

dat.strucxmngtest <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.strucxmngtest) <- c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag")
summary(dat.strucxmngtest)

#Making the format wide so that we can facet our different relative weather variables
dat.strucxmngtest <- tidyr::gather(dat.strucxmngtest, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)

#Merging the frames and marking significance
dat.strucxmngtest <- merge(dat.strucxmngtest, df.lag.strucxmngtest, all.x=T,)
dat.strucxmngtest$sig[!is.na(dat.strucxmngtest$p.val)] <- ifelse(dat.strucxmngtest$p.val[!is.na(dat.strucxmngtest$p.val)]<0.05, "sig", "n.s.")
dat.strucxmngtest$sig <- as.factor(dat.strucxmngtest$sig)
summary(dat.strucxmngtest)

dat.strucxmngtest$VAR <- car::recode(dat.strucxmngtest$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
                             'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")

plot.rel2 <- ggplot(data=dat.strucxmngtest[!is.na(dat.strucxmngtest$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("gray50","red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "strucxmngtest_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.rel2
dev.off()

write.csv(dat.strucxmngtest, file.path(path.google, "processed_data/strucxmngtest_before_crashes.csv"))

write.csv(df.ano.strucxmngtest, file.path(path.google, "processed_data/strucxmngtest_anova.csv"), row.names = F)


#-----------------------------------------------------#
# struc.var ~ as.factor(lag.crash)*Management-1
#-----------------------------------------------------#
df.lag.strucxmngtest2 <- data.frame()
df.ano.strucxmngtest2 <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  
  lag.list.strucxmngtest2 <- list()
  lag.list.strucxmngtest2[[paste(COL)]]$VAR <- COL
  lag.list.strucxmngtest2[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.strucxmngtest2[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.strucxmngtest2[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.strucxmngtest2[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.strucxmngtest2[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.strucxmngtest2 <- dplyr::bind_rows(lag.list.strucxmngtest2)
  temp.lag.strucxmngtest2$lag <- c(-5,-4,-3,-2,-1,0,NA, NA, NA, rep(unique(-4:0), times = 3))
  temp.lag.strucxmngtest2$Management <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap" ,rep(c("Under", "Shelter", "Gap"), each = 5))
  
  df.lag.strucxmngtest2 <- rbind(df.lag.strucxmngtest2, temp.lag.strucxmngtest2)
  df.ano.strucxmngtest2 <- rbind(df.ano.strucxmngtest2, df.ano)
  
}
summary(df.lag.strucxmngtest2)
summary(df.ano.strucxmngtest2)


plot.strucxmngtest2 <- ggplot(data=(df.lag.strucxmngtest2)) +
  facet_grid(VAR~Management, scales = "free_y") +
  geom_bar(data=df.lag.strucxmngtest2[!is.na(df.lag.strucxmngtest2$p.val) & df.lag.strucxmngtest2$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.strucxmngtest2[!is.na(df.lag.strucxmngtest2$p.val) & df.lag.strucxmngtest2$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.strucxmngtest2[!is.na(df.lag.strucxmngtest2$p.val) & df.lag.strucxmngtest2$p.val<0.05 & df.lag.strucxmngtest2$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.strucxmngtest2[!is.na(df.lag.strucxmngtest2$p.val) & df.lag.strucxmngtest2$p.val>=0.05 & df.lag.strucxmngtest2$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

#This figure has multiple issues resulting from the model output. I'm not sure how to deal with them but I want the figure to show the issue
png(paste0(path.figures, "Struc_X_MNG_test2_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.strucxmngtest2
dev.off()

dat.strucxmngtest2 <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.strucxmngtest2) <- c("year", "Management", "GCM", "rcp", struc.var, "one.crash", "lag")
summary(dat.strucxmngtest2)

#Making the format wide so that we can facet our different relative weather variables
dat.strucxmngtest2 <- tidyr::gather(dat.strucxmngtest2, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)

#Merging the frames and marking significance
dat.strucxmngtest2 <- merge(dat.strucxmngtest2, df.lag.strucxmngtest2, all.x=T,)
dat.strucxmngtest2$sig[!is.na(dat.strucxmngtest2$p.val)] <- ifelse(dat.strucxmngtest2$p.val[!is.na(dat.strucxmngtest2$p.val)]<0.05, "sig", "n.s.")
dat.strucxmngtest2$sig <- as.factor(dat.strucxmngtest2$sig)
summary(dat.strucxmngtest2)

dat.strucxmngtest2$VAR <- car::recode(dat.strucxmngtest2$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
                             'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")

plot.rel2 <- ggplot(data=dat.strucxmngtest2[!is.na(dat.strucxmngtest2$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("gray50","red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "strucxmngtest2_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.rel2
dev.off()

write.csv(dat.strucxmngtest2, file.path(path.google, "processed_data/strucxmngtest2_before_crashes.csv"))

write.csv(df.ano.strucxmngtest2, file.path(path.google, "processed_data/strucxmngtest2_anova.csv"), row.names = F)



#-----------------------------------------------------#
# Looking at weather metrics interacting with management
#-----------------------------------------------------#

#-----------------------------------------------------#
# met.var ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management
#-----------------------------------------------------#
df.lag.metxmng <- data.frame()
df.ano.metxmng <- data.frame()
for(COL in met.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  
  lag.list.metxmng <- list()
  lag.list.metxmng[[paste(COL)]]$VAR <- COL
  lag.list.metxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.metxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.metxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.metxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.metxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.metxmng <- dplyr::bind_rows(lag.list.metxmng)
  temp.lag.metxmng$lag <- c(rep(unique(-5:0), times = 4))
  temp.lag.metxmng$Management <- c(rep(c("None", "Under", "Shelter", "Gap"), each = 6))
  
  df.lag.metxmng <- rbind(df.lag.metxmng, temp.lag.metxmng)
  df.ano.metxmng <- rbind(df.ano.metxmng, df.ano)
  
}
summary(df.lag.metxmng)
summary(df.ano.metxmng)

plot.metxmng <- ggplot(data=df.lag.metxmng ) +
  facet_grid(VAR~Management, scales = "free_y") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val<0.05 & df.lag.metxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.metxmng[!is.na(df.lag.metxmng$p.val) & df.lag.metxmng$p.val>=0.05 & df.lag.metxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

#This figure has multiple issues resulting from the model output. I'm not sure how to deal with them but I want the figure to show the issue
png(paste0(path.figures, "Met_X_MNG_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.metxmng
dev.off()

dat.metxmng <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.metxmng) <- c("year", "Management", "GCM", "rcp", met.var, "one.crash", "lag")
summary(dat.metxmng)

#Making the format wide so that we can facet our different relative weather variables
dat.metxmng <- tidyr::gather(dat.metxmng, VAR, value, tair:precip.total, factor_key=TRUE)

#Merging the frames and marking significance
dat.metxmng <- merge(dat.metxmng, df.lag.metxmng, all.x=T,)
dat.metxmng$sig[!is.na(dat.metxmng$p.val)] <- ifelse(dat.metxmng$p.val[!is.na(dat.metxmng$p.val)]<0.05, "sig", "n.s.")
dat.metxmng$sig <- as.factor(dat.metxmng$sig)
summary(dat.metxmng)

dat.metxmng$VAR <- car::recode(dat.metxmng$VAR, "'tair'='Air Temp C'; 'VPD'='VPD (PA)'; 
                             'precip.total'='Total Precip (mm)'")

plot.met2 <- ggplot(data=dat.metxmng[!is.na(dat.metxmng$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "metxmng_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.met2
dev.off()

write.csv(dat.metxmng, file.path(path.google, "processed_data/metxmng_before_crashes.csv"))

write.csv(df.ano.metxmng, file.path(path.google, "processed_data/metxmng_anova.csv"), row.names = F)


#-----------------------------------------------------#
# relmet.var ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management
#-----------------------------------------------------#
df.lag.relxmng <- data.frame()
df.ano.relxmng <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ as.factor(lag.crash)*Management-1-as.factor(lag.crash)-Management, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$lag.crash),], na.action = na.omit)
  
  output <- summary(mod.lag)
  df.ano <- anova(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  
  lag.list.relxmng <- list()
  lag.list.relxmng[[paste(COL)]]$VAR <- COL
  lag.list.relxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.relxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.relxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.relxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.relxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.relxmng <- dplyr::bind_rows(lag.list.relxmng)
  temp.lag.relxmng$lag <- c(rep(unique(-5:0), times = 4))
  temp.lag.relxmng$Management <- c(rep(c("None", "Under", "Shelter", "Gap"), each = 6))
  
  df.lag.relxmng <- rbind(df.lag.relxmng, temp.lag.relxmng)
  df.ano.relxmng <- rbind(df.ano.relxmng, df.ano)
  
}
summary(df.lag.relxmng)
summary(df.ano.relxmng)

plot.relxmng <- ggplot(data=df.lag.relxmng ) +
  facet_grid(VAR~Management, scales = "free_y") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val>=0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val<0.05,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val<0.05 & df.lag.relxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=df.lag.relxmng[!is.na(df.lag.relxmng$p.val) & df.lag.relxmng$p.val>=0.05 & df.lag.relxmng$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

#This figure has multiple issues resulting from the model output. I'm not sure how to deal with them but I want the figure to show the issue
png(paste0(path.figures, "Met_X_MNG_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.relxmng
dev.off()

dat.relxmng <- runs.fill[!is.na(runs.fill$lag.crash), c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag.crash")]
#Just to make "lag" a common name for merging purposes
colnames(dat.relxmng) <- c("year", "Management", "GCM", "rcp", relmet.var, "one.crash", "lag")
summary(dat.relxmng)

#Making the format wide so that we can facet our different relative weather variables
dat.relxmng <- tidyr::gather(dat.relxmng, VAR, value, rel.precip:rel.VPD, factor_key=TRUE)

#Merging the frames and marking significance
dat.relxmng <- merge(dat.relxmng, df.lag.relxmng, all.x=T,)
dat.relxmng$sig[!is.na(dat.relxmng$p.val)] <- ifelse(dat.relxmng$p.val[!is.na(dat.relxmng$p.val)]<0.05, "sig", "n.s.")
dat.relxmng$sig <- as.factor(dat.relxmng$sig)
summary(dat.relxmng)

dat.relxmng$VAR <- car::recode(dat.relxmng$VAR, "'diff.tair'='Air Temp Difference C'; 'rel.VPD'='Relative VPD (PA)'; 
                             'rel.precip'='Relative Precip (mm)'")

plot.met2 <- ggplot(data=dat.relxmng[!is.na(dat.relxmng$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill=sig)) +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "relxmng_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.met2
dev.off()

write.csv(dat.relxmng, file.path(path.google, "processed_data/relxmng_before_crashes.csv"))

write.csv(df.ano.relxmng, file.path(path.google, "processed_data/relxmng_anova.csv"), row.names = F)



