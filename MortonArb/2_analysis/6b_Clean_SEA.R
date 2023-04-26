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
runs.yr$loss.event.20 <- ifelse(runs.yr$agb.rel.diff<=-0.2, 1, 0)
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
# Creating summary tables for structural values
#-----------------------------------------------------------#
#Management table
checks <- c("2050", "2099")
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
mng.list <- list()
for(MNG in unique(runs.yr$Management)){
  for(YR in checks){
    for(VAR in struc.var){
      value.m <- round(mean(runs.yr[runs.yr$Management == MNG & runs.yr$year == YR, VAR]), 4)
      value.sd <- round(sd(runs.yr[runs.yr$Management == MNG & runs.yr$year == YR, VAR]), 4)
      value.char <- paste0(value.m, " (", value.sd, ")")
      mng.list[[paste(MNG, YR, VAR, sep="-")]]$MNG <- as.factor(MNG)
      mng.list[[paste(MNG, YR, VAR, sep="-")]]$YEAR <- as.factor(YR)
      mng.list[[paste(MNG, YR, VAR, sep="-")]]$VAR <- as.factor(VAR)
      mng.list[[paste(MNG, YR, VAR, sep="-")]]$value <- value.char
    }
  }
}
mng.df <- dplyr::bind_rows(mng.list)
mng.wide <- tidyr::spread(mng.df, VAR, value)

colnames(mng.wide) <- c("Management", "Year", "AGB Mean(SD)", "Tree Density Mean(SD)", "Mean DBH Mean(SD)", "SD of DBH Mean(SD)")
write.csv(mng.wide, "../data/MNG_struc_values.csv")

#Emissions scenario table
checks <- c("2050", "2099")
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
rcp.list <- list()
for(RCP in unique(runs.yr$rcp)){
  for(YR in checks){
    for(VAR in struc.var){
    value.m <- round(mean(runs.yr[runs.yr$rcp == RCP & runs.yr$year == YR, VAR]), 4)
    value.sd <- round(sd(runs.yr[runs.yr$rcp == RCP & runs.yr$year == YR, VAR]), 4)
    value.char <- paste0(value.m, " (", value.sd, ")")
      rcp.list[[paste(RCP, YR, VAR, sep="-")]]$RCP <- as.factor(RCP)
      rcp.list[[paste(RCP, YR, VAR, sep="-")]]$YEAR <- as.factor(YR)
      rcp.list[[paste(RCP, YR, VAR, sep="-")]]$VAR <- as.factor(VAR)
      rcp.list[[paste(RCP, YR, VAR, sep="-")]]$value <- value.char
    }
  }
}
rcp.df <- dplyr::bind_rows(rcp.list)
rcp.wide <- tidyr::spread(rcp.df, VAR, value)

colnames(rcp.wide) <- c("Emmisions Scenario", "Year", "AGB Mean(SD)", "Tree Density Mean(SD)", "Mean DBH Mean(SD)", "SD of DBH Mean(SD)")
write.csv(rcp.wide, "../data/RCP_struc_values.csv")

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
relmet.var <- c("rel.precip", "diff.tair", "rel.VPD")
df.lag.relmetxind <- data.frame()
df.ano.relmetxind <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "crash")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)
  anova(mod.lag)
  
  df.ano <- anova(mod.lag)
  output <- summary(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  rownames(df.ano) <- NULL
  
  
  df.ano.relmetxind <- rbind(df.ano.relmetxind, df.ano)
  
}

summary(df.ano.relmetxind)
summary(df.lag.relmetxind)

df.ano.relmetxind <- df.ano.relmetxind[,c(6,5,1,2,3,4)]
df.ano.relmetxind$comp <- gsub("ind.crash.lag", "Time", df.ano.relmetxind$comp)
df.ano.relmetxind$comp <- gsub("relevel", "", df.ano.relmetxind$comp)
#IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
df.ano.relmetxind$`p-value` <- round(df.ano.relmetxind$`p-value`, 5)
View(df.ano.relmetxind)

#-----------------------------------------------------#
# Looking at relative weather before a crash to make a figure
# ind.crash.lag = time lag for individual management which crashed
# We include the crash year for this evaluation because we are working with temperature
# This is just to create a figure so we can investigate the directionality of our variables
#-----------------------------------------------------#
relmet.var <- c("rel.precip", "diff.tair", "rel.VPD")
df.lag.rel <- data.frame()
for(COL in relmet.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "crash")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag),], na.action = na.omit)

  output <- summary(mod.lag)
  lag.list.rel <- list()
  lag.list.rel[[paste(COL)]]$VAR <- COL
  lag.list.rel[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.rel[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.rel[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.rel[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.rel[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.rel <- dplyr::bind_rows(lag.list.rel)
  temp.lag.rel$lag <- c("crash", -1, -2, -3, -4, -5)
  
  df.lag.rel <- rbind(df.lag.rel, temp.lag.rel)
  
}
output <- summary(mod.lag)

summary(df.lag.rel)

plot.rel <- ggplot(data=df.lag.rel ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.rel, aes(x=as.factor(lag), y=estimate), stat="identity") +
  geom_errorbar(data=df.lag.rel, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))+
  scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1, "crash")))+
  ggtitle("Relative weather before crashes")

png(paste0(path.figures, "RelWeather_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.rel
dev.off()

#-----------------------------------------------------#
# Looking at structure before a crash
# ind.crash.lag = time lag for individual management which crashed
# We exclude the year of crash
#-----------------------------------------------------#
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.strucxind <- data.frame()
df.ano.strucxind <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-1")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag) & runs.fill$ind.crash.lag!="crash",], na.action = na.omit)
  anova(mod.lag)
  
  df.ano <- anova(mod.lag)
  output <- summary(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  rownames(df.ano) <- NULL
  
  df.ano.strucxind <- rbind(df.ano.strucxind, df.ano)
  
}

summary(df.ano.strucxind)
df.ano.strucxind <- df.ano.strucxind[,c(6,5,1,2,3,4)]
df.ano.strucxind$comp <- gsub("ind.crash.lag", "Time", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub("-1", "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub("None", "", df.ano.strucxind$comp)
df.ano.strucxind$comp <- gsub("relevel", "", df.ano.strucxind$comp)
#IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
df.ano.strucxind$`p-value` <- round(df.ano.strucxind$`p-value`, 5)

#df.ano.strucx <- df.ano.strucxind[df.ano.strucxind$VAR== "tree.dbh.sd",]

#-----------------------------------------------------#
# Looking at Structure before a crash to make a figure
# ind.crash.lag = time lag for individual management which crashed
# We include the crash year for this evaluation because we are working with temperature
# This is just to create a figure so we can investigate the directionality of our variables
#-----------------------------------------------------#
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")
df.lag.struc <- data.frame()
for(COL in struc.var){
  
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(ind.crash.lag), "-1")-1, list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$ind.crash.lag) & runs.fill$ind.crash.lag != "crash",], na.action = na.omit)
  
  output <- summary(mod.lag)
  lag.list.struc <- list()
  lag.list.struc[[paste(COL)]]$VAR <- COL
  lag.list.struc[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.struc[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.struc[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.struc[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.struc[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.struc <- dplyr::bind_rows(lag.list.struc)
  temp.lag.struc$lag <- c(-1, -2, -3, -4, -5)
  
  df.lag.struc <- rbind(df.lag.struc, temp.lag.struc)
  
}
output <- summary(mod.lag)

summary(df.lag.struc)

plot.struc <- ggplot(data=df.lag.struc ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.struc, aes(x=as.factor(lag), y=estimate), stat="identity") +
  geom_errorbar(data=df.lag.struc, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))+
  scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
  ggtitle("Structure before crashes")

png(paste0(path.figures, "Structure_before_crash_hist.png"), width=12, height=8, units="in", res=220)
  plot.struc
dev.off()

#-----------------------------------------------------#
# Looking for structural differences between the conditions that crashed and those that didn't by Management
# Looking if those differences vary by Management
# group.crash.lag = time lag for GROUP of conditions with at least ONE RUN crashing
# group.crash.lag.check --> Y/N indicating which set actually crashed
#-----------------------------------------------------#
df.lag.strucxcrashxmng <- data.frame()
df.ano.strucxcrashxmng <- data.frame()
for(COL in struc.var){
  
  # Checkign to see if there's anything if we move MGMT to FIXED
  mod.lag <- nlme::lme(eval(substitute(j ~ relevel(as.factor(group.crash.lag), "-1")*relevel(as.factor(group.crash.lag.check), "N")*relevel(Management, "None"), list(j = as.name(COL)))), random=list(rcp = ~1, GCM =~1), data = runs.fill[!is.na(runs.fill$group.crash.lag) & runs.fill$group.crash.lag!="crash",], na.action = na.omit)
  anova(mod.lag)
  
  df.ano <- anova(mod.lag)
  output <- summary(mod.lag)
  df.ano$comp <- rownames(df.ano)
  df.ano$VAR <- COL
  rownames(df.ano) <- NULL
  
  lag.list.strucxcrashxmng <- list()
  lag.list.strucxcrashxmng[[paste(COL)]]$VAR <- COL
  lag.list.strucxcrashxmng[[paste(COL)]]$Comp <- rownames(output$tTable)
  lag.list.strucxcrashxmng[[paste(COL)]]$estimate <- output$tTable[,"Value"]
  lag.list.strucxcrashxmng[[paste(COL)]]$std.err <- output$tTable[,"Std.Error"]
  lag.list.strucxcrashxmng[[paste(COL)]]$t.stat <- output$tTable[,"t-value"]
  lag.list.strucxcrashxmng[[paste(COL)]]$p.val <- output$tTable[,"p-value"]
  temp.lag.strucxcrashxmng <- dplyr::bind_rows(lag.list.strucxcrashxmng)
  temp.lag.strucxcrashxmng$lag <- c(NA, -2,-3,-4,-5, NA, NA, NA, NA, rep(unique(-2:-5), times = 4), NA, NA, NA, rep(unique(-2:-5), times = 3))
  temp.lag.strucxcrashxmng$crash <- c(NA, NA, NA, NA, NA, "Y", NA, NA, NA, "Y", "Y", "Y", "Y", rep(c(NA), each = 12), rep(c("Y"), each = 15))
  temp.lag.strucxcrashxmng$Management <- c(NA, NA, NA, NA, NA, NA, "Under", "Shelter", "Gap", NA, NA, NA, NA, rep(c("Under", "Shelter", "Gap"), each = 4), "Under", "Shelter", "Gap", rep(c("Under", "Shelter", "Gap"), each = 4))
  
  df.lag.strucxcrashxmng <- rbind(df.lag.strucxcrashxmng, temp.lag.strucxcrashxmng)
  df.ano.strucxcrashxmng <- rbind(df.ano.strucxcrashxmng, df.ano)
  
}
summary(df.ano.strucxcrashxmng)
df.ano.strucxcrashxmng <- df.ano.strucxcrashxmng[,c(6,5,1,2,3,4)]

df.ano.strucxcrashxmng$comp <- gsub("(group.crash.lag)", "Time", df.ano.strucxcrashxmng$comp)
df.ano.strucxcrashxmng$comp <- gsub("Time.check", "CrashY/N", df.ano.strucxcrashxmng$comp)
df.ano.strucxcrashxmng$comp <- gsub("-1", "", df.ano.strucxcrashxmng$comp)
df.ano.strucxcrashxmng$comp <- gsub("None", "", df.ano.strucxcrashxmng$comp)
df.ano.strucxcrashxmng$comp <- gsub("relevel", "", df.ano.strucxcrashxmng$comp)

#IMPORTANT!!!!! Rounding for easy reading. This should not be how the values are reported in the end
df.ano.strucxcrashxmng$`p-value` <- round(df.ano.strucxcrashxmng$`p-value`, 5)

#df.ano.strucx <- df.ano.strucxcrashxmng[df.ano.strucxcrashxmng$VAR== "density.tree",]

write.csv(df.ano.strucxcrashxmng, file.path(path.google, "processed_data/strucxcrashxmng_anova.csv"), row.names = F)

plot.strucxmng <- ggplot(data=df.lag.strucxcrashxmng ) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.struc, aes(x=as.factor(lag), y=estimate), stat="identity") +
  geom_errorbar(data=df.lag.struc, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))+
  scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
  ggtitle("Structure before crashes")

plot.mngxcrash <- ggplot(data=runs.fill) +
  facet_wrap(~VAR, scales = "free_y") +
  geom_bar(data=df.lag.struc, aes(x=as.factor(lag), y=estimate), stat="identity") +
  geom_errorbar(data=df.lag.struc, aes(as.factor(lag), ymin = estimate - std.err, ymax = estimate + std.err))+
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))+
  scale_x_discrete(limits = factor(c(-5, -4, -3, -2, -1)))+
  ggtitle("Structure before crashes")

dat.strucxmng <- runs.fill[!is.na(runs.fill$group.crash.lag), c("year", "Management", "GCM", "rcp", struc.var, "group.crash.lag", "ind.crash.lag", "group.crash.lag.check")]
#Just to make "lag" a common name for merging purposes
colnames(dat.strucxmng) <- c("year", "Management", "GCM", "rcp", struc.var, "lag", "ind.crash.lag", "group.crash.lag.check")
summary(dat.strucxmng)

#Making the format wide so that we can facet our different relative weather variables
dat.strucxmng <- tidyr::gather(dat.strucxmng, VAR, value, agb:tree.dbh.sd, factor_key=TRUE)

#Merging the frames and marking significance
dat.strucxmng <- merge(dat.strucxmng, df.lag.strucxcrashxmng, all.x=T,)
dat.strucxmng$sig[!is.na(dat.strucxmng$p.val)] <- ifelse(dat.strucxmng$p.val[!is.na(dat.strucxmng$p.val)]<0.05, "sig", "n.s.")
dat.strucxmng$sig <- as.factor(dat.strucxmng$sig)
summary(dat.strucxmng)

dat.strucxmng$VAR <- car::recode(dat.strucxmng$VAR, "'agb'='AGB'; 'density.tree'='Tree Density'; 
                             'tree.dbh.mean'='Mean DBH'; 'tree.dbh.sd'='SD of DBH'")

plot.strucxmng <- ggplot(data=dat.strucxmng[!is.na(dat.strucxmng$lag),]) +
  facet_grid(VAR~Management, scales="free_y") +
  geom_boxplot(aes(x=as.factor(lag), y=value, fill =group.crash.lag.check, color = group.crash.lag.check)) +
  scale_x_discrete(name="Loss event Lag") +
  scale_y_continuous(name="Difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))

png(paste0(path.figures, "StrucxMNG_before_crash_boxplot.png"), width=12, height=8, units="in", res=220)
  plot.strucxmng
dev.off()