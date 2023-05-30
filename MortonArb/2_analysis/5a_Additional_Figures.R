#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection, does linear regression, and creates figures
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(ggplot2)
library(nlme)
library(multcomp)
#------------------------------------------------------------------------#
# FIGURES SECTION
#------------------------------------------------------------------------#
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

runs.yr$density.tree.convert <- runs.yr$density.tree * 10000

#-----------------------------------------------------------#
# Creating summary tables for structural values
#-----------------------------------------------------------#
#Management table
checks <- c("2050", "2099")
struc.var <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")
mng.list <- list()
for(MNG in unique(runs.yr$Management)){
  for(YR in checks){
    for(VAR in struc.var){
      value.m <- round(mean(runs.yr[runs.yr$Management == MNG & runs.yr$year == YR, VAR]), 2)
      value.sd <- round(sd(runs.yr[runs.yr$Management == MNG & runs.yr$year == YR, VAR]), 2)
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
struc.var <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")
rcp.list <- list()
for(RCP in unique(runs.yr$rcp)){
  for(YR in checks){
    for(VAR in struc.var){
      value.m <- round(mean(runs.yr[runs.yr$rcp == RCP & runs.yr$year == YR, VAR]), 2)
      value.sd <- round(sd(runs.yr[runs.yr$rcp == RCP & runs.yr$year == YR, VAR]), 2)
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


crash.summary <- aggregate(crash~ GCM + rcp + RCP.name + Management, data=runs.yr[runs.yr$year>=2025 ,], FUN=sum)
names(crash.summary)[names(crash.summary)=="crash"] <- "crash.end"
crash.summary$crash.mid <- aggregate(crash~ GCM + rcp + RCP.name + Management, data=runs.yr[runs.yr$year>=2025 & runs.yr$year<=2050,], FUN=sum)$crash
summary(crash.summary)


library(MASS)
library(lme4)
#Table S4
crash.mid <- glm.nb(crash.mid~Management*rcp, data = crash.summary)
summary(crash.mid)
mid.output <- anova(crash.mid)
dat.mid <- data.frame(rownames(mid.output))
colnames(dat.mid) <- "Comp"
dat.mid$Time <- "Mid-century"
dat.mid$pvalue <- mid.output$`Pr(>Chi)` 



#Table S5
crash.end <- glm.nb(crash.end~Management*rcp, data = crash.summary)
summary(crash.end)
end.output <- anova(crash.end)
dat.end <- data.frame(rownames(end.output))
colnames(dat.end) <- "Comp"
dat.end$Time <- "End of century"
dat.end$pvalue <- end.output$`Pr(>Chi)` 


crash.comp <- rbind(dat.mid, dat.end)

crash.comp$pvalue <- ifelse(crash.comp$pvalue<=.05,"*"," ")

crash.sig <- reshape2::dcast(crash.comp, Time~Comp, value.var = "pvalue")

crash.sig <- crash.sig[,c("Time", "Management", "rcp", "Management:rcp")]

write.csv(crash.sig, "../data/crash_fvalues.csv")


crash.first <- aggregate(crash~ GCM + rcp + RCP.name + Management + year, data=runs.yr[runs.yr$year>=2025 ,], FUN=sum)
names(crash.first)[names(crash.first)=="crash"] <- "crash.end"
crash.first <- crash.first[crash.first$crash.end == 1,]

dat.first <- data.frame()
for(MOD in unique(crash.first$GCM)){
  for(RCP in unique(crash.first$rcp)){
    for(MNG in unique(crash.first$Management)){
      temp <- crash.first[crash.first$GCM == MOD & crash.first$rcp == RCP & crash.first$Management == MNG,] 
      temp.1 <- temp[1,]
      dat.first <- rbind(dat.first,temp.1)
    }
  }
}
dat.first <- dat.first[!is.na(dat.first$crash.end),]

mng.first.agg <- aggregate(cbind(year)~Management, data = dat.first, FUN = mean, na.action = NULL)
mng.first.agg[, "sd"] <- aggregate(cbind(year)~Management, data = dat.first, FUN = sd, na.action = NULL)[, "year"]


#We see that at the mid-century point, Shelter () management was the most resistant to AGB loss events compared to other prescriptions...
mng.crash.agg <- aggregate(cbind(crash.mid)~Management, data = crash.summary, FUN = mean, na.action = NULL)
mng.crash.agg[, "sd"] <- aggregate(cbind(crash.mid)~Management, data = crash.summary, FUN = sd, na.action = NULL)[, "crash.mid"]

#RCP 8.5 experienced more loss events than RCP 4.5 demonstrating the increasing importance of climate and...
rcp.crash.agg <- aggregate(cbind(crash.end)~rcp, data = crash.summary, FUN = mean, na.action = NULL)
rcp.crash.agg[, "sd"] <- aggregate(cbind(crash.end)~rcp, data = crash.summary, FUN = sd, na.action = NULL)[, "crash.end"]

#But by the end of the century air temperature and VPD were both higher in rcp 8.5 than in rcp 4.5 
rcp.temp.agg <- aggregate(cbind(tair)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = mean, na.action = NULL)
rcp.temp.agg[, "sd"] <- aggregate(cbind(tair)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = sd, na.action = NULL)[, "tair"]
rcp.VPD.agg <- aggregate(cbind(VPD)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = mean, na.action = NULL)
rcp.VPD.agg[, "sd"] <- aggregate(cbind(VPD)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = sd, na.action = NULL)[, "VPD"]

# Distinct structures were caused by management (p<0.05, df=) for all four different styles immediately post harvest (S4).
mng.struc.agg.postharv <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2025,], FUN = mean, na.action = NULL)
mng.struc.agg.postharv[, c("agb.sd", "density.tree.sd", "dbh.mean.sd", "dbh.sd.sd")] <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2025,], FUN = sd, na.action = NULL)[, c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")]

# Mid-century AGB was similar across all prescriptions while the structural changes of tree density, mean tree dbh, and tree dbh sd persisted.
mng.struc.agg.mid <- aggregate(cbind(density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2050,], FUN = mean, na.action = NULL)
mng.struc.agg.mid[, c("density.tree.sd", "dbh.mean.sd", "dbh.sd.sd")] <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2050,], FUN = sd, na.action = NULL)[, c("density.tree", "tree.dbh.mean", "tree.dbh.sd")]

# -----------------------------------------------------------
# Looking at the effect of harvest on structural variables
# -----------------------------------------------------------
vars.plot <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")

dat.harvest.pre <- stack(runs.yr[runs.yr$year==2019, vars.plot])
dat.harvest.pre$time <- "pre-harvest"
dat.harvest.pre[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2019, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.post <- stack(runs.yr[runs.yr$year==2025, vars.plot])
dat.harvest.post$time <- "post-harvest"
dat.harvest.post[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2025, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.mid <- stack(runs.yr[runs.yr$year==2050, vars.plot])
dat.harvest.mid$time <- "mid-century"
dat.harvest.mid[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2050, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.end <- stack(runs.yr[runs.yr$year==2099, vars.plot])
dat.harvest.end$time <- "end-century"
dat.harvest.end[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2099, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest <- rbind(dat.harvest.pre, dat.harvest.post, dat.harvest.mid, dat.harvest.end)

theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text.x = element_text(size=rel(2), face="bold"),
                       strip.text.y = element_text(size=rel(1),angle=0, face="bold"),
                       strip.background = element_rect(fill=NA),
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
                       legend.key = element_rect(fill=NA))
var.labs <- c("AGB (kgC/m2)", "Tree density (trees/hectare)", "Mean DBH (cm)", "SD of DBH (cm)")
names(var.labs) <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/"

theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text.x = element_text(size=rel(1), face="bold"),
                       strip.text.y = element_text(size=rel(1),angle=0, face="bold"),
                       strip.background = element_rect(fill=NA),
                       strip.placement = "outside",
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
                       axis.title.y = element_blank())

#--------------------------------------#
# Figure S1
#--------------------------------------#
#Supplemental figure of pre and post harvest
dat.harvest$time <-factor(dat.harvest$time, c("pre-harvest", "post-harvest", "mid-century", "end-century"))
png(paste0(path.figures, "HarvestStructure_Pre-Post.png"), width=12, height=8, units="in", res=220)
ggplot(data=dat.harvest[dat.harvest$time == "pre-harvest" | dat.harvest$time == "post-harvest",]) +
  facet_grid(ind~time, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
  geom_boxplot(aes(x=as.factor(rcp), y=values, fill=Management)) +
  scale_x_discrete(name="") +
  scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(strip.text.y.left = element_text(angle = 60))
dev.off()

#--------------------------------------#
# Figure 2
#--------------------------------------#
#Figure for paper that looks at management for initial, mid, and end of century
png(paste0(path.figures, "HarvestStructure_Management.png"), width=12, height=8, units="in", res=220)
ggplot(data=dat.harvest[dat.harvest$time == "mid-century" | dat.harvest$time == "end-century" | dat.harvest$time == "post-harvest",]) +
  facet_wrap(~ind, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
  geom_boxplot(aes(x=as.factor(time), y=values, fill=Management)) +
  scale_x_discrete(name="") +
  scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(strip.text.y.left = element_text(angle = 60))
dev.off()

#--------------------------------------#
# Figure 3
#--------------------------------------#
#Figure for paper that looks at emisisons scenario for initial, mid, and end of century
png(paste0(path.figures, "HarvestStructure_Emissions.png"), width=12, height=8, units="in", res=220)
ggplot(data=dat.harvest[dat.harvest$time == "mid-century" | dat.harvest$time == "end-century" | dat.harvest$time == "post-harvest",]) +
  facet_wrap(~ind, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
  geom_boxplot(aes(x=as.factor(time), y=values, fill=rcp)) +
  scale_x_discrete(name="") +
  #scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(strip.text.y.left = element_text(angle = 60))
dev.off()


#Multiple comparision
runs.late <- runs.yr[runs.yr$year >= 2025, ]
#lme anova analysis of our structural variables and Tukey multiple comparison analysis
mult.df.25 <- data.frame()
mult.df.50 <- data.frame()
mult.df.99 <- data.frame()
struc.var <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")
for(RCP in unique(runs.late$rcp)){
  for(COL in struc.var){
    lm.test.25 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2025 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "25"))
    print(anova(lm.test.25))
    #Doing a multiple comparison across the different management types
    mult.list.25 <- list()
    post.hoc <- glht(lm.test.25, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,3), "*"), round(output$test$pvalues,3))
    mult.list.25[[paste(COL)]]$Var <- COL
    mult.list.25[[paste(COL)]]$rcp <- RCP
    mult.list.25[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.25[[paste(COL)]]$estimate <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$coefficients,2), "(",round(output$test$sigma,2) ,")", "*"), paste0(round(output$test$coefficients,2),"(",round(output$test$sigma,2) ,")"))
    dat.mult.25 <- dplyr::bind_rows(mult.list.25)
    mult.df.25 <- rbind(mult.df.25, dat.mult.25)
    
    
    lm.test.50 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2050 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "50"))
    print(anova(lm.test.50))
    mult.list.50 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.50, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,3), "*"), round(output$test$pvalues,3))
    mult.list.50[[paste(COL)]]$Var <- COL
    mult.list.50[[paste(COL)]]$rcp <- RCP
    mult.list.50[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.50[[paste(COL)]]$estimate <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$coefficients,2), "(",round(output$test$sigma,2) ,")", "*"), paste0(round(output$test$coefficients,2),"(",round(output$test$sigma,2) ,")"))
    dat.mult.50 <- dplyr::bind_rows(mult.list.50)
    mult.df.50 <- rbind(mult.df.50, dat.mult.50)
    
    
    lm.test.99 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2099 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "99"))
    print(anova(lm.test.99))
    mult.list.99 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.99, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,3), "*"), round(output$test$pvalues,3))
    mult.list.99[[paste(COL)]]$Var <- COL
    mult.list.99[[paste(COL)]]$rcp <- RCP
    mult.list.99[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.99[[paste(COL)]]$estimate <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$coefficients,2), "(",round(output$test$sigma,2) ,")", "*"), paste0(round(output$test$coefficients,2),"(",round(output$test$sigma,2) ,")"))
    dat.mult.99 <- dplyr::bind_rows(mult.list.99)
    mult.df.99 <- rbind(mult.df.99, dat.mult.99)
  }
}
#Creating different dataframes to look at specific windows. This information should evtually end up captured in a figure
rcp45.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp45",], Comp ~ Var)
rcp45.25.df$Scenario <- "Low Emmissions"
rcp45.25.df$year <- "2025"
rcp45.25.df <- rcp45.25.df[,c(6,7,1,2,3,4,5)]
rcp85.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp85",], Comp ~ Var)
rcp85.25.df$Scenario <- "High Emmissions"
rcp85.25.df$year <- "2025"
rcp85.25.df <- rcp85.25.df[,c(6,7,1,2,3,4,5)]

rcp45.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp45",], Comp ~ Var)
rcp45.50.df$Scenario <- "Low Emmissions"
rcp45.50.df$year <- "2050"
rcp45.50.df <- rcp45.50.df[,c(6,7,1,2,3,4,5)]
rcp85.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp85",], Comp ~ Var)
rcp85.50.df$Scenario <- "High Emmissions"
rcp85.50.df$year <- "2050"
rcp85.50.df <- rcp85.50.df[,c(6,7,1,2,3,4,5)]

rcp45.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp45",], Comp ~ Var)
rcp45.99.df$Scenario <- "Low Emmissions"
rcp45.99.df$year <- "2099"
rcp45.99.df <- rcp45.99.df[,c(6,7,1,2,3,4,5)]
rcp85.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp85",], Comp ~ Var)
rcp85.99.df$Scenario <- "High Emmissions"
rcp85.99.df$year <- "2099"
rcp85.99.df <- rcp85.99.df[,c(6,7,1,2,3,4,5)]

struc.comp <- rbind(rcp45.25.df,rcp45.50.df, rcp45.99.df, rcp85.25.df, rcp85.50.df, rcp85.99.df)
write.csv(struc.comp, "../data/Struc_val_comp.csv")


runs.late <- runs.yr[runs.yr$year >= 2025, ]
#lme anova analysis of our structural variables
mult.df.25 <- data.frame()
mult.df.50 <- data.frame()
mult.df.99 <- data.frame()
struc.var <- c("agb", "density.tree.convert", "tree.dbh.mean", "tree.dbh.sd")
for(COL in struc.var){
  lm.test.25 <- lme(eval(substitute(j ~ Management*rcp, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2025,], method = "ML")
  print(paste(COL, "25"))
  output <- anova(lm.test.25)
  mult.list.25 <- list()
  output$`p-value` <- ifelse(output$`p-value` <=.05, paste0(round(output$`p-value` ,5), "*"), round(output$`p-value` ,5))
  mult.list.25[[paste(COL)]]$Var <- COL
  mult.list.25[[paste(COL)]]$Comp <- rownames(output)
  mult.list.25[[paste(COL)]]$pvalue <- output$`p-value` 
  mult.list.25[[paste(COL)]]$fvalue <- output$`F-value`
  dat.mult.25 <- dplyr::bind_rows(mult.list.25)
  mult.df.25 <- rbind(mult.df.25, dat.mult.25)
  
  lm.test.50 <- lme(eval(substitute(j ~ Management*rcp, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2050,], method = "ML")
  print(paste(COL, "50"))
  output <- anova(lm.test.50)
  mult.list.50 <- list()
  output$`p-value` <- ifelse(output$`p-value` <=.05, paste0(round(output$`p-value` ,5), "*"), round(output$`p-value` ,5))
  mult.list.50[[paste(COL)]]$Var <- COL
  mult.list.50[[paste(COL)]]$Comp <- rownames(output)
  mult.list.50[[paste(COL)]]$pvalue <- output$`p-value` 
  mult.list.50[[paste(COL)]]$fvalue <- output$`F-value`
  dat.mult.50 <- dplyr::bind_rows(mult.list.50)
  mult.df.50 <- rbind(mult.df.50, dat.mult.50)
  
  lm.test.99 <- lme(eval(substitute(j ~ Management*rcp-1, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2099,], method = "ML")
  print(paste(COL, "99"))
  output <- anova(lm.test.99)
  mult.list.99 <- list()
  output$`p-value` <- ifelse(output$`p-value` <=.05, paste0(round(output$`p-value` ,5), "*"), round(output$`p-value` ,5))
  mult.list.99[[paste(COL)]]$Var <- COL
  mult.list.99[[paste(COL)]]$Comp <- rownames(output)
  mult.list.99[[paste(COL)]]$pvalue <- output$`p-value` 
  mult.list.99[[paste(COL)]]$fvalue <- output$`F-value`
  dat.mult.99 <- dplyr::bind_rows(mult.list.99)
  mult.df.99 <- rbind(mult.df.99, dat.mult.99)
  
}

mult.df.25$Time <- "Post-harvest"

mult.df.50$Time <- "Mid-century"

mult.df.99$Time <- "End of century"


#Creating different dataframes to look at specific windows. This information should evtually end up captured in a figure
struc.comp <- rbind(mult.df.25, mult.df.50, mult.df.99)

struc.comp$pvalue <- ifelse(grepl("*", struc.comp$pvalue, fixed = TRUE), "sig", "N.S.")

struc.comp$Time <- factor(struc.comp$Time, levels=c("Post-harvest", "Mid-century", "End of century"))

struc.sig <- reshape2::dcast(struc.comp, Time + Var ~ Comp, value.var = "fvalue")

struc.sig <- struc.sig[,c("Time", "Var", "Management", "rcp", "Management:rcp")]

write.csv(struc.sig, "../data/Struc_fvalues.csv")


#--------------------------------------#
# Figure 1
#--------------------------------------#
vars.plot <- c("tair", "precip.total", "VPD")

#runs.yr$tair <- runs.yr$tair-273.15
dat.weather <- stack(runs.yr[, vars.plot])
dat.weather[dat.weather$ind=="tair","values"] <- dat.weather[dat.weather$ind=="tair","values"] - 273.15
dat.weather[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.wagg <- aggregate(values~ind+GCM+rcp+RCP.name+year, dat.weather, FUN = mean)

dat.means <- aggregate(values~ind+rcp+RCP.name, dat.weather[dat.weather$year<2020,], FUN = mean)

met.labs <- c("Temperature (C)", "Total precip (mm)", "VPD (Pa)")
names(met.labs) <- c("tair", "precip.total", "VPD")


weath.time <- ggplot(data=dat.wagg)+
  facet_grid(ind~rcp, scales= "free_y", labeller = labeller(ind = met.labs)) +
  geom_line(aes(x=year, y=values, group=GCM)) +
  geom_hline(data = dat.means, aes(yintercept=values), color="red2", linetype="dashed", size=1)+
  labs(x="Year", y="Weather Metrics") +
  theme(axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size=rel(2), face="bold"),
        strip.text.y = element_text(size=rel(1.5), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))


weath.cent <- ggplot(data=dat.wagg[dat.wagg$year == 2050 | dat.wagg$year == 2099,])+
  facet_grid(ind~., scales= "free_y", labeller = labeller(ind = met.labs)) +
  geom_boxplot(aes(x=as.character(year), y=values, fill = rcp)) +
  labs(x="Year", y="Weather Metrics")+
  scale_x_discrete(name="Period", labels=c("mid-century", "end of century")) +
  #ggpubr::stat_compare_means(aes(x=as.character(year), y=values, fill = rcp), method = "t.test")+
  theme(axis.text.y = element_text(size=rel(2), color="black"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=rel(1.5), color="black"),
        axis.title.x = element_text(size=rel(2), color="black", face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size=rel(2), face="bold"),
        strip.text.y = element_text(size=rel(1.5), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))

t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "tair", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "tair", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "precip.total", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85" & dat.wagg$ind== "precip.total", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "VPD", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "VPD", "values"] , paired=T)

t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "tair", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "tair", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "precip.total", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85" & dat.wagg$ind== "precip.total", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "VPD", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "VPD", "values"] , paired=T)

plot_row <- cowplot::plot_grid(weath.time, weath.cent, labels = c("A", "B"), label_size = 15 ,rel_widths = c(2,1))

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Climate Change Over Time by RCP Scenario",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 32
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 150)
  )
weath.plot <- cowplot::plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

png(width= 1000, filename= file.path(path.figures, paste0('Climate_Change_Over_Time.png')))
  weath.plot
dev.off()
