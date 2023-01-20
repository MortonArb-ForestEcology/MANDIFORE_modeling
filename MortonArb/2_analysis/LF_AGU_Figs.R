#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script creates figures for Lucien's 2022 AGU poster
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)
library(ggplot2)
library(ggpubr)
library(multcomp)


path.figures <- "../figures/"
path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"
path.read <- file.path(path.google, "processed_data/All_runs_yearly.csv")

runs.comb <- read.csv(paste0(path.read))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

runs.comb$RCP.name <- car::recode(runs.comb$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.comb$RCP.name <- factor(runs.comb$RCP.name, levels=c("Low Emmissions", "High Emissions"))

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff.future),]

#-------------------------------------------------------------#
#Counting the number of major agb loss events
#-------------------------------------------------------------#
#agb.extreme <- as.numeric(quantile(runs.late$agb.rel.diff.future, probs = c(.025)))

runs.comb$loss.event.20 <- ifelse(runs.comb$agb.rel.diff <= -.20, 1, 0)

#Counting individual instances of a crash beginning
for(i in 5:nrow(runs.comb)){
  DRIVE <- runs.comb[i, "Driver.set"]
  MNG <- runs.comb[i, "Management"]
  YR <- runs.comb[i, "year"]
  if(YR != 2007){
    prev.20 <- runs.comb[runs.comb$Driver.set == DRIVE & runs.comb$Management == MNG & runs.comb$year == YR-1 , "loss.event.20"]
    runs.comb[i, "nonseq.loss.event.20"] <- ifelse((runs.comb[i, "loss.event.20"] == 1 & prev.20 ==F), 1, 0)
  }
}

#-----------------------------#
# FIGURE 1
#-----------------------------#
vars.plot <- c("tair", "precip.total", "VPD")

#runs.comb$tair <- runs.comb$tair-273.15
dat.weather <- stack(runs.comb[, vars.plot])
dat.weather[dat.weather$ind=="tair","values"] <- dat.weather[dat.weather$ind=="tair","values"] - 273.15
dat.weather[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.comb[, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.wagg <- aggregate(values~ind+GCM+rcp+RCP.name+year, dat.weather, FUN = mean)

dat.means <- aggregate(values~ind+rcp+RCP.name, dat.weather[dat.weather$year<2020,], FUN = mean)

met.labs <- c("Temperature (C)", "Total precip (mm)", "VPD (Pa)")
names(met.labs) <- c("tair", "precip.total", "VPD")


weath.time <- ggplot(data=dat.wagg)+
  facet_grid(ind~RCP.name, scales= "free_y", labeller = labeller(ind = met.labs), switch = "y") +
  geom_line(aes(x=year, y=values, group=GCM)) +
  geom_hline(data = dat.means, aes(yintercept=values), color="red2", linetype="dashed", size=1)+
  labs(x="Year", y="Weather Metrics") +
  theme(axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size=rel(2), face="bold"),
        strip.text.y = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        strip.placement = "outside",
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
        axis.title.y = element_blank())

png(paste0(path.figures, "Weather_over_time.png"), width=11, height=8, units="in", res=500)
  weath.time
dev.off()

#-----------------------------#
# FIGURE 2
#-----------------------------#

#Modified so that red dots are now looking at unique occurences of a crash instead of the total number of years crashing
png(paste0(path.figures, "AGB_static_crashes.png"), width=10, height=8, units="in", res=300)
ggplot(data=runs.comb)+
  facet_grid(Management ~ RCP.name) +
  geom_rect(xmin=2020, xmax=2024, ymin=-Inf, ymax=Inf, fill="orange3", alpha=0.9) +
  geom_line(aes(x=year, y=agb, group=GCM)) +
  #geom_point(data=runs.comb[runs.comb$loss.event.20==T & runs.comb$year>2024 & !is.na(runs.comb$agb.rel.diff),], aes(x=year, y=agb, group=GCM), color="blue2", size=3) +
  geom_point(data=runs.comb[runs.comb$nonseq.loss.event.20==T & runs.comb$year>2024 & !is.na(runs.comb$agb.rel.diff),], aes(x=year, y=agb, group=GCM), color="red2", size=3) +
  labs(x="Year", y="Aboveground Biomass (kgC/m2)") +
  geom_text(x=2025, y=25, label="Harvest Period: 2020-2024", color="orange3", hjust=0) +
  theme(axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))
dev.off()

#-----------------------------#
# FIGURE 3 doesn't use R. It's an image of management scenarios
#-----------------------------#

#-----------------------------#
# FIGURE 4
#-----------------------------#
# -----------------------------------------------------------
# Looking at the effect of harvest on structural variables
# -----------------------------------------------------------
vars.plot <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")

dat.harvest.pre <- stack(runs.comb[runs.comb$year==2019, vars.plot])
dat.harvest.pre$time <- "pre-harvest"
dat.harvest.pre[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.comb[runs.comb$year==2019, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.post <- stack(runs.comb[runs.comb$year==2025, vars.plot])
dat.harvest.post$time <- "post-harvest"
dat.harvest.post[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.comb[runs.comb$year==2025, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.mid <- stack(runs.comb[runs.comb$year==2050, vars.plot])
dat.harvest.mid$time <- "mid-century"
dat.harvest.mid[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.comb[runs.comb$year==2050, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.end <- stack(runs.comb[runs.comb$year==2099, vars.plot])
dat.harvest.end$time <- "end-century"
dat.harvest.end[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.comb[runs.comb$year==2099, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest <- rbind(dat.harvest.pre, dat.harvest.post, dat.harvest.mid, dat.harvest.end)

theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text.x = element_text(size=rel(2), face="bold"),
                       strip.text.y = element_text(size=rel(2),angle=230, face="bold"),
                       strip.background = element_rect(fill=NA),
                       strip.placement = "outside",
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_blank(),
                       legend.key = element_rect(fill=NA),
                       legend.text = element_text(size=rel(2)),
                       legend.title = element_text(size=rel(2)),
                       legend.position = "top",
                       axis.title.y = element_blank())
var.labs <- c("AGB (kgC/m2)", "Tree density (trees/m2)", "Mean DBH (cm)", "SD of DBH (cm)")
names(var.labs) <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")


#Figure for paper that use mid and end of century
png(paste0(path.figures, "HarvestStructure_Mid.png"), width=13, height=11, units="in", res=300)
  ggplot(data=dat.harvest[dat.harvest$time == "mid-century",]) +
    facet_grid(ind~RCP.name, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
    geom_boxplot(aes(x=as.factor(year), y=values, fill=Management)) +
    scale_x_discrete(name="Mid-century") +
    scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
    theme.clean+
    ggtitle("Mid-century structure by Management")+
    theme(axis.text.x=element_blank())
dev.off()
