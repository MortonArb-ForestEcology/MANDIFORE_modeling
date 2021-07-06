#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script crates plots and tables exploring variables of interest
# Inputs: ED2 Morton Arb site data 
# Outputs: Plots mmostly exploring agb, basal tree area, and density
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(readbulk)

path.script <- "C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis"

path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)

runs.yr <- read_bulk(directory = "output", extension = ".csv", header = TRUE)

setwd(path.script)


library("nlme")

runs.start <- runs.yr

#runs.start <- runs.start[runs.start$GCM != "ACCESS1-0", ]

met.vars <- read.csv("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/met_raw.v3/met_tdm_qaqc/CMIP5_TDM_year_byModel.csv")

#col <- colnames(runs.start[c(17:35, 37:44)])
interest <- c("basal.area.tree", "density.tree", "agb", "nee", "soil.moist.surf", "soil.moist.deep")

dat.interact <- data.frame()
dat.all <- data.frame()
dat.value <- data.frame()
diff.list <- list()
for(COL in interest){
  var.list <- list()
  for(VAR in unique(met.vars$var)){
    met.temp <- met.vars[met.vars$var == VAR,]
    
    runs.all <- merge(runs.start, met.temp, by.x= c('GCM', 'rcp', 'year'), by.y= c('model', 'scenario', 'year'))
    
    runs.all <- runs.all[runs.all$GCM != "MIROC-ESM-CHEM",]
    
    runs.all <- runs.all[runs.all$GCM != "MIROC-ESM",]
    
    runs.first <- runs.all[runs.all$year < 2036 & runs.all$year > 2025,]
    
    runs.last <- runs.all[runs.all$year > 2089,]
    
    
    #This is an abomination of aggregate functions that could be combined but I wrote this quick to get an abstract out.
    ED.num <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.first,
                         FUN = mean)
    
    ED.num[,c("first.mean.temp")] <- aggregate(mean~Management+GCM+rcp, data =runs.first,
                                                FUN = mean)[,c("mean")]
    
    colnames(ED.num) <- c("Management", "GCM", "rcp", "first.mean.ED", "first.mean.temp")
    
    
    ED.num[,c("last.mean.ED")] <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.last,
                                              FUN = mean)["eval(as.symbol(COL))"]
    
    ED.num[,c("last.mean.temp")] <- aggregate(mean~Management+GCM+rcp, data =runs.last,
                                               FUN = mean)[,c("mean")]
    
    
    #Another aggreagate abomination creates differnet data frame structure for another visual. Can compare first and last
    ED.num$ED.diff <-  ED.num$last.mean.ED - ED.num$first.mean.ED
    ED.num$temp.diff <- ED.num$last.mean.temp - ED.num$first.mean.temp
    
    ED.num$Management <- factor(ED.num$Management, levels = c("MgmtNone", "MgmtGap", "MgmtShelter", "MgmtUnder"))
    lm.test <- lme(ED.diff ~ temp.diff*Management, random=list(rcp=~1, GCM=~1), data=ED.num)
    hold <- anova(lm.test)
    
    sum <- summary(lm.test)
    df.eff <- as.data.frame(sum$tTable)
    df.eff$Fixedeff <- rownames(df.eff)
    df.eff$Fixedeff <- gsub("temp.diff", VAR , df.eff$Fixedeff)
    df.eff$Fixedeff <- gsub("Management", paste0("Management(", VAR, ")" ) , df.eff$Fixedeff)
    
    var.list[[paste(COL, VAR, sep="-")]]$MVAR <- paste(COL, VAR, sep=":")
    var.list[[paste(COL, VAR, sep="-")]]$Fixedeff <- df.eff$Fixedeff
    var.list[[paste(COL, VAR, sep="-")]]$Value <- df.eff$Value
    var.list[[paste(COL, VAR, sep="-")]]$pvalue <- df.eff$`p-value`
    
    diff.list[[paste0(COL, VAR, sep = "-")]]$GCM <- ED.num$GCM
    diff.list[[paste0(COL, VAR, sep = "-")]]$Management <- ED.num$Management
    diff.list[[paste0(COL, VAR, sep = "-")]]$rcp <- ED.num$rcp
    diff.list[[paste0(COL, VAR, sep = "-")]]$Weather.VAR <- VAR
    diff.list[[paste0(COL, VAR, sep = "-")]]$ED.VAR <- COL
    diff.list[[paste0(COL, VAR, sep = "-")]]$ED.diff <- ED.num$ED.diff
    diff.list[[paste0(COL, VAR, sep = "-")]]$Weather.diff <- ED.num$temp.diff
  }
  dat.diff <- dplyr::bind_rows(diff.list)
  
  dat.var <- dplyr::bind_rows(var.list)
  dat.value <- rbind(dat.value, dat.var)
  dat.worth <- dat.var[dat.var$pvalue < .05,]
  
  dat.worth <- dat.worth[grepl(":M", dat.worth$Fixedeff),]
  dat.all <- rbind(dat.all, dat.diff)
  dat.interact <- rbind(dat.interact, dat.worth)

}

library(tidyr)
dat.interact <-  dat.interact %>%
  separate(Fixedeff, c("WeatherVAR", "Management"), sep = ":")
dat.interact$Management <- gsub(".*)","", dat.interact$Management )
dat.interact$MVAR <- gsub("\\:.*","", dat.interact$MVAR)


write.csv(dat.interact, file.path("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/figures/Interactive_Effects.csv"), row.names = F)

dat.value <-  dat.value %>%
  separate(MVAR, c("ED.VAR", "Weather.VAR"), sep = ":")


for(MVAR in unique(dat.interact$MVAR)){
  dat.temp <- dat.interact[dat.interact$MVAR == MVAR,]
  for(WVAR in unique(dat.temp$WeatherVAR)){
    dat.figs <- dat.all[dat.all$ED.VAR == MVAR & dat.all$Weather.VAR == WVAR, ]
    dat.tbl <- dat.value[dat.value$ED.VAR == MVAR & dat.value$Weather.VAR == WVAR,]
    write.csv(dat.tbl, file.path("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/figures", paste0(MVAR, "_and_", WVAR, "_lme", ".csv")), row.names = F)
    png(file.path("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/figures", paste0(MVAR, "_interactive_with_", WVAR , ".png")))
    figs <- ggplot(dat.figs)+
      geom_point(aes(x = Weather.diff, y = ED.diff, color = Management))+
      geom_smooth(aes(x = Weather.diff, y = ED.diff, color = Management, fill = Management),method = "lm")+
      ggtitle(paste0(MVAR, " Interactive effects with ", WVAR))+
      xlab(WVAR)+
      ylab(MVAR)
    ggsave(figs, file=paste0(file.path("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/figures", paste0(MVAR, "_interactive_with_", WVAR , ".png"))))
    #dev.off()
  }
}


library(ggplot2)
# ------------------------------------
# Plot the data
# These figures aren't great, but are what's in the google drive folder
# ------------------------------------
path.figs <- file.path("../figures/")
# dir.exists(path.figs)

png(file.path(path.figs, "Explore_AGB_by_PFT_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ Management) +
  # facet_wrap( ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=agb, color=pft, linetype=as.factor(rcp)), size=1.5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0,max(runs.yr$agb, na.rm = T)), expand=c(0,0)) +
  scale_color_discrete(name = "PFT")+
  scale_linetype_discrete(name = "RCP")+
  ggtitle("Aboveground Biomass") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_All.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=stress, color=as.factor(pft), linetype=as.factor(rcp))) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  scale_color_discrete(name = "PFT")+
  scale_linetype_discrete(name = "RCP")+
  ggtitle("Average Stress by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_AGB_Total_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[]) +
  facet_grid(GCM ~ rcp) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=soil.moistpot.deep, color=Management), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)") +
  ggtitle("Total Aboveground Biomass") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=basal.area.tree, color=as.factor(rcp), linetype=as.factor(rcp)), size=1.5) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  scale_color_discrete(name = "RCP")+
  scale_linetype_discrete(name = "RCP")+
  ggtitle("Basal Area") +
  theme_bw()
dev.off()


#------------------------------------------------------------------------------------------------------------#
# These require PFT's which we don't have at the Site level
#------------------------------------------------------------------------------------------------------------#

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=as.factor(PFT), linetype=as.factor(RCP)))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.g45)), expand=c(0,0)) +
  scale_color_discrete(name = "PFT")+
  scale_linetype_discrete(name = "RCP")+
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees_v2.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=Management, linetype=RCP)) +
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.g45)), expand=c(0,0)) +
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Density_by_PFT_Time_SmallTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.sap,
                color=Management, linetype=RCP))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.sap)), expand=c(0,0)) +
  ggtitle("Density Saplings 1-10 cm DBH by PFT") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_AGB_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=Management, linetype=RCP), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)") +
  ggtitle("Oak Aboveground Biomass (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Oak Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_PropBA_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.prop, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Proportion") +
  ggtitle("Oak Proportion by Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_All.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress (All Oaks)") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Stress_PFT10_Time_BigTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft.g45, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress >45 cm DBH") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_SmallTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.sap, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.1, 0, 0.025, 0.04), labels = c("-0.1\n(Even Worse)", "0\n(Very Bad)", "0.025\n(Not Much Better)", "0.04\n(Nowhere close;\nGood = 1!)"), limits=c(-0.0025,0.045)) +
  ggtitle("Average Oak Stress 1-10 cm DBH") +
  theme_bw()
dev.off()


#Evaluating Tree density over time
col <- colnames(runs.yr)

col <- col[c(3, 5, 7, 8, 17:35, 37:44)]

runs.mng <- subset(runs.yr, select = c(col))

runs.mng <- aggregate(.~Management+year+month+rcp, runs.mng, mean)

png(file.path(path.figs, "Explore_Tree_Density_Total_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mng[]) +
  facet_wrap(~ rcp) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_smooth(aes(x=year, y=basal.area.tree, color=Management), size=1.5) +
  scale_y_continuous(name="Tree Density") +
  ggtitle("Tree Density") +
  theme_bw()
dev.off()


#Evaluating Tree density over time
col <- colnames(runs.yr)

col <- col[c(3, 5, 7, 8:35, 37:44)]

runs.mng <- subset(runs.yr, select = c(col))

runs.mng <- aggregate(.~Management+year+month+rcp, runs.mng, mean)

png(file.path(path.figs, "Explore_Surface_Pressure_Total_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mng[]) +
  facet_wrap(~ rcp) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_smooth(aes(x=year, y=psurf, color=Management), size=1.5) +
  scale_y_continuous(name="Surface_Pressure") +
  ggtitle("Surface_Pressure") +
  theme_bw()
dev.off()

sum <- summary(lm.test)
df.eff <- as.data.frame(sum$tTable)
df.eff$Fixedeff <- rownames(df.eff)




