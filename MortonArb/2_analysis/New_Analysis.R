library(readbulk)

path.script <- "C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis"

path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)

runs.yr <- read_bulk(directory = "output", extension = ".csv", header = TRUE)

setwd(path.script)

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
  geom_line(aes(x=year, y=agb, color=Management), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0, max(runs.yr$agb, na.rm=T))) +
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


library("nlme")

runs.start <- runs.yr

#runs.start <- runs.start[runs.start$GCM != "ACCESS1-0", ]

met.vars <- read.csv("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/met_raw.v3/met_tdm_qaqc/CMIP5_TDM_year_byModel.csv")

col <- colnames(runs.start[c(17:35, 37:44)])

dat.interact <- data.frame()
for(COL in col){
  var.list <- list()
  for(VAR in unique(met.vars$var)){
    met.temp <- met.vars[met.vars$var == VAR,]
    
    runs.all <- merge(runs.start, met.temp, by.x= c('GCM', 'rcp', 'year'), by.y= c('model', 'scenario', 'year'))
    
    runs.all <- runs.all[runs.all$GCM != "MIROC-ESM-CHEM",]
    
    runs.all <- runs.all[runs.all$GCM != "MIROC-ESM",]
    
    runs.first <- runs.all[runs.all$year < 2036 & runs.all$year > 2025,]
    
    runs.last <- runs.all[runs.all$year > 2089,]
    
    
    #This is an abomination of aggregate functions that could be combined but I wrote this quick to get an abstract out.
    AGB.num <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.first,
                         FUN = mean)
    
    AGB.num[,c("first.mean.temp")] <- aggregate(mean~Management+GCM+rcp, data =runs.first,
                                                FUN = mean)[,c("mean")]
    
    colnames(AGB.num) <- c("Management", "GCM", "rcp", "first.mean.AGB", "first.mean.temp")
    
    
    AGB.num[,c("last.mean.AGB")] <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.last,
                                              FUN = mean)["eval(as.symbol(COL))"]
    
    AGB.num[,c("last.mean.temp")] <- aggregate(mean~Management+GCM+rcp, data =runs.last,
                                               FUN = mean)[,c("mean")]
    
    
    #Another aggreagate abomination creates differnet data frame structure for another visual. Can compare first and last
    AGB.num$AGB.diff <-  AGB.num$last.mean.AGB - AGB.num$first.mean.AGB
    AGB.num$temp.diff <- AGB.num$last.mean.temp - AGB.num$first.mean.temp
    
    lm.test <- lme(AGB.diff ~ temp.diff*Management, random=list(rcp=~1, GCM=~1), data=AGB.num)
    hold <- anova(lm.test)
    var.list[[paste(VAR, sep="-")]]$MVAR <- COL
    var.list[[paste(VAR, sep="-")]]$value <- c(paste(VAR, "Intercept"), paste(VAR, "additive"), paste(VAR, "Mangement Additive"), paste(VAR, "Mangement Interactive"))
    var.list[[paste(VAR, sep="-")]]$'p-value' <- hold$`p-value`
  }
  dat.var <- dplyr::bind_rows(var.list)
  dat.worth <- dat.var[dat.var$`p-value` < .05,]
  
  dat.worth <- dat.worth[grepl("Interactive", dat.worth$value),]
  dat.interact <- rbind(dat.interact, dat.worth)

}
write.csv(dat.interact, file.path("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/figures/Interactive_Effects.csv"), row.names = F)
