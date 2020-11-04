# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"

dat.base <- "../1_runs/MortonArb_ed_runs.v2/"
runs.done <- dir(dat.base, "statCO2")

runs.all <- data.frame()
for(RUNID in runs.done){
  run.splt <- stringr::str_split(RUNID, "_")[[1]]
  
  # Some models have different num. hypens, so use model name as index
  mod.splt <- length(stringr::str_split(run.splt[3], "-")[[1]])
  
  f.yr <- dir(file.path(dat.base, RUNID, "analy"), "-Y-")
  
  print("")
  print(RUNID)
  pb <- txtProgressBar(min=0, max=length(f.yr), style=3); pb.ind=0
  df.run <- data.frame()
  df.run <- data.frame()
  for(FILE in f.yr){
    pb.ind=pb.ind+1
    setTxtProgressBar(pb, pb.ind)

    f.split <- strsplit(FILE, "-")[[1]]
    yr.now <- as.numeric(f.split[mod.splt+2])
    mo.now <- as.numeric(f.split[mod.splt+3])
    
    fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", FILE)) 
    # summary(fnow$var)
    # test <- ncdf4::ncvar_get(fnow, "CB")
    
    df.pch <- data.frame(year=yr.now,
                         # month=mo.now,
                         patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"),
                         n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                         patch.area = ncdf4::ncvar_get(fnow, "AREA"),
                         patch.age = ncdf4::ncvar_get(fnow, "AGE"))
    df.pch$patchID <- 1:nrow(df.pch)
    # summary(df.pch)
    
    df.co <- data.frame(year=yr.now,
                        # month=mo.now,
                        PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")),
                        Density=ncdf4::ncvar_get(fnow, "NPLANT"),
                        DBH=ncdf4::ncvar_get(fnow, "DBH"),
                        AGB=ncdf4::ncvar_get(fnow, "AGB_CO"),
                        BA=ncdf4::ncvar_get(fnow, "BA_CO"),
                        Stress=ncdf4::ncvar_get(fnow, "CBR_BAR"))
    # summary(df.co)
    
    ncdf4::nc_close(fnow)
    
    for(i in 1:nrow(df.pch)){
      df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("patchID", "patch.area", "patch.age")] <- df.pch[i,c("patchID", "patch.area", "patch.age")]
    }
    # tail(df.co)
    
    df.co$dens.wt <- df.co$Density*df.co$patch.area
    
    for(PFT in unique(df.co$PFT)){
      df.co[df.co$PFT==PFT,"pft.wt"] <- df.co$dens.wt[df.co$PFT==PFT]/sum(df.co$dens.wt[df.co$PFT==PFT])
      df.co[df.co$PFT==PFT & df.co$DBH>45,"pft.wt.g45"] <- df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45]/sum(df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45])
    }
    summary(df.co)
    
    df.run <- rbind(df.run, df.co)
  }
  # run.splt
  df.run$RunID <- as.factor(RUNID)
  df.run$GCM <- as.factor(run.splt[3])
  df.run$RCP <- as.factor(run.splt[4])
  df.run$CO2 <- as.factor(run.splt[5])
  df.run$Management <- as.factor(substr(run.splt[6], 5, nchar(run.splt[6])))

  runs.all <- rbind(runs.all, df.run)
}
runs.all$Management <- factor(runs.all$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.all$CO2 <- car::recode(runs.all$CO2, "'CO2'='dynamic'; 'statCO2'='static'")
runs.all$AGB.wt <- runs.all$AGB*runs.all$dens.wt
runs.all$BA.wt <- runs.all$BA*runs.all$dens.wt
runs.all$Stress.wt.pft <- runs.all$Stress*runs.all$pft.wt
runs.all$Stress.wt.pft.g45 <- runs.all$Stress*runs.all$pft.wt.g45
summary(runs.all)

write.csv(runs.all, file.path(path.google, "output", "Summary_PFTs_Cohort_Year.csv"), row.names=F)

summary(runs.all[runs.all$GCM=="ACCESS1-0" & runs.all$year>2030 & runs.all$PFT!=5,])

# ------------------------------------
# Glance at and aggregate the data
# ------------------------------------

ggplot(data=runs.all[runs.all$PFT!=5 & runs.all$DBH>10,])+
  facet_grid(Management ~ GCM) +
  geom_histogram(aes(x=DBH, fill=PFT))

ggplot(data=runs.all[runs.all$PFT!=5 & runs.all$DBH>10,])+
  facet_grid(Management ~ GCM) +
  geom_histogram(aes(x=Stress, fill=PFT))

summary(runs.all[runs.all$GCM=="bcc-csm1-1" & runs.all$PFT!=5,])

# Comparing Basal area o
runs.yr <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft", "Stress.wt.pft.g45")],
                     by=runs.all[runs.all$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                     FUN=sum, na.rm=T)
summary(runs.yr); dim(runs.yr)


# Getting the number of 45cm (~18") DBH stems
runs.yr2 <- aggregate(runs.all[runs.all$PFT!=5 & runs.all$DBH>45,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft.g45")],
                     by=runs.all[runs.all$PFT!=5 & runs.all$DBH>45,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                     FUN=sum)
names(runs.yr2)[8:11] <- c("AGB.g45", "BA.g45", "Density.g45", "Stress.g45")
summary(runs.yr2); dim(runs.yr2)


# Getting some regen stats
runs.yr3 <- aggregate(runs.all[runs.all$PFT!=5 & runs.all$DBH>1 & runs.all$DBH<10,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft")],
                      by=runs.all[runs.all$PFT!=5 & runs.all$DBH>1 & runs.all$DBH<10,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                      FUN=sum)
names(runs.yr3)[8:11] <- c("AGB.sap", "BA.sap", "Density.sap", "Stress.sap")
summary(runs.yr3); dim(runs.yr2)

runs.yr <- merge(runs.yr, runs.yr2, all.x=T)
runs.yr[is.na(runs.yr$AGB.g45), c("AGB.g45", "BA.g45", "Density.g45")] <- 0
runs.yr <- merge(runs.yr, runs.yr3, all.x=T)
runs.yr[is.na(runs.yr$AGB.sap), c("AGB.sap", "BA.sap", "Density.sap")] <- 0
summary(runs.yr)

runs.yr.tot <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt")],
                         by=runs.all[runs.all$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year")],
                         FUN=sum)
names(runs.yr.tot)[7:9] <- c("AGB.tot", "BA.tot", "Density.Tot")
summary(runs.yr.tot)


runs.yr <- merge(runs.yr, runs.yr.tot, all.x=T)
runs.yr$AGB.prop <- runs.yr$AGB.wt/runs.yr$AGB.tot
runs.yr$BA.prop <- runs.yr$BA.wt/runs.yr$BA.tot
summary(runs.yr)

write.csv(runs.yr, file.path(path.google, "output", "Summary_PFTs_Site_Year.csv"), row.names=F)
# ------------------------------------

# ------------------------------------
# Plot the data
# ------------------------------------
path.figs <- file.path(path.google, "figures/ensemble/explore")
# dir.exists(path.figs)

png(file.path(path.figs, "Explore_AGB_by_PFT_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ Management) +
  # facet_wrap( ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=PFT, linetype=RCP), size=1.5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0,max(runs.yr$AGB.wt)), expand=c(0,0)) +
  ggtitle("Aboveground Biomass by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_AGB_by_PFT_Time_v2.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=Management, linetype=RCP), size=1.5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0,max(runs.yr$AGB.wt)), expand=c(0,0)) +
  ggtitle("Aboveground Biomass by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_All.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=PFT, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_All_v2.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=Management, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_BigTrees.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[,]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.g45, color=PFT, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress Trees >45 cm DBH by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_AGB_Total_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.tot, color=Management), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0, max(runs.yr$AGB.tot, na.rm=T))) +
  ggtitle("Total Aboveground Biomass") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_by_PFT_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=PFT, linetype=RCP)) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Basal Area by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_by_PFT_Time_v2.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=Management, linetype=RCP)) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Basal Area by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=PFT, linetype=RCP))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.g45)), expand=c(0,0)) +
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees_v2.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=Management, linetype=RCP)) +
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.g45)), expand=c(0,0)) +
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Density_by_PFT_Time_SmallTrees.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.sap,
                color=Management, linetype=RCP))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.yr$Density.sap)), expand=c(0,0)) +
  ggtitle("Density Saplings 1-10 cm DBH by PFT") +
  theme_bw()
dev.off()

  
png(file.path(path.figs, "Explore_AGB_PFT10_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=Management, linetype=RCP), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)") +
  ggtitle("Oak Aboveground Biomass (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_PFT10_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Oak Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_PropBA_PFT10_Time.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.prop, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Proportion") +
  ggtitle("Oak Proportion by Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_All.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress (All Oaks)") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Stress_PFT10_Time_BigTrees.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft.g45, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress >45 cm DBH") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_SmallTrees.png"), height=6, width=8, units="in", res=120)
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.sap, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.1, 0, 0.025, 0.04), labels = c("-0.1\n(Even Worse)", "0\n(Very Bad)", "0.025\n(Not Much Better)", "0.04\n(Nowhere close;\nGood = 1!)"), limits=c(-0.0025,0.045)) +
  ggtitle("Average Oak Stress 1-10 cm DBH") +
  theme_bw()
dev.off()

# ------------------------------------
 