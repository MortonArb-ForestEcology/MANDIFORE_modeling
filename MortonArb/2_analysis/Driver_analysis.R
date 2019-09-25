# Making some figures demonstrating

# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

sec2yr <- (365*24*60*60)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"
dat.base <- "../1_runs/MortonArb_ed_runs.v1/"
runs.done <- dir(dat.base)

# ------------------------------------
# Extract and save the data (skip if already extracted!)
# ------------------------------------
runs.all <- data.frame()
for(RUNID in runs.done){
  run.splt <- stringr::str_split(RUNID, "_")[[1]]
  
  f.yr <- dir(file.path(dat.base, RUNID, "analy"), "-E-")
  
  
  print("")
  print(RUNID)
  pb <- txtProgressBar(min=0, max=length(f.yr), style=3); pb.ind=0
  df.run <- data.frame()
  for(FILE in f.yr){
    pb.ind=pb.ind+1
    setTxtProgressBar(pb, pb.ind)
    
    f.split <- strsplit(FILE, "-")[[1]]
    yr.now <- as.numeric(f.split[4])
    mo.now <- as.numeric(f.split[5])
    
    fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", FILE)) 
    # summary(fnow$var)
    # test <- ncdf4::ncvar_get(fnow, "MMEAN_GND_TEMP_PY")
    
    dat.pch <- data.frame(area=ncdf4::ncvar_get(fnow, "AREA"),
                          water.deficit=ncdf4::ncvar_get(fnow, "AVG_MONTHLY_WATERDEF"))
    
    df.now <- data.frame(year=yr.now,
                         month=mo.now,
                         temp.air= ncdf4::ncvar_get(fnow, "MMEAN_ATM_TEMP_PY")-273.16,
                         CO2.air = ncdf4::ncvar_get(fnow, "MMEAN_ATM_CO2_PY"),
                         precip  = ncdf4::ncvar_get(fnow, "MMEAN_PCPG_PY"),
                         runoff = ncdf4::ncvar_get(fnow, "MMEAN_RUNOFF_PY"),
                         temp.ground=ncdf4::ncvar_get(fnow, "MMEAN_GND_TEMP_PY")-273.16, # Just getting surface soil temp
                         par.ground = ncdf4::ncvar_get(fnow, "MMEAN_PAR_GND_PY"),
                         water.deficit=sum(dat.pch$water.deficit*dat.pch$area)
                         )
    
    ncdf4::nc_close(fnow)
    
    df.run <- rbind(df.run, df.now)
  }
  
  # Making life a *hair* easier by aggregating to the annual scale
  df.run$RunID <- as.factor(RUNID)
  df.run$GCM <- as.factor(run.splt[3] )
  df.run$RCP <- as.factor(run.splt[4])
  df.run$CO2.type <- as.factor(run.splt[5])
  df.run$Management <- as.factor(substr(run.splt[6], 5, nchar(run.splt[6])))
  
  # Save the monthly output since we have it anyways
  write.csv(runs.all, file.path(path.google, "output", paste0("Summary_Site_Month_", RUNID,".csv")), row.names=F)
  
    
  df.run <- aggregate(df.run[,c("temp.air", "CO2.air", "precip", "runoff", "temp.ground", "par.ground", "water.deficit")],
                      by=df.run[,c("RunID", "GCM", "RCP", "CO2.type", "Management", "year")],
                      FUN=mean)
  df.run[,c("precip", "runoff")] <- df.run[,c("precip", "runoff")]*sec2yr
  

  runs.all <- rbind(runs.all, df.run)
}
runs.all$Management <- factor(runs.all$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.all$CO2.type <- car::recode(runs.all$CO2.type, "'CO2'='dynamic'; 'statCO2'='static'")
summary(runs.all)

write.csv(runs.all, file.path(path.google, "output", "Summary_Site_Year.csv"), row.names=F)
# ------------------------------------


# ------------------------------------
# Making some figures of the drivers
# ------------------------------------
runs.all <- read.csv(file.path(path.google, "output", "Summary_Site_Year.csv"))
runs.all$CO2.type <- car::recode(runs.all$CO2.type, "'dynamic'='Dynamic CO2'; 'static'='Static CO2'")
summary(runs.all)

plot.temp <- ggplot(data=runs.all[runs.all$Management=="None" & runs.all$year>2018,]) +
  facet_grid(. ~ CO2.type) +
  geom_line(aes(x=year, y=temp.air, color=RCP), size=2) +   
  scale_color_manual(values=c("#2c7bb6", "#d7191c", "black")) +
  scale_y_continuous(name="Temp.") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  # guides(color=F) +
  theme(legend.position="top",
        legend.key = element_rect(fill=NA),
        legend.text=element_text(size=rel(1.5), color="black"),
        legend.title=element_text(size=rel(1.5), color="black", face="bold"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5), color="black", margin=unit(c(1,1,1,2.25), "lines")),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), color="black", face="bold"),
        axis.ticks.length = unit(-0.5, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(2.5, "lines"),
        plot.margin = unit(c(1,1.5,0.5,1), "lines"))



plot.precip <- ggplot(data=runs.all[runs.all$Management=="None" & runs.all$year>2018,]) +
  facet_grid(. ~ CO2.type) +
  geom_line(aes(x=year, y=precip, color=RCP), size=2) +
  scale_color_manual(values=c("#2c7bb6", "#d7191c", "black")) +
  scale_y_continuous(name="Precip.") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  guides(color=F) +
  theme(legend.position="bottom",
        legend.key = element_rect(fill=NA),
        legend.text=element_text(size=rel(1.5), color="black"),
        legend.title=element_text(size=rel(2), color="black", face="bold"),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=rel(1.5), color="black", margin=unit(c(1,1,1,1), "lines")),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=rel(1.5), color="black", face="bold"),
        axis.ticks.length = unit(-0.5, "lines"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(2.5, "lines"),
        plot.margin = unit(c(0.5,1.5,0.5,1), "lines"))


plot.CO2 <-  ggplot(data=runs.all[runs.all$Management=="None" & runs.all$year>2018,]) +
  facet_grid(. ~ CO2.type) +
  geom_line(aes(x=year, y=CO2.air, color=RCP), size=2) +
  scale_color_manual(name="Scenario", values=c("#2c7bb6", "#d7191c", "black")) +
  scale_y_continuous(name="CO2") +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  guides(color=F) +
  theme(legend.position=c(0.1, 0.8),
        legend.key = element_rect(fill=NA),
        legend.text=element_text(size=rel(1.5), color="black"),
        legend.title=element_text(size=rel(2), color="black", face="bold"),
        axis.text.x=element_text(size=rel(1.75), color="black", margin=unit(c(1,1,1,1), "lines")),
        axis.text.y=element_text(size=rel(1.5), color="black", margin=unit(c(1,1,1,1.5), "lines")),
        axis.title=element_text(size=rel(1.5), color="black", face="bold"),
        axis.ticks.length = unit(-0.5, "lines"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(2.5, "lines"),
        plot.margin = unit(c(0.5,1.5,1,1), "lines"))

png(file.path(path.figs, "ClimateDriver_Scenarios.png"), height=6, width=9.5, units="in", res=180)
cowplot::plot_grid(plot.temp, plot.precip, plot.CO2, ncol=1, rel_heights = c(1.5, 0.9, 1.25))
dev.off()
# ------------------------------------
