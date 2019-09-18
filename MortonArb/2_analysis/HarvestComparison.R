# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)


dat.base <- "../1_runs/MortonArb_ed_runs.v1/"
runs.done <- dir(dat.base)

runs.all <- data.frame()
for(RUNID in runs.done){
  run.splt <- stringr::str_split(RUNID, "_")[[1]]
  
  f.yr <- dir(file.path(dat.base, RUNID, "analy"), "-Y-")
  
  df.run <- data.frame()
  for(FILE in f.yr){
    f.split <- strsplit(FILE, "-")[[1]]
    yr.now <- as.numeric(f.split[4])
    mo.now <- as.numeric(f.split[5])
    
    fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", FILE)) 
    # summary(fnow$var)
    # test <- ncdf4::ncvar_get(fnow, "TOTAL_AGB")
    
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
                        DBH=ncdf4::ncvar_get(fnow, "DBH"),
                        AGB=ncdf4::ncvar_get(fnow, "AGB_CO"),
                        Density=ncdf4::ncvar_get(fnow, "NPLANT"))
    # summary(df.co)
    
    ncdf4::nc_close(fnow)
    
    for(i in 1:nrow(df.pch)){
      df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("patchID", "patch.area", "patch.age")] <- df.pch[i,c("patchID", "patch.area", "patch.age")]
    }
    # tail(df.co)
    
    df.co$dens.wt <- df.co$Density*df.co$patch.area
    summary(df.co)
    
    df.run <- rbind(df.run, df.co)
  }
  # run.splt
  df.run$RunID <- RUNID
  df.run$GCM <- as.factor(run.splt[3] )
  df.run$RCP <- as.factor(run.splt[4])
  df.run$CO2 <- as.factor(run.splt[5])
  df.run$Management <- as.factor(substr(run.splt[6], 5, nchar(run.splt[6])))

  runs.all <- rbind(runs.all, df.run)
}
runs.all$CO2 <- car::recode(runs.all$CO2, "'CO2'='dynamic'; 'statCO2'='static'")
runs.all$BA <- (pi*(runs.all$DBH/2)^2)*runs.all$Density
runs.all$BA.wt <- runs.all$BA*runs.all$patch.area
runs.all$AGB.wt <- runs.all$AGB*runs.all$dens.wt
summary(runs.all)

# Comparing Basal area o
runs.yr <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt")],
                     by=runs.all[runs.all$PFT!=5,c("GCM", "RCP", "CO2", "Management", "year", "PFT")],
                     FUN=sum)
summary(runs.yr)

runs.yr.tot <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt")],
                         by=runs.all[runs.all$PFT!=5,c("GCM", "RCP", "CO2", "Management", "year")],
                         FUN=sum)
names(runs.yr.tot)[6:7] <- c("AGB.tot", "BA.tot")
summary(runs.yr.tot)


runs.yr <- merge(runs.yr, runs.yr.tot)
runs.yr$AGB.prop <- runs.yr$AGB.wt/runs.yr$AGB.tot
runs.yr$BA.prop <- runs.yr$BA.wt/runs.yr$BA.tot
summary(runs.yr)

library(ggplot2)
ggplot(data=runs.yr) +
  facet_grid(RCP ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=0, ymax=1, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=PFT, linetype=CO2)) 
  

ggplot(data=runs.yr[runs.yr$PFT!="5",]) +
  facet_grid(RCP ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=0, ymax=max(runs.yr$BA.wt), alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=PFT, linetype=CO2))

  
ggplot(data=runs.yr[runs.yr$PFT=="10",]) +
  facet_grid(CO2 ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=0, ymax=1, alpha=0.1) +
  geom_line(aes(x=year, y=BA.prop, color=Management, linetype=CO2))
  