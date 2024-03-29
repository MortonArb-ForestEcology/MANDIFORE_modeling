# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Perform a visual check on the met that has been temporally downscaled
# Creator: Christy Rollinson, updated 15 June 2022
# Contact: crollinson@mortonarb.org
# -----------------------------------
# Description
# -----------------------------------
# Post-bias correct QAQC to make sure diurnal patterns and transitions between days (& years!) look okay
# -----------------------------------
# General Workflow Components
# -----------------------------------
# 0. Set up file structure, load packages, etc
# 1. Read in & format met data
#    1.1. Raw 
#    1.2. Bias-corrected (summarize)
# 2. QAQC graphing
# -----------------------------------
# rm(list=ls())

# -----------------------------------
# 0. Set up file structure, load packages, etc
# -----------------------------------
library(ncdf4)
library(ggplot2)
library(stringr)

# Ensemble directories
# wd.base <- "/home/crollinson/met_ensemble"
vers=".v1"
site.name= "BART"
site.lat = 44.063889
site.lon = -71.287375
timestep = 3

wd.base <- file.path("..", paste0("met_raw", vers))
path.dat <- file.path(wd.base, paste0(timestep, "hr"), site.name)
path.out <- file.path(wd.base, paste0(timestep, "hr_tdm_qaqc"), site.name)

dir.create(path.out, recursive=T, showWarnings = F)
# GCM.list <- c("bcc-csm1-1", "CCSM4", "MIROC-ESM", "MPI-ESM-P")
# GCM.list <- c("bcc-csm1-1", "CCSM4", "MIROC-ESM")
GCM.list <- dir(path.dat)
GCM.list <- GCM.list[!GCM.list %in% c("NLDAS", "figures_qaqc")]

# Cleaning up the names a little bit to make life easier
GCM.info <- gsub(".tdm", "", GCM.list) # Get rid of the tdm suffix
GCM.info <- gsub("r1i1p1", "", GCM.info) # Get rid of the run type for GFDL-CM3
GCM.info <- gsub("GFDL_CM3", "GFDL-CM3", GCM.info) # Make a consistent pattern match with other GCMS

mods.df <- data.frame(runID=GCM.list, matrix(unlist(strsplit(GCM.info, "_")), byrow = T, nrow=length(GCM.list)))
names(mods.df)[2:3] <- c("model", "scenario")


scen.all <- c("rcp45", "rcp85")

yrs.all <- 2006:2099
yrs.check <- c(2006, 2025, 2050, 2075, 2099)

doy.all <- 1:365

# yrs.check <- 2015
daywin = 7
# days.graph <- data.frame(winter=(45-(daywin-1)/2):(45+(daywin-1)/2),
#                          spring=(135-(daywin-1)/2):(135+(daywin-1)/2),
#                          summer=(225-(daywin-1)/2):(225+(daywin-1)/2),
#                          fall=(315-(daywin-1)/2):(315+(daywin-1)/2))

days.graph <- data.frame(season=rep(c("winter", "spring", "summer", "fall"), 
                                    each=length(daywin)),
                         yday=c((45-(daywin-1)/2):(45+(daywin-1)/2),
                                (135-(daywin-1)/2):(135+(daywin-1)/2),
                                (225-(daywin-1)/2):(225+(daywin-1)/2),
                                (315-(daywin-1)/2):(315+(daywin-1)/2)))


vars.CF <- c("air_temperature", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
vars.short <- c("tair", "precip", "swdown", "lwdown", "press", "qair", "wind")
# -----------------------------------

# -----------------------------------
# Extract the Met data
# - we're going to extract everything and store it in memory so 
#   we can compare at different levels
# - We want to double check on both the diurnal cycles as well as making sure we didn't break the day & year cycles
# -----------------------------------
mods.all <- unique(mods.df$model)
scen.all <- unique(mods.df$scenario)
all.yr <- data.frame(model=as.factor(rep(mods.all, each=length(yrs.all)*length(scen.all)*(length(vars.CF)+2))),
                     scenario=as.factor(rep(scen.all, each=length(yrs.all)*(length(vars.CF)))),
                     var=as.factor(rep(c("air_temperature_maximum", "air_temperature_minimum", vars.CF), each=length(yrs.all))),
                     year=rep(yrs.all, length.out=length(scen.all)*length(mods.all)*(length(vars.CF))*length(yrs.all)))
all.yr[,c("mean", "min", "max")] <- NA


all.day <- data.frame(model=as.factor(rep(mods.all, each=length(doy.all)*length(scen.all)*(length(vars.CF)+2))),
                      scenario=as.factor(rep(scen.all, each=length(doy.all)*(length(vars.CF)))),
                      var=as.factor(rep(c("air_temperature_maximum", "air_temperature_minimum", vars.CF), each=length(doy.all))),
                      yday=rep(1:365, length.out=length(scen.all)*length(mods.all)*(length(vars.CF)+2)*length(doy.all)))
all.day[,c("mean", "min", "max")] <- NA

all.hr <- data.frame(model=as.factor(rep(mods.all, each=24/timestep*nrow(days.graph)*length(scen.all)*length(vars.CF)*length(yrs.check))),
                     scenario=as.factor(rep(scen.all, each=24/timestep*nrow(days.graph)*length(vars.CF)*length(yrs.check))),
                     var=as.factor(rep(vars.CF, each=24/timestep*nrow(days.graph)*length(yrs.check))),
                     year=rep(yrs.check, each=24/timestep*nrow(days.graph)),
                     season=as.factor(rep(c("winter", "spring", "summer", "fall"), each=24/timestep*daywin)),
                     yday=rep(days.graph$yday, each=24/timestep),
                     hour=seq(timestep/2, 24, by=timestep))
all.hr$date <- as.POSIXct(paste(all.hr$year, all.hr$yday, all.hr$hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
all.hr$season <- factor(all.hr$season, levels=c("winter", "spring", "summer", "fall"))
# summary(all.hr$season)

summary(all.hr[all.hr$season=="fall",])
# all.day[,c("mean", "min", "max")] <- NA


for(i in 1:length(GCM.list)){
  print(paste0("Processing Model: ", GCM.list[i]))
  GCM=mods.df$model[i]
  SCEN=mods.df$scenario[i]

  # scenarios <- dir(file.path(path.dat, GCM))
  # for(SCEN in scenarios){
    # print(paste0("     Scenario: ", SCEN))
    
    # ens.now <- dir(file.path(path.dat, GCM.list[i]))
    
    mod.array <- array(dim=c(365, length(yrs.all), length(vars.CF)+2))
    dimnames(mod.array) <- list(day=1:365, year=yrs.all, var=c("air_temperature_minimum", "air_temperature_maximum", vars.CF))
    
    for(YR in yrs.all){
      # nday <- ifelse(lubridate::leap_year(yr), 366, 365)
      
      nc.now <- dir(file.path(path.dat, GCM.list[i]), paste(YR))
      if(length(nc.now)==0) next 
      
      ncT <- ncdf4::nc_open(file.path(path.dat, GCM.list[i], nc.now))
      time.nc <- ncdf4::ncvar_get(ncT, "time")
      
      dat.temp <- data.frame(GCM=GCM, scenario=SCEN, year = YR, yday = rep(1:365, each=24/timestep), hour=rep(seq(timestep/2, 24, by=timestep)))
      dat.temp$date <- strptime(paste(dat.temp$year, dat.temp$yday, dat.temp$hour, sep="-"), format=("%Y-%j-%H"), tz="UTC")
      
      for(VAR in vars.CF){
        if(VAR %in% names(ncT$var)){
          dat.temp[,VAR] <- ncdf4::ncvar_get(ncT, VAR, start=c(1,1,1), count=c(1,1,365*24/timestep))
        } else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
          ew <- ncdf4::ncvar_get(ncT, "eastward_wind", start=c(1,1,1), count=c(1,1,365*24/timestep))
          nw <- ncdf4::ncvar_get(ncT, "northward_wind", start=c(1,1,1), count=c(1,1,365*24/timestep))
          
          # Calculate wind speed from ew/nw using pythagorean theorem
          wnd <- sqrt(ew^2 + nw^2)
          
          dat.temp[,VAR] <- wnd 
        } else next
        
         
      }
      nc_close(ncT)
      
      
      # If a year we want to check hourly on, write the hourly data to the data frame
      if(YR %in% yrs.check){
        tmp.hr <- dat.temp[dat.temp$yday %in% days.graph$yday,]
        
        # Write to data frame
        for(VAR in vars.CF){
          ind.hr <- which(all.hr$model==GCM & all.hr$scenario==SCEN & all.hr$year==YR & all.hr$var==VAR )
          all.hr[ind.hr,"value"] <- tmp.hr[,VAR]
        }
        
        
      }
      
      # For all years, aggregate to daily & store in the model array so we can 
      # easily do the post-hoc calculations to compare to the input data
      # tmp.day <- data.frame(yday=1:365)
      tmp.day <- aggregate(dat.temp[,vars.CF],by=dat.temp[,c("year","yday")], FUN=mean)
      tmp.day$air_temperature_minimum <- aggregate(air_temperature ~ yday, data=dat.temp, FUN=min)$air_temperature
      tmp.day$air_temperature_maximum <- aggregate(air_temperature ~ yday, data=dat.temp, FUN=max)$air_temperature
      
      for(VAR in dimnames(mod.array)[[3]]){
        mod.array[,paste(YR), VAR] <- tmp.day[,VAR]
      }
      
    } # End yr loop
    mod.yr <- array(dim=c(dim(mod.array)[2:3], 3))
    mod.day <- array(dim=c(dim(mod.array)[c(1,3)], 3))
    
    dimnames(mod.yr)[[1]] <- dimnames(mod.array)[[2]]
    dimnames(mod.yr)[[2]] <- dimnames(mod.array)[[3]]
    dimnames(mod.yr)[[3]] <- c("mean", "min", "max")
    dimnames(mod.day)[[1]] <- dimnames(mod.array)[[1]]
    dimnames(mod.day)[[2]] <- dimnames(mod.array)[[3]]
    dimnames(mod.day)[[3]] <- c("mean", "min", "max")
    
    mod.yr[,,1] <- apply(mod.array, c(2,3), mean)
    mod.day[,,1] <- apply(mod.array, c(1,3), mean)
    mod.yr[,,2] <- apply(mod.array, c(2,3), min)
    mod.day[,,2] <- apply(mod.array, c(1,3), min)
    mod.yr[,,3] <- apply(mod.array, c(2,3), max)
    mod.day[,,3] <- apply(mod.array, c(1,3), max)
    
    # Merge at least th mean data into the data frame
    for(VAR in c("air_temperature_minimum", "air_temperature_maximum", vars.CF)){
      ind.yr <- which(all.yr$model==GCM & all.yr$scenario==SCEN & all.yr$var==VAR )
      all.yr$mean[ind.yr] <- mod.yr[,VAR,"mean"]
      all.yr$min[ind.yr] <- mod.yr[,VAR,"min"]
      all.yr$max[ind.yr] <- mod.yr[,VAR,"max"]
      
      ind.day <- which(all.day$model==GCM & all.day$scenario==SCEN & all.day$var==VAR)
      all.day$mean[ind.day] <- mod.day[,VAR,"mean"]
      all.day$min[ind.day] <- mod.day[,VAR,"min"]
      all.day$max[ind.day] <- mod.day[,VAR,"max"]
    }
  # } # End Scenario Loop
} # End GCM loop

summary(all.yr)
summary(all.day)
summary(all.hr)
summary(all.hr[is.na(all.hr$value),])

write.csv(all.yr, file.path(path.out, "CMIP5_TDM_year_byModel.csv"), row.names=F)
write.csv(all.day, file.path(path.out, "CMIP5_TDM_day_byModel.csv"), row.names=F)
# -----------------------------------


# -----------------------------------
# Graphing data
# -----------------------------------

pdf(file.path(path.out, "CMIP5_TDM_year_byModel.pdf"), height=11, width=8.5)
for(GCM in unique(all.yr$model)){
  print(
    ggplot(data=all.yr[all.yr$model==GCM,]) +
      ggtitle(GCM) +
      facet_wrap( ~ var, scales="free_y") +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=year, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=year, y=mean, color=scenario))
  )
}
dev.off()

pdf(file.path(path.out, "CMIP5_TDM_day_byModel.pdf"), height=11, width=8.5)
for(GCM in unique(all.day$model)){
  print(
    ggplot(data=all.day[all.day$model==GCM,]) +
      ggtitle(GCM) +
      facet_wrap( ~ var, scales="free_y") +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=yday, y=mean, color=scenario))
  )
}
dev.off()



pdf(file.path(path.out, "CMIP5_TDM_year_byVar.pdf"), height=11, width=8.5)
for(VAR in c("air_temperature_maximum", "air_temperature_minimum", vars.CF)){
  print(
    ggplot(data=all.yr[all.yr$var==VAR,]) +
      ggtitle(VAR) +
      facet_wrap( ~ model) +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=year, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=year, y=mean, color=scenario))
  )
}
dev.off()

pdf(file.path(path.out, "CMIP5_TDM_day_byVar.pdf"), height=11, width=8.5)
for(VAR in vars.CF){
  print(
    ggplot(data=all.day[all.day$var==VAR,]) +
      ggtitle(VAR) +
      facet_wrap( ~ model) +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=yday, y=mean, color=scenario))
  )
}
dev.off()


for(VAR in vars.CF){
  pdf(file.path(path.out, paste0("CMIP5_TDM_hour_byVar_",VAR, ".pdf")), height=8.5, width=11)
  for(YR in yrs.check){
    print(
      ggplot(data=all.hr[all.hr$year==YR & all.hr$var==VAR,]) +
        ggtitle(YR) +
        facet_grid( scenario ~ season, scales="free_x") +
        # facet_wrap( ~ season, scales="free") +
        # facet_grid(var ~ scenario, scales="free_y") +
        # geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=model, group=scenario), alpha=0.2)+
        geom_line(aes(x=date, y=value, color=model)) +
        scale_y_continuous(limits=range(all.hr[all.hr$year==YR & all.hr$var==VAR,"value"], na.rm=T))
    )
  }
  dev.off()
}
# -----------------------------------


# -----------------------------------
# Doing some extra checks on precip
# -----------------------------------
dat.precip <- all.yr[all.yr$var=="precipitation_flux",]
summary(dat.precip)

precip.gcm <- aggregate(mean ~ model + scenario, data=dat.precip, FUN="mean")
precip.gcm$mean.yr <- precip.gcm$mean*60*60*24*365

ggplot(data=dat.precip[dat.precip$scenario=="rcp45",])+
  facet_wrap(~model) +
  geom_histogram(aes(x=mean))

ggplot(data=precip.gcm[precip.gcm$scenario=="rcp45",]) +
  geom_histogram(aes(x=mean.yr)) +
  geom_vline(xintercept=precip.gcm$mean.yr[precip.gcm$model=="GFDL-CM3" & precip.gcm$scenario=="rcp45"], color="red")
# -----------------------------------
