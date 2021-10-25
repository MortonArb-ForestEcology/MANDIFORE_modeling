#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script pulls hourly weather data and aggregates it to daily.
# Inputs: Met output found in MANDIFORE_modeling/MortonArb/met_raw.v3/subdaily_tdm/MortonArb
# Outputs: A dataframe of summaraized daily weather data that has been aggregated from hourly data
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#



# Plot the raw model outputs for assessment

path.out = "../met_raw.v3"

site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

# Get a list of everything we have to work with
dir.mods <- file.path(path.out, "subdaily_tdm", site.name)
mods.raw <- dir(dir.mods)
mods.raw <- mods.raw[c(1:12,15:32)]

yrs.all <- 2006:2099
vars.all <- c("air_temperature", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
#Scenario is not included because the format of subdaily makes it easier to sepearte scenario at the end.
#scen.all <- c("rcp45", "rcp85")
doy.all <- 1:365
hr.all <- 1:24

all.day <- data.frame(model=as.factor(rep(mods.raw, each=length(yrs.all)*length(doy.all)*length(vars.all))),
                      var=as.factor(rep(vars.all, each=length(yrs.all)*length(doy.all))),
                      year=rep(yrs.all, each=length(doy.all)),
                      yday=rep(doy.all, length.out=length(mods.raw)*length(vars.all)*length(doy.all)*length(yrs.all)))
all.day[,c("mean", "min", "max")] <- NA
#all.day[,c("mean")] <- NA

#all.check <- all.day[all.day$yday == 365,]

for(MOD in mods.raw){
  print(paste0("Processing Model: ", MOD))
  #scenarios <- dir(file.path(dir.mods, MOD))
  #for(SCEN in scenarios){
    #print(paste0("     Scenario: ", SCEN))
    
    fmod <- dir(file.path(dir.mods, MOD))
    # scen.list <- list()
    
    # making a 3-D array to help with aggregation
    #This will be contatining hourly weather data that will then get scaled up to our all.day df
    mod.array <- array(dim=c(length(hr.all), length(doy.all), length(yrs.all), length(vars.all)))
    dimnames(mod.array) <- list(hour = hr.all, day=doy.all, year=yrs.all, var=vars.all)
    
    for(YR in yrs.all){
      fnow <- fmod[grep(YR, fmod)]
      
      if(length(fnow)!=1) next
      
      ncT <- ncdf4::nc_open(file.path(dir.mods, MOD, fnow))
      check <- ncdf4::ncvar_get(ncT, "time")
      for(DAY in doy.all){
        for(VAR in vars.all){
          if(VAR %in% names(ncT$var)){
            mod.array[,paste(DAY),paste(YR), VAR] <- ncdf4::ncvar_get(ncT, VAR, start=c(1,1, (24*DAY)-23), count=c(1,1,max(hr.all)))
          } else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
            ew <- ncdf4::ncvar_get(ncT, "eastward_wind", start=c(1,1, (24*DAY)-23), count=c(1,1,max(hr.all)))
            nw <- ncdf4::ncvar_get(ncT, "northward_wind", start=c(1,1, (24*DAY)-23), count=c(1,1,max(hr.all)))
            
            # Calculate wind speed from ew/nw using pythagorean theorem
            mod.array[,paste(DAY), paste(YR), "wind_speed"] <- sqrt(ew^2 + nw^2)
            
          } else next
          
          # if(!VAR %in% names(ncT$var)) next 
          # var.ind <- which(dimnames(mod.array)[[3]]==VAR)
          
        } # end var loop
      }
      ncdf4::nc_close(ncT)
    } # End file loop
    # Get yearly and daily means
    
    #Using 2,3, and 4 because those are day, year, and VAR
    mod.day <- array(dim=c(dim(mod.array)[c(2,3,4)], 3))
    
    dimnames(mod.day)[[1]] <- dimnames(mod.array)[[2]]
    dimnames(mod.day)[[2]] <- dimnames(mod.array)[[3]]
    dimnames(mod.day)[[3]] <- dimnames(mod.array)[[4]]
    dimnames(mod.day)[[4]] <- c("mean", "min", "max")
    
    mod.day[,,,1] <- apply(mod.array, c(2,3,4), mean)
    mod.day[,,,2] <- apply(mod.array, c(2,3,4), min)
    mod.day[,,,3] <- apply(mod.array, c(2,3,4), max)
    
    # Merge at least th mean data into the data frame
    for(VAR in vars.all){
      for(YEAR in yrs.all){
      ind.day <- which(all.day$model==MOD & all.day$var==VAR & all.day$year == YEAR)
      all.day$mean[ind.day] <- mod.day[ , as.character(YEAR), VAR, "mean"]
      all.day$min[ind.day] <- mod.day[ , as.character(YEAR), VAR, "min"]
      all.day$max[ind.day] <- mod.day[ , as.character(YEAR), VAR, "max"]
      }
    }
} 
# End model loop
write.csv(all.day, "../Aggregate_Weater_Daily.csv", row.names=F)

summary(all.day)

summary(all.day[is.na(all.day$mean),])
summary(all.day[is.na(all.day$mean),])

summary(all.day[is.na(all.day$mean) & all.day$model=="bcc-csm1-1",])

# Creating a list of missing data
yrs.bad <- aggregate(year ~ model + scenario + var, data=all.yr[is.na(all.yr$mean),], FUN=min)
names(yrs.bad)[names(yrs.bad)=="year"] <- "yr.min"
yrs.bad$yr.max <- aggregate(year ~ model + scenario + var, data=all.yr[is.na(all.yr$mean),], FUN=max)$year
yrs.bad <- yrs.bad[order(yrs.bad$model, yrs.bad$scenario, yrs.bad$var),]
yrs.bad
yrs.bad[yrs.bad$scenario=="rcp45",]

write.csv(all.day, "../Aggregate_Weater_Daily.csv", row.names=F)

library(ggplot2)
ggplot(data=all.yr[,]) +
  facet_grid(var ~ scenario, scales="free_y") +
  geom_ribbon(aes(x=year, ymin=min, ymax=max, fill=model), alpha=0.2)+
  geom_line(aes(x=year, y=mean, color=model))

ggplot(data=all.day) +
  facet_grid(var ~ scenario, scales="free_y") +
  geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=model), alpha=0.2)+
  geom_line(aes(x=yday, y=mean, color=model))


if(!dir.exists(file.path(path.out, "met_raw_qaqc"))) dir.create(file.path(path.out, "met_raw_qaqc"))

pdf(file.path(path.out, "met_raw_qaqc", "CMIP5_raw_year_byModel.pdf"), height=11, width=8.5)
for(MOD in mods.raw){
  print(
    ggplot(data=all.yr[all.yr$model==MOD,]) +
      ggtitle(MOD) +
      facet_wrap( ~ var, scales="free_y") +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=year, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=year, y=mean, color=scenario))
  )
}
dev.off()


pdf(file.path(path.out, "met_raw_qaqc", "CMIP5_raw_day_byModel.pdf"), height=11, width=8.5)
for(MOD in mods.raw){
  print(
    ggplot(data=all.day[all.day$model==MOD,]) +
      ggtitle(MOD) +
      facet_wrap( ~ var, scales="free_y") +
      # facet_grid(var ~ scenario, scales="free_y") +
      geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=scenario), alpha=0.2)+
      geom_line(aes(x=yday, y=mean, color=scenario))
  )
}
dev.off()



pdf(file.path(path.out, "met_raw_qaqc", "CMIP5_raw_year_byVar.pdf"), height=11, width=8.5)
for(VAR in vars.all){
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

pdf(file.path(path.out, "met_raw_qaqc", "CMIP5_raw_day_byVar.pdf"), height=11, width=8.5)
for(VAR in vars.all){
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

