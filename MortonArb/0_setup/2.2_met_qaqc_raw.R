# Plot the raw model outputs for assessment

path.out = "../met_raw.v3"

site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

# Get a list of everything we have to work with
dir.mods <- file.path(path.out, "daily", site.name)
mods.raw <- dir(dir.mods)
# mods.raw <- mods.raw[1:(length(mods.raw)-1)]

yrs.all <- 2006:2099
vars.all <- c("air_temperature_maximum", "air_temperature_minimum", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
# nrs.nldas <- 1980:2019
scen.all <- c("rcp45", "rcp85")
doy.all <- 1:365
all.yr <- data.frame(model=as.factor(rep(mods.raw, each=length(yrs.all)*length(scen.all)*length(vars.all))),
                     scenario=as.factor(rep(scen.all, each=length(yrs.all)*length(vars.all))),
                     var=as.factor(rep(vars.all, each=length(yrs.all))),
                     year=rep(yrs.all, length.out=length(scen.all)*length(mods.raw)*length(vars.all)*length(yrs.all)))
all.yr[,c("mean", "min", "max")] <- NA


all.day <- data.frame(model=as.factor(rep(mods.raw, each=length(doy.all)*length(scen.all)*length(vars.all))),
                     scenario=as.factor(rep(scen.all, each=length(doy.all)*length(vars.all))),
                     var=as.factor(rep(vars.all, each=length(doy.all))),
                     yday=rep(doy.all, length.out=length(scen.all)*length(mods.raw)*length(vars.all)*length(doy.all)))
all.day[,c("mean", "min", "max")] <- NA


for(MOD in mods.raw){
  print(paste0("Processing Model: ", MOD))
  scenarios <- dir(file.path(dir.mods, MOD))
  for(SCEN in scenarios){
    print(paste0("     Scenario: ", SCEN))
    
    fmod <- dir(file.path(dir.mods, MOD, SCEN))
    # scen.list <- list()
    
    # making a 3-D array to help with aggregation
    mod.array <- array(dim=c(length(doy.all), length(yrs.all), length(vars.all)))
    dimnames(mod.array) <- list(day=doy.all, year=yrs.all, var=vars.all)

    for(YR in yrs.all){
      fnow <- fmod[grep(YR, fmod)]
      
      if(length(fnow)!=1) next

      ncT <- ncdf4::nc_open(file.path(dir.mods, MOD, SCEN, fnow))
      for(VAR in vars.all){
        if(VAR %in% names(ncT$var)){
          mod.array[,paste(YR), VAR] <- ncdf4::ncvar_get(ncT, VAR, start=c(1,1,1), count=c(1,1,max(doy.all)))
        } else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
          ew <- ncdf4::ncvar_get(ncT, "eastward_wind", start=c(1,1,1), count=c(1,1,max(doy.all)))
          nw <- ncdf4::ncvar_get(ncT, "northward_wind", start=c(1,1,1), count=c(1,1,max(doy.all)))
        
          # Calculate wind speed from ew/nw using pythagorean theorem
          mod.array[,paste(YR), "wind_speed"] <- sqrt(ew^2 + nw^2)
          
        } else next
        
        # if(!VAR %in% names(ncT$var)) next 
        # var.ind <- which(dimnames(mod.array)[[3]]==VAR)
        
      } # end var loop
      ncdf4::nc_close(ncT)
    } # End file loop
    # Get yearly and daily means
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
    for(VAR in vars.all){
      ind.yr <- which(all.yr$model==MOD & all.yr$scenario==SCEN & all.yr$var==VAR )
      all.yr$mean[ind.yr] <- mod.yr[,VAR,"mean"]
      all.yr$min[ind.yr] <- mod.yr[,VAR,"min"]
      all.yr$max[ind.yr] <- mod.yr[,VAR,"max"]
      
      ind.day <- which(all.day$model==MOD & all.day$scenario==SCEN & all.day$var==VAR)
      all.day$mean[ind.day] <- mod.day[,VAR,"mean"]
      all.day$min[ind.day] <- mod.day[,VAR,"min"]
      all.day$max[ind.day] <- mod.day[,VAR,"max"]
    }
  } # End scenario loop
} # End model loop

summary(all.yr)
summary(all.day)

summary(all.yr[is.na(all.yr$mean),])
summary(all.day[is.na(all.day$mean),])
summary(all.day[is.na(all.day$mean),])

summary(all.yr[is.na(all.yr$mean) & all.yr$model=="ACCESS1-3",])
summary(all.day[is.na(all.day$mean) & all.day$model=="bcc-csm1-1",])
summary(all.yr[is.na(all.yr$mean) & all.yr$model=="HadGEM2-CC",])
summary(all.yr[is.na(all.yr$mean) & all.yr$model=="HadGEM2-ES",])

# Creating a list of missing data
yrs.bad <- aggregate(year ~ model + scenario + var, data=all.yr[is.na(all.yr$mean),], FUN=min)
names(yrs.bad)[names(yrs.bad)=="year"] <- "yr.min"
yrs.bad$yr.max <- aggregate(year ~ model + scenario + var, data=all.yr[is.na(all.yr$mean),], FUN=max)$year
yrs.bad <- yrs.bad[order(yrs.bad$model, yrs.bad$scenario, yrs.bad$var),]
yrs.bad
yrs.bad[yrs.bad$scenario=="rcp45",]

write.csv(yrs.bad, "../met_problems.csv", row.names=F)

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
