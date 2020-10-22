# Do a quick check of how things look post-bias correction
# Plot the raw model outputs for assessment


site.name= "MortonArb"
vers=".v2"
site.lat = 41.82
site.lon = -88.04

path.out = file.path("..",paste0("met_raw", vers))

# Get a list of everything we have to work with
dir.mods <- file.path(path.out, "daily_bc", site.name)
mods.raw <- dir(dir.mods)
mods.raw <- mods.raw[!mods.raw %in% c("bias_correct_qaqc")]
# mods.raw <- mods.raw[1:(length(mods.raw)-1)]

mods.df <- data.frame(matrix(unlist(strsplit(mods.raw, "_")), byrow = T, nrow=length(mods.raw)))
names(mods.df) <- c("model", "scenario", "type")

yrs.all <- 2006:2099
vars.all <- c("air_temperature_maximum", "air_temperature_minimum", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
# nrs.nldas <- 1980:2019
scen.all <- c("rcp45", "rcp85")
doy.all <- 1:365
all.yr <- data.frame(model=as.factor(rep(unique(mods.df$model), each=length(yrs.all)*length(unique(mods.df$scenario))*length(vars.all))),
                     scenario=as.factor(rep(unique(mods.df$scenario), each=length(yrs.all)*length(vars.all))),
                     var=as.factor(rep(vars.all, each=length(yrs.all))),
                     year=rep(yrs.all, length.out=length(unique(mods.df$scenario))*length(unique(mods.df$model))*length(vars.all)*length(yrs.all)))
all.yr[,c("mean", "min", "max")] <- NA


all.day <- data.frame(model=as.factor(rep(unique(mods.df$model), each=length(doy.all)*length(scen.all)*length(vars.all))),
                      scenario=as.factor(rep(unique(mods.df$scenario), each=length(doy.all)*length(vars.all))),
                      var=as.factor(rep(vars.all, each=length(doy.all))),
                      yday=rep(doy.all, length.out=length(unique(mods.df$scenario))*length(unique(mods.df$model))*length(vars.all)*length(doy.all)))
all.day[,c("mean", "min", "max")] <- NA


for(i in 1:length(mods.raw)){
  print(paste0("Processing Model: ", mods.raw[i]))
  MOD=mods.df$model[i]
  SCEN=mods.df$scenario[i]
  # scenarios <- dir(file.path(dir.mods, MOD))
  # for(SCEN in scenarios){
    # print(paste0("     Scenario: ", SCEN))
    
    fmod <- dir(file.path(dir.mods, mods.raw[i]))
    # scen.list <- list()
    
    # making a 3-D array to help with aggregation
    mod.array <- array(dim=c(length(doy.all), length(yrs.all), length(vars.all)))
    dimnames(mod.array) <- list(day=doy.all, year=yrs.all, var=vars.all)
    
    for(YR in yrs.all){
      fnow <- fmod[grep(YR, fmod)]
      
      if(length(fnow)!=1) next
      
      ncT <- ncdf4::nc_open(file.path(dir.mods, mods.raw[i], fnow))
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
  # } # End scenario loop
} # End model loop

summary(all.yr)
summary(all.day)

library(ggplot2)
ggplot(data=all.yr[,]) +
  facet_grid(var ~ scenario, scales="free_y") +
  geom_ribbon(aes(x=year, ymin=min, ymax=max, fill=model), alpha=0.2)+
  geom_line(aes(x=year, y=mean, color=model))

ggplot(data=all.day) +
  facet_grid(var ~ scenario, scales="free_y") +
  geom_ribbon(aes(x=yday, ymin=min, ymax=max, fill=model), alpha=0.2)+
  geom_line(aes(x=yday, y=mean, color=model))


if(!dir.exists(file.path(path.out, "met_bc_qaqc"))) dir.create(file.path(path.out, "met_bc_qaqc"))

pdf(file.path(path.out, "met_bc_qaqc", "CMIP5_bc_year_byModel.pdf"), height=11, width=8.5)
for(MOD in unique(all.yr$model)){
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


pdf(file.path(path.out, "met_bc_qaqc", "CMIP5_bc_day_byModel.pdf"), height=11, width=8.5)
for(MOD in unique(all.day$model)){
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


pdf(file.path(path.out, "met_bc_qaqc", "CMIP5_bc_year_byVar.pdf"), height=11, width=8.5)
for(VAR in unique(all.yr$var)){
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

pdf(file.path(path.out, "met_bc_qaqc", "CMIP5_bc_day_byVar.pdf"), height=11, width=8.5)
for(VAR in unique(all.day$var)){
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
