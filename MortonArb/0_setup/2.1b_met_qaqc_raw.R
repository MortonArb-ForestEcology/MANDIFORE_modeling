# Plot the raw model outputs for assessment

path.out = "../met_raw"

site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

# Get a list of everything we have to work with
dir.mods <- file.path(path.out, "daily", site.name)
mods.raw <- dir(dir.mods)

yrs.all <- 2006:2100
vars.all <- c("air_temperature_maximum", "air_temperature_minimum", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
# nrs.nldas <- 1980:2019
scen.all <- c("rcp45", "rcp85")
doy.all <- 1:365
all.yr <- data.frame(model=rep(mods.raw, each=length(yrs.all)*length(scen.all)),
                     scenario=rep(scen.all, each=length(yrs.all)),
                     year=rep(yrs.all, length.out=length(scen.all)*length(mods.raw)))
all.yr[,vars.all] <- NA


all.day <- data.frame(model=rep(mods.raw, each=length(doy.all)*length(scen.all)),
                     scenario=rep(scen.all, each=length(doy.all)),
                     year=rep(doy.all, length.out=length(scen.all)*length(mods.raw)))
all.day[,vars.all] <- NA


for(MOD in mods.raw){
  scenarios <- dir(file.path(dir.mods, MOD))
  for(SCEN in scenarios){
    fmod <- dir(file.path(dir.mods, MOD, SCEN))
    # scen.list <- list()
    
    # making a 3-D array to help with aggregation
    mod.array <- array(dim=c(length(doy.all), length(fmod), length(vars.all)))
    dimnames(mod.array) <- list(day=doy.all, year=fmod, var=vars.all)

    for(i in 1:length(fmod)){
      fnow <- fmod[i]
      fsplit <- strsplit(fnow, "[.]")[[1]]
      dimnames(mod.array)[[2]][i] <- fsplit[4]
      
      ncT <- ncdf4::nc_open(file.path(dir.mods, MOD, SCEN, fnow))
      for(VAR in vars.all){
        if(!VAR %in% names(ncT$var)) next 
        # var.ind <- which(dimnames(mod.array)[[3]]==VAR)
        mod.array[, i, VAR] <- ncdf4::ncvar_get(ncT, VAR, start=c(1,1,1), count=c(1,1,max(doy.all)))
      } # end var loop
      ncdf4::nc_close(ncT)
    } # End file loop
    # Get yearly and daily means
    mod.yr <- apply(mod.array, c(2,3), mean)
    mod.day <- apply(mod.array, c(1,3), mean)
    
    # merge model data in to relevant data frame
    
  } # End scenario loop
} # End model loop
