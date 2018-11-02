# Download/Extract Met Products
path.pecan <- "~/Desktop/Research/pecan/"

# Site Metadata
site.name = "WILLOWCREEK"
site.lat  =  45.805822 # 45°48′21″N
site.lon  = -90.079722 # 90°04′47″W
path.out = "../met_raw"

# -------------------------------
# Empirical met from NLDAS
# -------------------------------
# Source Pecan Extraction functions
source(file.path(path.pecan, "modules/data.atmosphere/R", "extract_local_NLDAS.R"))
ldas.type = "NLDAS"
path.nldas = "/Volumes/Celtis/Meteorology/LDAS/NLDAS_FORA0125_H.002/netcdf/"
extract.local.NLDAS(outfolder=file.path(path.out, site.name, "NLDAS"), in.path=path.nldas, 
                    start_date="1999-01-01", end_date="2016-12-31", 
                    site_id=site.name, lat.in=site.lat, lon.in=site.lon)


# ---------------
# Aggregate to 3-hourly and daily to make the debias go easier
# ---------------
library(ncdf4)
path.ldas <- file.path(path.out, site.name, "NLDAS")
files.train <- dir(path.ldas, ".nc")

outfolder.3hr <- file.path(path.out, site.name, "NLDAS_3hr")
dir.create(outfolder.3hr, recursive=T)

outfolder.day <- file.path(path.out, site.name, "NLDAS_day")
dir.create(outfolder.day, recursive=T)

df.var <- data.frame(CF.name = c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", 
                                 "surface_downwelling_longwave_flux_in_air",
                                 "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                 "eastward_wind", "northward_wind", "wind_speed", "specific_humidity", "precipitation_flux"), 
                     units = c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "m/s", "g/g", "kg/m2/s"))

nc.info <- data.frame(CF.name = c("air_temperature", "air_temperature_minimum", "air_temperature_maximum", "precipitation_flux", 
                                  "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", 
                                  "air_pressure", "specific_humidity", "wind_speed"), 
                      longname = c("2 meter air temperature", "2 meter minimum air temperature", "2 meter maximum air temperature", 
                                   "cumulative precipitation (water equivalent)", "incident (downwelling) showtwave radiation", 
                                   "incident (downwelling) longwave radiation", "air_pressureure at the surface", 
                                   "Specific humidity measured at the lowest level of the atmosphere", 
                                   "Wind speed"), 
                      units = c("K", "K", "K", "kg m-2 s-1", "W m-2", "W m-2", "Pa", 
                                "kg kg-1", "m s-1"))
# Aggregate to 3-hourly
for(i in 1:length(files.train)){
  # Figure out what year we're working with
  yr.now <- as.numeric(strsplit(files.train[i], "[.]")[[1]][2])
  nday <- ifelse(lubridate::leap_year(yr.now), 366, 365)
  
  dat.day <- list()
  
  # Open the file so we can query from it
  ncT <- ncdf4::nc_open(file.path(path.ldas, files.train[i]))
  
  # Extract som plot dimensions
  lat.nc <- ncdf4::ncvar_get(ncT, "latitude")
  lon.nc <- ncdf4::ncvar_get(ncT, "longitude")
  
  time.nc <- ncdf4::ncvar_get(ncT, "time") 
  time.df <- data.frame(time.nc=time.nc, time.grp = rep(rep(1:(length(time.nc)/3), each=3), length.out=length(time.nc)))
  time.agg <- aggregate(time.df$time.nc, by=list(time.df$time.grp), FUN=mean)

  # Extract plot info & aggregate to daily resolution
  for(v in names(ncT$var)){
    dat.hr <- matrix(ncdf4::ncvar_get(ncT, v), ncol=nrow(time.agg))
    if(v %in% c("eastward_wind", "northward_wind")) {
      wind.e <- matrix(ncdf4::ncvar_get(ncT, "eastward_wind"), ncol=nrow(time.agg))
      wind.n <- matrix(ncdf4::ncvar_get(ncT, "northward_wind"), ncol=nrow(time.agg))
      wind <- sqrt(wind.e^2 + wind.n^2)
      dat.day[["wind_speed"]] <- apply(wind, 2, mean)
    } else {
      dat.day[[v]] <- apply(dat.hr, 2, mean)
    }
  }
  
  # Create a daily .nc file for each year
  dim.lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat.nc, create_dimvar=TRUE)
  dim.lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon.nc, create_dimvar=TRUE)
  dim.time <- ncdf4::ncdim_def(name='time', units="sec", vals=time.agg$x, create_dimvar=TRUE, unlim=TRUE)
  nc.dim=list(dim.lat,dim.lon,dim.time)
  
  var.list = list()
  for(v in names(dat.day)){
    var.list[[v]] = ncdf4::ncvar_def(name=v, units=as.character(nc.info[nc.info$CF.name==v, "units"]), dim=nc.dim, missval=-999, verbose=F)
  }
  
  loc.file <- file.path(outfolder.3hr, paste("NLDAS_3hr", stringr::str_pad(yr.now, width=4, side="left",  pad="0"), "nc", sep = "."))
  loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = F)
  
  for (v in names(dat.day)) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(v), vals = dat.day[[v]])
  }
  ncdf4::nc_close(loc)	
}

# Aggregate to daily
for(i in 1:length(files.train)){
  # Figure out what year we're working with
  yr.now <- as.numeric(strsplit(files.train[i], "[.]")[[1]][2])
  nday <- ifelse(lubridate::leap_year(yr.now), 366, 365)
  
  dat.day <- list()
  
  # Open the file so we can query from it
  ncT <- ncdf4::nc_open(file.path(path.ldas, files.train[i]))
  
  # Extract som plot dimensions
  lat.nc <- ncdf4::ncvar_get(ncT, "latitude")
  lon.nc <- ncdf4::ncvar_get(ncT, "longitude")
  
  time.nc <- ncdf4::ncvar_get(ncT, "time") 
  time.day <- apply(matrix(time.nc, ncol=nday), 2, mean) # get the daily time stamps
  
  # Extract plot info & aggregate to daily resolution
  for(v in names(ncT$var)){
    dat.hr <- matrix(ncdf4::ncvar_get(ncT, v), ncol=nday)
    if(v == "air_temperature"){
      dat.day[["air_temperature_minimum"]] <- apply(dat.hr, 2, min)
      dat.day[["air_temperature_maximum"]] <- apply(dat.hr, 2, max)
    } else if(v %in% c("eastward_wind", "northward_wind")) {
      wind.e <- matrix(ncdf4::ncvar_get(ncT, "eastward_wind"), ncol=nday)
      wind.n <- matrix(ncdf4::ncvar_get(ncT, "northward_wind"), ncol=nday)
      wind <- sqrt(wind.e^2 + wind.n^2)
      dat.day[["wind_speed"]] <- apply(wind, 2, mean)
    } else {
      dat.day[[v]] <- apply(dat.hr, 2, mean)
    }
  }
  
  # Create a daily .nc file for each year
  dim.lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat.nc, create_dimvar=TRUE)
  dim.lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon.nc, create_dimvar=TRUE)
  dim.time <- ncdf4::ncdim_def(name='time', units="sec", vals=time.day, create_dimvar=TRUE, unlim=TRUE)
  nc.dim=list(dim.lat,dim.lon,dim.time)
  
  var.list = list()
  for(v in names(dat.day)){
    var.list[[v]] = ncdf4::ncvar_def(name=v, units=as.character(nc.info[nc.info$CF.name==v, "units"]), dim=nc.dim, missval=-999, verbose=F)
  }
  
  loc.file <- file.path(outfolder.day, paste("NLDAS_day", stringr::str_pad(yr.now, width=4, side="left",  pad="0"), "nc", sep = "."))
  loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = F)
  
  for (v in names(dat.day)) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(v), vals = dat.day[[v]])
  }
  ncdf4::nc_close(loc)	
}
# ---------------


# -------------------------------


# -------------------------------
# Future met from GFDL
# -------------------------------
source(file.path(path.pecan, "modules/data.atmosphere/R", "download.GFDL.R"))
# ------------
# Using the default GCM
# ------------
# ------------
download.GFDL(outfolder=file.path(path.out, site.name), 
              start_date="2006-01-01", end_date="2100-12-31", 
              lat.in=site.lat, lon.in=site.lon,
              overwrite = FALSE, verbose = FALSE,
              model = "CM3", scenario = "rcp45", ensemble_member = "r1i1p1")

download.GFDL(outfolder=file.path(path.out, site.name), 
              start_date="2006-01-01", end_date="2100-12-31", 
              lat.in=site.lat, lon.in=site.lon,
              overwrite = FALSE, verbose = FALSE,
              model = "CM3", scenario = "rcp85", ensemble_member = "r1i1p1")
# -------------------------------
