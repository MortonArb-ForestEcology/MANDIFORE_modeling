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
# Aggregate to daily to make the debias go easier
# ---------------
library(ncdf4)
path.ldas <- file.path(path.out, site.name, "NLDAS")
files.train <- dir(path.ldas, ".nc")

outfolder <- file.path(path.out, site.name, "NLDAS_day")
dir.create(outfolder, recursive=T)

df.var <- data.frame(CF.name = c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", 
                                 "surface_downwelling_longwave_flux_in_air",
                                 "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                 "eastward_wind", "northward_wind", "wind_speed", "specific_humidity", "precipitation_flux"), 
                     units = c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "m/s", "g/g", "kg/m2/s"))

nc.info <- data.frame(CF.name = c("air_temperature_minimum", "air_temperature_maximum", "precipitation_flux", 
                                  "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", 
                                  "air_pressure", "specific_humidity", "wind_speed"), 
                      longname = c("2 meter minimum air temperature", "2 meter maximum air temperature", 
                                   "cumulative precipitation (water equivalent)", "incident (downwelling) showtwave radiation", 
                                   "incident (downwelling) longwave radiation", "air_pressureure at the surface", 
                                   "Specific humidity measured at the lowest level of the atmosphere", 
                                   "Wind speed"), 
                      units = c("K", "K", "kg m-2 s-1", "W m-2", "W m-2", "Pa", 
                                "kg kg-1", "m s-1"))


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
  
  loc.file <- file.path(outfolder, paste("NLDAS_day", stringr::str_pad(yr.now, width=4, side="left",  pad="0"), "nc", sep = "."))
  loc <- ncdf4::nc_create(filename = loc.file, vars = var.list, verbose = F)
  
  for (v in names(dat.day)) {
    ncdf4::ncvar_put(nc = loc, varid = as.character(v), vals = dat.day[[v]])
  }
  ncdf4::nc_close(loc)	
}
# ---------------


# -------------------------------


# -------------------------------
# Future met from MACA
# -------------------------------
source(file.path(path.pecan, "modules/data.atmosphere/R", "download.MACA.R"))
# ------------
# Using the default GCM
# ------------
download.MACA(outfolder=file.path(path.out, site.name, "MACA", "IPSL_CM5A-LR_rcp45"), 
              start_date="2095-01-01", end_date="2099-12-31", 
              site_id=site.name, lat.in=site.lat, lon.in=site.lon,
              model='IPSL-CM5A-LR', scenario='rcp45', ensemble_member='r1i1p1')

download.MACA(outfolder=file.path(path.out, site.name, "MACA", "IPSL_CM5A-LR_rcp85"), 
              start_date="2006-01-01", end_date="2099-12-31", 
              site_id=site.name, lat.in=site.lat, lon.in=site.lon, 
              model='IPSL-CM5A-LR', scenario='rcp85', ensemble_member='r1i1p1')
# ------------

# ------------
# Downloading CCSM4
# ------------
download.MACA(outfolder=file.path(path.out, site.name, "MACA", "CCSM4_rcp45"), 
              start_date="2006-01-01", end_date="2099-12-31", 
              site_id=site.name, lat.in=site.lat, lon.in=site.lon,
              model='CCSM4', scenario='rcp45', ensemble_member='r1i1p1')

download.MACA(outfolder=file.path(path.out, site.name, "MACA", "CCSM4_rcp85"), 
              start_date="2006-01-01", end_date="2099-12-31", 
              site_id=site.name, lat.in=site.lat, lon.in=site.lon, 
              model='CCSM4', scenario='rcp85', ensemble_member='r1i1p1')
# ------------
# -------------------------------
