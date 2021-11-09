#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script pulls hourly weather data and aggregates it to daily.
# Inputs: Met output found in MANDIFORE_modeling/MortonArb/met_raw.v3/subdaily_tdm/MortonArb
# Outputs: A dataframe of summaraized daily weather data that has been aggregated from hourly data
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
#Folder containing the data
path.out = "../met_raw.v3"

#Site identification
site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

# Get a list of everything we have to work with
dir.mods <- file.path(path.out, "subdaily_tdm", site.name)
mods.raw <- dir(dir.mods)

#Subsetting out GFDL-CM3_rcp45_r1i1p1 and GFDL-CM3_rcp85_r1i1p1 because they have a different format that is tricky
mods.raw <- mods.raw[c(1:12,15:32)]

#Setting the years and variables of interest
yrs.all <- 2006:2099
vars.all <- c("air_temperature", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")

#Creating the list that will be populated
mod.list <- list()
#mods.raw <- mods.raw[c(1)]
for(MOD in mods.raw){
  #Printing to make sure it's running
  print(paste0("Processing Model: ", MOD))
  
  fmod <- dir(file.path(dir.mods, MOD))
  
  for(YR in yrs.all){
    #Identifiying the proper file for this year
    fnow <- fmod[grep(YR, fmod)]
    
    #Making sure we have the year in question
    if(length(fnow)!=1) next
    
    ncT <- ncdf4::nc_open(file.path(dir.mods, MOD, fnow))
    sec <- ncdf4::ncvar_get(ncT, "time")
    ## convert time to seconds
    sec <- udunits2::ud.convert(sec, unlist(strsplit(ncT$dim$time$units, " "))[1], "seconds")

    #These documents uses a weird "Days since start" unit for hours giving values like (.0208, .0625) for hour 1 and 2
    #Converting from days since year start to hours since year start
    #h.time <- 0:(length(day.time)-1)
    
    #Making the final Datetime object
    final.time <- lubridate::seconds(sec) + as.POSIXct(paste0(YR, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
    
    #Beginning the loop for every variable
    for(VAR in vars.all){
      #Checking for the windspeed exception
      if(VAR %in% names(ncT$var)){
        #Building the list that contains the hourly values in nested lists for each model, year, and variable
        mod.list[[paste(MOD, YR, VAR)]]$value <- ncdf4::ncvar_get(ncT, VAR)
        #Rain.me <- ncdf4::ncvar_get(ncT, "precipitation_flux")
        mod.list[[paste(MOD, YR, VAR)]]$model <- MOD
        mod.list[[paste(MOD, YR, VAR)]]$var <- VAR
        mod.list[[paste(MOD, YR, VAR)]]$Date <- substr(final.time, 1, 10) 
        #mod.list[[paste(MOD, YR, VAR)]]$yday <- lubridate::yday(final.time)
        #mod.list[[paste(MOD, YR, VAR)]]$year <- lubridate::year(final.time)
        #mod.list[[paste(MOD, YR, VAR)]]$month <- lubridate::month(final.time)
      } 
      #Special case for windspeed
      
      else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
        ew <- ncdf4::ncvar_get(ncT, "eastward_wind")
        nw <- ncdf4::ncvar_get(ncT, "northward_wind")
        
        # Calculate wind speed from ew/nw using pythagorean theorem
        mod.list[[paste(MOD, YR, VAR)]]$value <- sqrt(ew^2 + nw^2)
        mod.list[[paste(MOD, YR, VAR)]]$model <- MOD
        mod.list[[paste(MOD, YR, VAR)]]$var <- VAR
        mod.list[[paste(MOD, YR, VAR)]]$Date<- substr(final.time, 1, 10) #This is to make aggregation easy later
        #mod.list[[paste(MOD, YR, VAR)]]$yday <- lubridate::yday(final.time)
        #mod.list[[paste(MOD, YR, VAR)]]$year <- lubridate::year(final.time)
        #mod.list[[paste(MOD, YR, VAR)]]$month <- lubridate::month(final.time)
      } 
    } # end var loop
    ncdf4::nc_close(ncT)
  } # End file loop
  
} 
# End model loop
#Make into a dataframe
mod.df <- dplyr::bind_rows(mod.list)

#Splitting the models and scenarios into two columns for easier cross-walking and analysis
mod.df$scenario <- ifelse(grepl("85", mod.df$model, fixed = TRUE), "rcp85", "rcp45")
mod.df$model <- car::recode(mod.df$model, "'ACCESS1-0_rcp45_bc.tdm'='ACCESS1-0'; 'ACCESS1-0_rcp85_bc.tdm'='ACCESS1-0'; 'bcc-csm1-1-m_rcp45_bc.tdm'='bcc-csm1-1-m';  
  'bcc-csm1-1-m_rcp85_bc.tdm'='bcc-csm1-1-m'; 'bcc-csm1-1_rcp45_bc.tdm'='bcc-csm1-1'; 'bcc-csm1-1_rcp85_bc.tdm'='bcc-csm1-1';    
  'BNU-ESM_rcp45_bc.tdm'='BNU-ESM'; 'BNU-ESM_rcp85_bc.tdm'='BNU-ESM'; 'CNRM-CM5_rcp45_bc.tdm'='CNRM-CM5';      
  'CNRM-CM5_rcp85_bc.tdm'='CNRM-CM5'; 'CSIRO-Mk3-6-0_rcp45_bc.tdm'='CSIRO-Mk3-6-0'; 'CSIRO-Mk3-6-0_rcp85_bc.tdm'='CSIRO-Mk3-6-0'; 
  'GFDL-ESM2G_rcp45_bc.tdm'='GFDL-ESM2G'; 'GFDL-ESM2G_rcp85_bc.tdm'='GFDL-ESM2G'; 'GFDL-ESM2M_rcp45_bc.tdm'='GFDL-ESM2M';    
  'GFDL-ESM2M_rcp85_bc.tdm'='GFDL-ESM2M'; 'inmcm4_rcp45_bc.tdm'='inmcm4'; 'inmcm4_rcp85_bc.tdm'='inmcm4';        
 'IPSL-CM5A-LR_rcp45_bc.tdm'='IPSL-CM5A-LR'; 'IPSL-CM5A-LR_rcp85_bc.tdm'='IPSL-CM5A-LR'; 'IPSL-CM5B-LR_rcp45_bc.tdm'='IPSL-CM5B-LR';  
  'IPSL-CM5B-LR_rcp85_bc.tdm'='IPSL-CM5B-LR'; 'MIROC-ESM-CHEM_rcp45_bc.tdm'='MIROC-ESM-CHEM'; 'MIROC-ESM-CHEM_rcp85_bc.tdm'='MIROC-ESM-CHEM';
  'MIROC-ESM_rcp45_bc.tdm'='MIROC-ESM'; 'MIROC-ESM_rcp85_bc.tdm'='MIROC-ESM'; 'MIROC5_rcp45_bc.tdm'='MIROC5';        
  'MIROC5_rcp85_bc.tdm'='MIROC5'; 'MRI-CGCM3_rcp45_bc.tdm'='MRI-CGCM3'; 'MRI-CGCM3_rcp85_bc.tdm'='MRI-CGCM3'")



#I've decided to split them by variable of interest here because the data gets too large otherwise

heat.df <- mod.df[mod.df$var == "air_temperature",]

#adding a flag for Heatwave temperature
heat.df$Heatwave <-  ifelse(heat.df$value > 313.15, 1, 0)

heat.df$value <- heat.df$value -273.15

#Taking the mean heat and the sum. THe sum is more useful for counting the number of hours with temp above the heatwave threshold
heat.agg <- aggregate(value~var+Date+model+scenario, data =heat.df, FUN = mean)
colnames(heat.agg) <- c("var", "Date", "model", "scenario", "mean")
heat.agg$sum <- aggregate(value~var+Date+model+scenario, data =heat.df, FUN = sum)[,c("value")]

#Saving the dataframe of temperature at daily resolution
write.csv(heat.agg, "../Temp_Weather_Daily.csv", row.names=F)


#Making a precipitation specific dataframe for conversion
precip.df <- mod.df[mod.df$var == "precipitation_flux",]

#Summing the precipitation converts from kg2/m2/hour to kg2/m2/day
#I am still summing for posterity but our workflow uses to mean to match with ED2 output
precip.agg <- aggregate(value~var+Date+model+scenario, data =precip.df, FUN = mean)
colnames(precip.agg) <- c("var", "Date", "model", "scenario", "mean")
precip.agg$sum <- aggregate(value~var+Date+model+scenario, data =precip.df, FUN = sum)[,c("value")]

write.csv(precip.agg, "../Precip_Weather_Daily.csv", row.names=F)

