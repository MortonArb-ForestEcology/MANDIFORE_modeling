#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script pulls hourly weather data and aggregates it to daily.
# Inputs: Met output found in MANDIFORE_modeling/MortonArb/met_raw.v3/subdaily_tdm/MortonArb
# Outputs: A dataframe of summaraized daily weather data that has been aggregated from hourly data
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
#Folder containing the data
path.out = "../../../../crollinson/MANDIFORE_modeling/MortonArb/met_raw.v3"
install.packages("ncdf4")

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
#mods.raw <- mods.raw[1:3]
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
    day.time <- ncdf4::ncvar_get(ncT, "time")
    
    #These documents uses a weird "Days since start" unit for hours giving values like (.0208, .0625) for hour 1 and 2
    #Converting from days since year start to hours since year start
    h.time <- 1:length(day.time)
    
    #Making the final Datetime object
    final.time <- lubridate::hours(h.time) + as.POSIXct(paste0(YR, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
    
    #Beginning the loop for every variable
    for(VAR in vars.all){
      #Checking for the windspeed exception
      if(VAR %in% names(ncT$var)){
        
        #Building the list that contains the hourly values in nested lists for each model, year, and variable
        mod.list[[paste(MOD, YR, VAR)]]$value <- ncdf4::ncvar_get(ncT, VAR)
        mod.list[[paste(MOD, YR, VAR)]]$model <- MOD
        mod.list[[paste(MOD, YR, VAR)]]$var <- VAR
        mod.list[[paste(MOD, YR, VAR)]]$Date <- final.time 
      } 
      #Special case for windspeed
      
      else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
        ew <- ncdf4::ncvar_get(ncT, "eastward_wind")
        nw <- ncdf4::ncvar_get(ncT, "northward_wind")
        
        # Calculate wind speed from ew/nw using pythagorean theorem
        mod.list[[paste(MOD, YR, VAR)]]$value <- sqrt(ew^2 + nw^2)
        mod.list[[paste(MOD, YR, VAR)]]$model <- MOD
        mod.list[[paste(MOD, YR, VAR)]]$var <- VAR
        mod.list[[paste(MOD, YR, VAR)]]$Date<- final.time
      } 
    } # end var loop
    ncdf4::nc_close(ncT)
  } # End file loop
  
} 
# End model loop
#Make into a dataframe
mod.df <- dplyr::bind_rows(mod.list)

#HERE is where I convert kg2/m2/sec to kg2/m2/hour
mod.df$value <- ifelse(mod.df$var == "precipitation_flux", mod.df$value * 60 * 60, mod.df$value)

#Creating a date object that is Daily (Removing hours)
mod.df$Date <- as.Date(mod.df$Date)

#adding a flag for Heatwave temperature
mod.df$Heatwave <- ifelse(mod.df$var == "air_temperature" & mod.df$value > 313.15, 1, 0)

#Aggregating for each day
day.df <- aggregate(value~var+Date+model, data =mod.df, FUN = mean)

#I've removed these for now due to time and memory restrictions but might bring them back if they seem useful

#day.df$min <- aggregate(value~var+Date+model, data =mod.df, FUN = min)[,c("value")]
#day.df$max <- aggregate(value~var+Date+model, data =mod.df, FUN = max)[,c("value")]

#Splitting the models and scenarios into two columns for easier cross-walking and analysis
#day.df$scenario <- ifelse(grepl("85", day.df$model, fixed = TRUE), "rcp85", "rcp45")
#day.df$model <- car::recode(day.df$model, "'ACCESS1-0_rcp45_bc.tdm'='ACCESS1-0'; 'ACCESS1-0_rcp85_bc.tdm'='ACCESS1-0'; 'bcc-csm1-1-m_rcp45_bc.tdm'='bcc-csm1-1-m';  
#  'bcc-csm1-1-m_rcp85_bc.tdm'='bcc-csm1-1-m'; 'bcc-csm1-1_rcp45_bc.tdm'='bcc-csm1-1'; 'bcc-csm1-1_rcp85_bc.tdm'='bcc-csm1-1';    
#  'BNU-ESM_rcp45_bc.tdm'='BNU-ESM'; 'BNU-ESM_rcp85_bc.tdm'='BNU-ESM'; 'CNRM-CM5_rcp45_bc.tdm'='CNRM-CM5';      
#  'CNRM-CM5_rcp85_bc.tdm'='CNRM-CM5'; 'CSIRO-Mk3-6-0_rcp45_bc.tdm'='CSIRO-Mk3-6-0'; 'CSIRO-Mk3-6-0_rcp85_bc.tdm'='CSIRO-Mk3-6-0'; 
#  'GFDL-ESM2G_rcp45_bc.tdm'='GFDL-ESM2G'; 'GFDL-ESM2G_rcp85_bc.tdm'='GFDL-ESM2G'; 'GFDL-ESM2M_rcp45_bc.tdm'='GFDL-ESM2M';    
#  'GFDL-ESM2M_rcp85_bc.tdm'='GFDL-ESM2M'; 'inmcm4_rcp45_bc.tdm'='inmcm4'; 'inmcm4_rcp85_bc.tdm'='inmcm4';        
# 'IPSL-CM5A-LR_rcp45_bc.tdm'='IPSL-CM5A-LR'; 'IPSL-CM5A-LR_rcp85_bc.tdm'='IPSL-CM5A-LR'; 'IPSL-CM5B-LR_rcp45_bc.tdm'='IPSL-CM5B-LR';  
#  'IPSL-CM5B-LR_rcp85_bc.tdm'='IPSL-CM5B-LR'; 'MIROC-ESM-CHEM_rcp45_bc.tdm'='MIROC-ESM-CHEM'; 'MIROC-ESM-CHEM_rcp85_bc.tdm'='MIROC-ESM-CHEM';
#  'MIROC-ESM_rcp45_bc.tdm'='MIROC-ESM'; 'MIROC-ESM_rcp85_bc.tdm'='MIROC-ESM'; 'MIROC5_rcp45_bc.tdm'='MIROC5';        
#  'MIROC5_rcp85_bc.tdm'='MIROC5'; 'MRI-CGCM3_rcp45_bc.tdm'='MRI-CGCM3'; 'MRI-CGCM3_rcp85_bc.tdm'='MRI-CGCM3'")

day.df$sum <- aggregate(value~var+Date+model, data =mod.df, FUN = sum)[,c("value")]
#Saving the dataframe of every variable together at daily resolution
write.csv(day.df, "../Aggregate_Weater_Daily.csv", row.names=F)

#Making a precipitation specific dataframe for conversion
#dat.precip <- day.df[day.df$var == "precipitation_flux",]
#Summing the precipitation converts from kg2/m2/hour to kg2/m2/day



#dat.temp <- day.df[day.df$var == "air_temperature",]
#For each day I sum the total number of hours the temperature went above th photsynthetic threshold
#dat.temp$Heatwave <- aggregate(Heatwave~var+Date+model, data = mod.df, FUN = sum)[,c("Heatwave")]
#colnames(day.df) <- c("var", "Date", "model", "mean", "min", "max", "sum")







