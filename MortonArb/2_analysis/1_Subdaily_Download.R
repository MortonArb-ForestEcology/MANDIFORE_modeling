#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script pulls hourly weather data and aggregates it to daily.
# Inputs: Met output found in MANDIFORE_modeling/MortonArb/met_raw.v3/subdaily_tdm/MortonArb
# Outputs: A dataframe of summarized daily weather data that has been aggregated from hourly data
#          A dataframe containing all of the different gcm ED2 runs into one csv
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------#
#Reading in the Mandifore data and combined it into 1 file
#-------------------------------------------------#
path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"
#path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

runs.load <- readbulk::read_bulk(directory = file.path(path.google, "output/"), extension = ".csv")
write.csv(runs.load, file.path(path.google, "processed_data/All_runs.csv"), row.names = F)

#---------------------------------------------------#
#Getting the weather data
#---------------------------------------------------#

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
# mods.raw <- mods.raw[c(1:12,15:32)] # Hard coding is BAD

#Setting the years and variables of interest
yrs.all <- 2006:2099
vars.all <- c("air_temperature", "precipitation_flux", "surface_downwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")

# Setting up a function that can easily be passed to our different paths
met.pull <- function(ncT, VAR){
  mod.out <- list()
  if(VAR %in% names(ncT$var)){
    #Building the list that contains the hourly values in nested lists for each model, year, and variable
    mod.out$value <- ncdf4::ncvar_get(ncT, VAR)
    #Rain.me <- ncdf4::ncvar_get(ncT, "precipitation_flux")
    mod.out$model <- MOD
    mod.out$var <- VAR
    mod.out$Date <- substr(final.time, 1, 10) 
    #mod.out$yday <- lubridate::yday(final.time)
    #mod.out$year <- lubridate::year(final.time)
    #mod.out$month <- lubridate::month(final.time)
  } 
  #Special case for windspeed
  
  else if(VAR=="wind_speed" & "eastward_wind" %in% names(ncT$var)){
    ew <- ncdf4::ncvar_get(ncT, "eastward_wind")
    nw <- ncdf4::ncvar_get(ncT, "northward_wind")
    
    # Calculate wind speed from ew/nw using pythagorean theorem
    mod.out$value <- sqrt(ew^2 + nw^2)
    mod.out$model <- MOD
    mod.out$var <- VAR
    mod.out$Date<- substr(final.time, 1, 10) #This is to make aggregation easy later
    #mod.out$yday <- lubridate::yday(final.time)
    #mod.out$year <- lubridate::year(final.time)
    #mod.out$month <- lubridate::month(final.time)
  } 
  
  return(mod.out)
}


#Creating the list that will be populated
mod.list <- list()
#mods.raw <- mods.raw[c(1)]
for(MOD in mods.raw){
  #Printing to make sure it's running
  print(paste0("Processing Model: ", MOD))
  
  mod.name <- strsplit(MOD, "_")[[1]][1]
  if(mod.name=="GFDL") mod.name <- "GFDL-CM3"
  
  rcp <- ifelse(grepl("45", MOD), "rcp45", "rcp85")
  
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

    #Making the final Datetime object
    final.time <- lubridate::seconds(sec) + as.POSIXct(paste0(YR, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
    
    #Beginning the loop for every variable
    for(VAR in vars.all){
      #Checking for the windspeed exception
      mod.list[[paste(MOD, YR, VAR)]] <- met.pull(ncT, VAR)
      mod.list[[paste(MOD, YR, VAR)]]$model <- mod.name
      mod.list[[paste(MOD, YR, VAR)]]$sceario <- rcp
    } # end var loop
    ncdf4::nc_close(ncT)
  } # End file loop
} # End model loop


# Adding back in GFDL-CM3
dir.mods2 <- file.path(path.out, "subdaily", site.name)
mods.raw2 <- dir(dir.mods2)
mods.raw2 <- mods.raw2[!mods.raw2 %in% c("NLDAS")]

for(MOD in mods.raw2){
  #Printing to make sure it's running
  print(paste0("Processing Model: ", MOD))
  
  mod.name <- strsplit(MOD, "_")[[1]][1]
  if(mod.name=="GFDL") mod.name <- "GFDL-CM3"
  
  fmod <- dir(file.path(dir.mods2, MOD))
  
  for(YR in yrs.all){
    #Identifiying the proper file for this year
    fnow <- fmod[grep(YR, fmod)]
    
    #Making sure we have the year in question
    if(length(fnow)!=1) next
    
    ncT <- ncdf4::nc_open(file.path(dir.mods2, MOD, fnow))
    sec <- ncdf4::ncvar_get(ncT, "time")
    ## convert time to seconds
    sec <- udunits2::ud.convert(sec, unlist(strsplit(ncT$dim$time$units, " "))[1], "seconds")
    
    #Making the final Datetime object
    final.time <- lubridate::seconds(sec) + as.POSIXct(paste0(YR, "-01-01 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
    
    #Beginning the loop for every variable
    for(VAR in vars.all){
      #Checking for the windspeed exception
      mod.list[[paste(MOD, YR, VAR)]] <- met.pull(ncT, VAR)
      mod.list[[paste(MOD, YR, VAR)]]$model <- mod.name
      mod.list[[paste(MOD, YR, VAR)]]$sceario <- rcp
    } # end var loop
    ncdf4::nc_close(ncT)
  } # End file loop
} # End model loop



#Make into a dataframe
mod.df <- dplyr::bind_rows(mod.list)

#Splitting the models and scenarios into two columns for easier cross-walking and analysis
# mod.df$scenario <- ifelse(grepl("85", mod.df$model, fixed = TRUE), "rcp85", "rcp45")
# summary(mod.df)
head(mod.df)

met.day <- aggregate(value ~ var+Date+model+scenario, data =mod.df, FUN = mean)
names(met.day)[names(met.day)=="value"] <- "mean"
met.day$max  <- aggregate(value ~ var+Date+model+scenario, data =mod.df, FUN = max)$value
met.day$min  <- aggregate(value ~ var+Date+model+scenario, data =mod.df, FUN = min)$value
write.csv(met.day, file.path(path.google, "processed_data/Met_All_Daily.csv"), row.names=F)

#I've decided to split them by variable of interest here because the data gets too large otherwise
heat.df <- mod.df[mod.df$var == "air_temperature",]

#adding a flag for Extreme temperature ; 313.15 = 40˚C; from Huve et al 2011 Plant Cell Environment
heat.df$HeatExtreme <-  ifelse(heat.df$value > 313.15, 1, 0)

heat.df$value <- heat.df$value -273.15 # Converting to celcius

#Taking the mean heat and the sum. THe sum is more useful for counting the number of hours with temp above the heatwave threshold
heat.agg <- aggregate(value~var+Date+model+scenario, data =heat.df, FUN = mean)
colnames(heat.agg) <- c("var", "Date", "model", "scenario", "mean")
heat.agg$HExt.hours <- aggregate(HeatExtreme~var+Date+model+scenario, data =heat.df, FUN = sum)[,c("HeatExtreme")]
heat.agg$HExt.hours[heat.agg$model=="GFDL-CM3"] <- heat.agg$HExt.hours[heat.agg$model=="GFDL-CM3"]*3 # GFDL is 3-hourly

#Saving the dataframe of temperature at daily resolution
write.csv(heat.agg, file.path(path.google, "processed_data/Met_Temperature_Daily.csv"), row.names=F)


#Making a precipitation specific dataframe for conversion
precip.df <- mod.df[mod.df$var == "precipitation_flux",]

#Summing the precipitation converts from kg2/m2/hour to kg2/m2/day
## CR: The precip hours are kg/m2/s unless you're seeing something I'm not
#I am still summing for posterity but our workflow uses to mean to match with ED2 output
precip.agg <- aggregate(value~var+Date+model+scenario, data =precip.df, FUN = mean)
colnames(precip.agg) <- c("var", "Date", "model", "scenario", "precip.mean")
# precip.agg$sum <- aggregate(value~var+Date+model+scenario, data =precip.df, FUN = sum)[,c("value")]

write.csv(precip.agg, file.path(path.google, "processed_data/Met_Precip_Daily.csv"), row.names=F)

