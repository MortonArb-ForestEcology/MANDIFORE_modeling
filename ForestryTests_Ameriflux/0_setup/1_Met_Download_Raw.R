# Download/Extract Met Products
path.pecan <- "~/Desktop/Research/pecan/"
path.out = "../met_raw"

# Load the experimental design table
sites <- read.csv("Sites_ExperimentalDesign_Test.csv")
summary(sites)

# sites <- aggregate(expdesign[,c("lat", "lon")], by=list(expdesign$SiteName), mean)
# names(sites)[1] <- "SiteName"


# -------------------------------
# Future met from GFDL
# -------------------------------
# source(file.path(path.pecan, "modules/data.atmosphere/R", "download.GFDL.R"))
source("pecan_met_conversion/download.GFDL.R")

# ------------
# Using the default GCM
# ------------
ENS.all <- c("CM3", "ESM2M", "ESM2G")
# CM3: https://www.gfdl.noaa.gov/coupled-physical-model-cm3/

for(i in 1:nrow(sites)){
  site.name=sites$SiteName[i]
  site.lat =sites$lat[i]
  site.lon =sites$lon[i]
  
  for(ENS in ENS.all[1]){
    download.GFDL(outfolder=file.path(path.out, site.name), 
                  start_date="2006-01-01", end_date="2100-12-31", 
                  lat.in=site.lat, lon.in=site.lon,
                  overwrite = FALSE, verbose = FALSE,
                  model = ENS, scenario = "rcp45", ensemble_member = "r1i1p1", add.co2=TRUE)
    
    download.GFDL(outfolder=file.path(path.out, site.name), 
                  start_date="2006-01-01", end_date="2100-12-31", 
                  lat.in=site.lat, lon.in=site.lon,
                  overwrite = FALSE, verbose = FALSE,
                  model = ENS, scenario = "rcp85", ensemble_member = "r1i1p1", add.co2=TRUE,
                  FTP=FALSE)
  }
  
}
# -------------------------------
