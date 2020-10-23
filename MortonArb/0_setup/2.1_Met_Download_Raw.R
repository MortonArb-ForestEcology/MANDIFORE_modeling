# Download/Extract Met Products
# Note: GFDL needs to be downloaded & stored locally for extraction.  go here: to find/queue up products: https://esgf-node.llnl.gov/search/cmip5/ 
# path.pecan <- "~/Desktop/Research/pecan/"
path.out = "../met_raw.v2"

# Path to pecan repository where functions now live
# path.pecan <- "~/Desktop/Research/pecan/"
path.pecan <- "../../../pecan/"

site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04


# -------------------------------
# Future met from GFDL
# -------------------------------
# source(file.path(path.pecan, "modules/data.atmosphere/R", "download.GFDL.R"))
source("pecan_met_conversion/download.GFDL.R")

# ------------
# Using the default GCM
# ------------
# ENS.all <- c("CM3", "ESM2M", "ESM2G")
ENS.all <- c("CM3")
# CM3: https://www.gfdl.noaa.gov/coupled-physical-model-cm3/

for(ENS in ENS.all[1]){
  download.GFDL(outfolder=file.path(path.out, "subdaily", site.name), 
                start_date="2006-01-01", end_date="2099-12-31", 
                lat.in=site.lat, lon.in=site.lon,
                overwrite = FALSE, verbose = FALSE,
                model = ENS, scenario = "rcp45", ensemble_member = "r1i1p1", add.co2=TRUE, method="local", local.path="/Volumes/Celtis/Meteorology/CMIP5/")
  
  download.GFDL(outfolder=file.path(path.out, "subdaily", site.name), 
                start_date="2006-01-01", end_date="2100-12-31", 
                lat.in=site.lat, lon.in=site.lon,
                overwrite = FALSE, verbose = FALSE,
                model = ENS, scenario = "rcp85", ensemble_member = "r1i1p1", add.co2=TRUE, method="local", local.path="/Volumes/Celtis/Meteorology/CMIP5/")
}
# -------------------------------

# -------------------------------
# Get NLDAS so we can do the temporal downscaling
# -------------------------------
# source("~/Desktop/Research/pecan/")
source(file.path(path.pecan, "modules/data.atmosphere/R", "extract_local_NLDAS.R"))
ldas.type = "NLDAS"
path.nldas = "/Volumes/Celtis/Meteorology/LDAS/NLDAS_FORA0125_H.002/netcdf/"
extract.local.NLDAS(outfolder=file.path(path.out, "subdaily", site.name, "NLDAS"), in.path=path.nldas, 
                    start_date="1980-01-01", end_date="2019-12-31", 
                    site_id=site.name, lat.in=site.lat, lon.in=site.lon)
# -------------------------------


# -------------------------------
# Additional Datasets Downloaded by Lucien
# -------------------------------
source(file.path(path.pecan, "modules/data.atmosphere/R", "extract_local_CMIP5.R"))
path.cmip5 = "/Volumes/Seagate Portable Drive/lfitzpatrick/GCM/"
GCM.scenarios = c("rcp45", "rcp85")
GCM.list <- dir(path.cmip5)
cmip5.start = "2006-01-01"
cmip5.end   = "2099-12-31"

# path.out
# path.cmip <- "day/atmos/day/r1i1p1/latest/"

GCM.done <- dir(file.path(path.out, "daily", site.name))
GCM.list <- GCM.list[!GCM.list %in% GCM.done]

for(GCM in GCM.list){
  for(scenario in GCM.scenarios){
    
    print(paste(GCM, scenario, sep=" - "))
    # huss_day_MIROC-ESM_past1000_r1i1p1_08500101-10091231.nc
    extract.local.CMIP5(outfolder = file.path(path.out, "daily", site.name, GCM, scenario), 
                        in.path = file.path(path.cmip5, GCM, scenario), 
                        start_date = "2099-01-01", end_date = "2100-12-31", 
                        site_id = site.name, lat.in = site.lat, lon.in = site.lon, 
                        model = GCM, scenario = scenario, ensemble_member = "r1i1p1",
                        adjust.pr=10)   
    print("")
  } # end GCM.scenarios
} # End GCM lop
# -------------------------------
