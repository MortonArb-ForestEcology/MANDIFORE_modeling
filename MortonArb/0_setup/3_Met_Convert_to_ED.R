# Define pecan file path
# path.pecan <- "~/Desktop/Research/pecan/"
path.pecan <- "~/pecan/"

# Source PEcAn ED conversion file
source(file.path(path.pecan, "base/utils/R/seconds_in_year.R"))
source(file.path(path.pecan, "base/utils/R/days_in_year.R"))
source(file.path(path.pecan, "modules/data.atmosphere/R/solar_angle.R"))
# source(file.path(path.pecan, "models/ed/R/write_ed_metheader.R"))
source("pecan_met_conversion/write_ed_metheader.R")
source(file.path(path.pecan, "models/ed/R/check_ed_metheader.R"))
# source(file.path(path.pecan, "models/ed/R/met2model.ED2.R"))
source("pecan_met_conversion/met2model.ED2.R")


met.base="/mnt/data/crollinson/MANDIFORE_modeling/MortonArb/met_ed"
in.base="../met_raw/subdaily/MortonArb"
outfolder="../met_ed/"

SITE=site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

if(!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

GCMs <- dir(file.path(in.base))
GCMs <- GCMs[!GCMs %in% c("NLDAS", "GFDL_CM3_rcp45_r1i1p1", "GFDL_CM3_rcp85_r1i1p1")]

# NEED TO FIX LEAP YEAR -- IT KEEPS SKIPPING!
for(GCM in met.avail){
  rcp.avail <- dir(file.path(in.base, GCM))
  for(RCP in rcp.avail){
    path.in <- file.path(in.base, GCM, RCP, paste0(GCM, ".tdm"))
    gcm.lab <- paste(GCM, RCP, sep="_")
    
    met2model.ED2(in.path=path.in, 
                  in.prefix=paste0(GCM, ".tdm"), 
                  outfolder=file.path(outfolder, SITE, gcm.lab), 
                  header_folder = file.path(met.base, SITE, gcm.lab),
                  start_date="2006-01-01", 
                  end_date="2099-12-31",
                  leap_year = FALSE, overwrite=F)
    
  }
}
