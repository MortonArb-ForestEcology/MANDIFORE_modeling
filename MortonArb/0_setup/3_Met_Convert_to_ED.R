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
in.base="../met_raw/"
outfolder="../met_ed/"

SITE=site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

if(!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

met.avail <- dir(file.path(in.base, SITE))

# NEED TO FIX LEAP YEAR -- IT KEEPS SKIPPING!
for(GCM in met.avail){
  met2model.ED2(in.path=file.path(in.base, SITE, GCM), 
                in.prefix=gsub("_", ".", GCM), 
                outfolder=file.path(outfolder, SITE, GCM), 
                header_folder = file.path(met.base, SITE, GCM),
                start_date="2006-01-01", 
                end_date="2100-12-31",
                leap_year = FALSE, overwrite=F)
}
