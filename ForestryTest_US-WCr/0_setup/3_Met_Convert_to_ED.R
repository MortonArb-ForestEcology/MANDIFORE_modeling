# Define pecan file path
path.pecan <- "~/Desktop/Research/pecan/"

# Source PEcAn ED conversion file
source(file.path(path.pecan, "base/utils/R/seconds_in_year.R"))
source(file.path(path.pecan, "base/utils/R/days_in_year.R"))
source(file.path(path.pecan, "modules/data.atmosphere/R/solar_angle.R"))
source(file.path(path.pecan, "models/ed/R/write_ed_metheader.R"))
source(file.path(path.pecan, "models/ed/R/check_ed_metheader.R"))
# source(file.path(path.pecan, "models/ed/R/met2model.ED2.R"))
source("pecan_met_conversion/met2model.ED2.R")


in.base="../met_raw/WILLOWCREEK/"
outfolder="../ED_MET/"
# met.base="~/Desktop/Research/MANDIFORE_modeling/ForestryTest_US-WCr/ED_MET/"
met.base="/mnt/data/crollinson/MANDIFORE_modeling/ForestryTest_US-WCr/ED_MET/"
if(!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

gcm.convert <- c("GFDL_CM3_rcp45_r1i1p1")

met2model.ED2(in.path=file.path(in.base, gcm.convert[1]), 
              in.prefix=gsub("_", ".", gcm.convert[1]), 
              outfolder=file.path(outfolder, gcm.convert[1]), 
              header_folder = file.path(met.base, gcm.convert[1]),
              start_date="2006-01-01", 
              end_date="2100-12-31")
