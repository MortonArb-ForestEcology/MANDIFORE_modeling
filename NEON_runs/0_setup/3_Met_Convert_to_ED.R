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



vers=".v1"
site.name= "BART"
site.lat = 44.063889
site.lon = -71.287375

met.base=file.path("/mnt/data/crollinson/MANDIFORE_modeling/MortonArb", paste0("met_ed", vers))
in.base=file.path("..", paste0("met_raw", vers), "subdaily_tdm", SITE)
outfolder=file.path("..",paste0("met_ed", vers))

if(!dir.exists(outfolder)) dir.create(outfolder, recursive = T)

GCM.list <- dir(file.path(in.base))
GCM.list <- GCM.list

# NEED TO FIX LEAP YEAR -- IT KEEPS SKIPPING!
for(GCM in GCM.list){
  # rcp.avail <- dir(file.path(in.base, GCM))
  # for(RCP in rcp.avail){
    path.in <- file.path(in.base, GCM)

    # Setting up a dynamic prefix; assumes suffix for everhting is .YYYY.nc
    GCM.pref <- dir(path.in)[1]
    GCM.pref <- substr(GCM.pref, 1, nchar(GCM.pref)-8)
    
    leap <- ifelse(substr(GCM, 1, 8) %in% c("GFDL_CM3", "GFDL-CM3"), FALSE, TRUE)
    
    
    met2model.ED2(in.path=path.in, 
                  in.prefix=GCM.pref, 
                  outfolder=file.path(outfolder, SITE, GCM), 
                  header_folder = file.path(met.base, SITE, GCM),
                  start_date="2006-01-01", 
                  end_date="2099-12-31",
                  leap_year = leap, overwrite=F)
    
  # } # End RCP loop
} # End GCM loop
