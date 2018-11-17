# Identify sites
# Loop through sites

# Sourcing some functions we need 
source("../0_setup/model2netcdf.ED2.MANDIFORE.R")
source("../0_setup/pecan.utils/metutils.R")
source("../0_setup/pecan.utils/utils.R")
mstmip_vars <- read.csv("../0_setup/pecan.utils/mstmip_vars.csv", sep=";")
mstmip_local <- read.csv("../0_setup/pecan.utils/mstmip_local.csv", sep=";")

# some site metadata
sitelat =  45.805822
sitelon = -90.079722
start_date <- "2006-01-01" # The runs actually start in June 1800, but lets skip that first year because it's only partial
end_date = "2100-12-31"

## Original
runs.dir <- "ed_runs.v2" # Where our output is
all.runs <- dir(runs.dir) # Get a list of what's been run (at least partially)
out.base <- "extracted_output.v2" # Where we want to put our output
dir.create(out.base, recursive = T, showWarnings = F) # Create the directory if it's not there yet

# Get a list of what runs we need to extract
runs.done <- dir(out.base)
# runs.extract <- all.runs[!all.runs %in% runs.done]
runs.extract <- all.runs

for(RUNID in runs.extract){
  
  # -------
  # Set up dynamic file paths based on each site we're looping through
  # -------
  ed.dir <- file.path(runs.dir, RUNID, "analy") # Where the raw data are
  
  # Check to see if it finished the runs
  outfiles <- dir(ed.dir, "-E-") # Listing the monthly files
  # run.done <- any(as.numeric(substr(outfiles,11,14))==lubridate::year(end_date) & as.numeric(substr(outfiles,16,17))==lubridate::month(end_date))
  
  
  outdir <- file.path(out.base, RUNID) # Where we want to save our output
  dir.create(outdir, recursive = T, showWarnings = F)
  
  # For everything that is fine and not missing a year
  model2netcdf.ED2(ed.dir, outdir, sitelat, sitelon, start_date, end_date, pft_names = NULL, vars.E=T)
}