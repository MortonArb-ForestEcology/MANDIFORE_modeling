#------------------------------------------------
# Script to Convert Phase1a Land Use driver to work with PalEON ED runs
# Christy Rollinson, crollinson@gmail.com
# 26 March 2015
#------------------------------------------------
# Modifications Required
#	1) Add 1000 to each year (because ED doesnt' like going from 3-4 digit years or 0 place-holders)
#	2) Add lat/lon to after site name
#	3) get rid of the last 2 columns (sbh3, f_sbh3) -- not currently supported in ED
#------------------------------------------------

#------------------------------------------------
# Set the working directories, etc
#------------------------------------------------
dir_lu_orig <- "~/Dropbox/PalEON CR/phase1a_env_drivers/phase1a_env_drivers_v3/site_lulc"
outdir <- "~/Dropbox/PalEON CR/ED_PalEON_processing/lulcc_paleon_ed"

sites <- c("PHA", "PHO", "PUN", "PBL", "PDL", "PMB")
site.lat <- c(42.5, 45.5, 46.5, 46.5, 47.5, 43.5)
site.lon <- c(-72.5, -68.5, -89.5, -94.5, -95.5, -82.5)
#------------------------------------------------


#------------------------------------------------
# Loop through the LU files
#------------------------------------------------
for(s in 1:length(sites)){
	lulc <- read.table(file.path(dir_lu_orig, paste0(sites[s], ".lu")), header=T)

	lulc$year <- lulc$year+1000
	lulc[,2:ncol(lulc)] <- format(round(lulc[,2:ncol(lulc)], digits=6), scientific=F, nsmall=6) # this step probably isn't needed, but keeps the same formatting
	lulc <- lulc[,1:(ncol(lulc)-2)] # ED does not support the last 2 
	summary(lulc)

	write.table(lulc, file.path(outdir, paste0(sites[s], "lat", site.lat[s],"lon",site.lon[s],".lu")), row.names=F, quote=F)
}
#------------------------------------------------

# ------------------------------------------------
# NOTE ! NOTE ! NOTE ! NOTE ! NOTE ! NOTE ! NOTE #
# ------------------------------------------------
# After writing the table, you need to manually add the header
# (until somebody figures out how to automate that, but I was lazy) 
# ------------------------------------------------
# NOTE ! NOTE ! NOTE ! NOTE ! NOTE ! NOTE ! NOTE #
# ------------------------------------------------

