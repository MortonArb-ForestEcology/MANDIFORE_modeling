# Workflow to convert NEON Veg data to ED2 .css file
# 14 June 2022 -- Christy working from Pecan extract_NEON_veg & this tutorial: https://www.neonscience.org/resources/learning-hub/tutorials/download-explore-neon-data

library(neonstore); library(neonUtilities)

dir.create("../neon_data_raw", recursive = T, showWarnings = F)

# NEON says loadByProduct works best because it does a couple things all at once; 
#  - it's not letting me specify the savepath though 
sitename="TALL"
store_dir = "../neon_data_raw"
neonstore::neon_download("DP1.10098.001", dir = store_dir, site = sitename)

apparentindividual <- neonstore::neon_read(table = "apparentindividual", product = "DP1.10098.001", site = sitename, dir = store_dir)
mappingandtagging <- neonstore::neon_read(table = "mappingandtagging", product = "DP1.10098.001", site = sitename, dir = store_dir)
joined.veg <- dplyr::left_join(mappingandtagging, apparentindividual, by = "individualID")
#Filter joined.veg for required information: DBH, tree height, and species
filter.veg <- dplyr::select(joined.veg, .data$siteID.x, .data$plotID.x, .data$subplotID, .data$taxonID, .data$scientificName, .data$taxonRank, .data$date.y, .data$stemDiameter, .data$height)
summary(filter.veg)

# Moving into a dataframe way of working because that's what I know
df.veg <- data.frame(filter.veg)
names(df.veg) <- c("site_name", "plot", "subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height")
df.veg$year <- as.numeric(format(df.veg$date, "%Y"))
summary(df.veg)

# Subsetting to only get data with a DBH & a height
df.veg <- df.veg[!is.na(df.veg$DBH) & !is.na(df.veg$subplot) & !is.na(df.veg$height),]
summary(df.veg)

# Going to need to think about the best way to do this crosswalk... there's a TON of species since it's clearly not all trees
summary(as.factor(df.veg$species))

# Converting the 

#neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")


test <- neonstore::stack