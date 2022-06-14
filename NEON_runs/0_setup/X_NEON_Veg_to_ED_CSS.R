# Workflow to convert NEON Veg data to ED2 .css file

library(neonstore); library(neonUtilities)

neonstore::neon_download("DP1.10098.001", dir = store_dir, table = NA, site = sitename, start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")


test <- neonstore::stack