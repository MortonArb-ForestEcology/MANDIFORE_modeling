# Generate management driver files
# Only 2 schemes need LU drivers right now:
#  1. Production (clear cut); option for plantation later
#     - MinDBH = 10 cm 
#     - Harv Prob = 1 (everything cut) 
#     - Mature Harvest Age = 50 years
#     - Fraction harvested = 0.02 yr-1 (return interval = 50 years)
#  2. Ecological (partial harvest); option for over/understory later
#     - Min DBH = 50 cm
#     - Harv Prob = 0.5 (50% of trees cut)
#     - Fraction Harvested = 0.02 yr-1 (return interval = 50 years)


# Workflow:
# 1. Read table from Google with model settings
# 2. Loop through and write settings as appropriate
options(scipen=999) # Turn off scientific notation

site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

biomass.targ <- 0 # target biomass removed; set to negative to ignore
year.start <- 2006
year.end <- 2100
block.area <- 1e12 # LU area; dunno what this actually does

exp.design <- list()
exp.design[["none"]] <- list(Harvest.PFTs=c(9, 10, 11),
                             SLOG.DBH = c(200, 200, 200),
                             SLOG.PROB.G = c(0, 0, 0),
                             SLOG.PROB.L = c(0,0,0),
                             FPLT.DBH = c(200, 200, 200),
                             FPLT.PROB.G = c(0, 0, 0),
                             FPLT.PROB.L = c(0,0,0),
                             Harvest.yrs = NULL)
exp.design[["gaps"]] <- list(Harvest.PFTs=c(9, 10, 11),
                             SLOG.DBH = c(20, 20, 20),
                             SLOG.PROB.G = c(1, 1, 1),
                             SLOG.PROB.L = c(1, 1, 1),
                             FPLT.DBH = c(200, 200, 200),
                             FPLT.PROB.G = c(0, 0, 0),
                             FPLT.PROB.L = c(0,0,0),
                             Harvest.yrs = data.frame(year=2020:2024,
                                                      area=0.05))
exp.design[["under"]] <- list(Harvest.PFTs=c(9, 10, 11),
                              SLOG.DBH = c(20, 20, 20),
                              SLOG.PROB.G = c(0.01, 0.01, 0.01), # 1% collateral damage
                              SLOG.PROB.L = c(0.75, 0.1, 1), # Get rid of all late successional; leave all oaks; assume 10% collateral
                              FPLT.DBH = c(200, 200, 200),
                              FPLT.PROB.G = c(0, 0, 0),
                              FPLT.PROB.L = c(0,0,0),
                              Harvest.yrs = data.frame(year=2020:2024,
                                                       area=0.20)) # Some area won't get touched b/c 20% of available land

exp.design[["shelter"]] <- list(Harvest.PFTs=c(9, 10, 11),
                                SLOG.DBH = c(20, 20, 20),
                                SLOG.PROB.G = c(0.70, 0.30, 0.70), # 1% collateral damage
                                SLOG.PROB.L = c(1, 1, 1), # Get rid of all understory stems
                                FPLT.DBH = c(200, 200, 200),
                                FPLT.PROB.G = c(0, 0, 0),
                                FPLT.PROB.L = c(0,0,0),
                                Harvest.yrs = data.frame(year=2020:2024,
                                                         area=0.20)) # Some area won't get touched b/c 20% of available land


latmin <- trunc(site.lat)
lonmin <- trunc(site.lon)

# Loop through management scenarios
for(MGMT in names(exp.design)){
  
  # constants during the run
  lu.pref <- paste("MortonArb", MGMT, sep="_")
  lu.suff <- paste0("lat", latmin+0.5, "lon", lonmin-0.5, ".lu")
  
  lu.bb <- c(lonmin-1, lonmin, latmin, latmin+1) # Making this really big just for ease at the moment
  
  # LU Table Header Categories
  LU.header <- list()
  LU.header$WEST.LONGITUDE = lu.bb[1]
  LU.header$EAST.LONGITUDE = lu.bb[2]
  LU.header$SOUTH.LATITUDE = lu.bb[3]
  LU.header$NORTH.LATITUDE = lu.bb[4]
  LU.header$BLOCK.AREA = block.area # Copied from PalEON; not sure if this matters
  LU.header$FIRST.LUYEAR = year.start
  LU.header$LAST.LUYEAR = year.end
  LU.header$N.PFT.HARVEST = length(exp.design[[MGMT]]$Harvest.PFTs)
  LU.header$HARVEST.PFT = exp.design[[MGMT]]$Harvest.PFTs
  LU.header$MINDBH.SLOG = exp.design[[MGMT]]$SLOG.DBH
  LU.header$HARVPROB.SLOG.G = exp.design[[MGMT]]$SLOG.PROB.G
  LU.header$HARVPROB.SLOG.L = exp.design[[MGMT]]$SLOG.PROB.L
  LU.header$MINDBH.FPLT = exp.design[[MGMT]]$FPLT.DBH
  LU.header$HARVPROB.FPLT.G = exp.design[[MGMT]]$FPLT.PROB.G
  LU.header$HARVPROB.FPLT.L = exp.design[[MGMT]]$FPLT.PROB.L
  
  # Building the LU Transition Table -- Unless ED is modified, there should be 19 columns + year as row names
  LU.cols <- c("year", "cp", "pc", "pv", "vp", "vc", "cv", "sc", "cs", "sp", "ps", "vs", "sbh", "f_sbh", "vbh", "f_vbh", "sbh2", "f_sbh2", "vbh2", "f_vbh2")
  lu.mat <- array(0, dim=c(length(LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR), length(LU.cols)))
  colnames(lu.mat) <- LU.cols
  lu.mat[,"year"] <- LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR
  lu.mat[,c("sbh", "vbh")] <- biomass.targ # Sets biomass target or flag to ignore
  
  if(!is.null(exp.design[[MGMT]]$Harvest.yrs)){
    lu.mat[lu.mat[,"year"] %in% exp.design[[MGMT]]$Harvest.yrs$year,"f_vbh"] <- exp.design[[MGMT]]$Harvest.yrs$area # Set area to harvest
  }
  lu.mat[,2:ncol(lu.mat)] <- format(round(lu.mat[,2:ncol(lu.mat)], digits=6), scientific=F, nsmall=6)
  
  # Writing the file
  # First, lets take care of the header
  LU.header2 <- character(length(LU.header))
  for(i in 1:length(names(LU.header))){
    LU.header2[i] <- paste(stringr::str_pad(names(LU.header)[i], width=14, side="right", pad=" "), "=", paste(LU.header[[i]], collapse=" "), sep=" ")
  }
  
  # Now writing the landuse transition matrix
  lu.mat2 <- character(nrow(lu.mat)+1)
  lu.mat2[1] <- paste(LU.cols, collapse=" ")
  for(i in 1:nrow(lu.mat)){
    lu.mat2[i+1] <- paste(lu.mat[i,], collapse=" ")
  }
  
  # Write the file
  file.LU <- paste(lu.pref, lu.suff, sep="_")
  writeLines(c(LU.header2, lu.mat2), file.path("../lu_files", file.LU))
  # close(file.LU)
  # write.table(lu.mat, file.name, row.names=F, quote=F)
    
}
