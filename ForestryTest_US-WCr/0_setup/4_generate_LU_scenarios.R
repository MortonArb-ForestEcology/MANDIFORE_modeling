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

lu.settings <- data.frame(file.name = c("US-WCr_Production", "US-WCr_Ecological"),
                          minDBH    = c(10, 50),
                          pharv     = c(1, 0.5),
                          aharv     = c(0.02, 0.02)
                          )

# constants during the run
lu.suff <- c("lat42.5lon-90.5.lu")
biomass.targ <- -1 # target biomass removed; set to negative to ignore
year.start <- 2006
year.end <- 2100
block.area <- 1e6 # LU area; dunno what this actually does
lu.bb <- c(-90.750, -90.250, 42.250, 42.750) # Land Use boudnign box: xmin, xmax, ymin, ymax
harvest.pft <- c(6, 8, 9, 10, 11)

for(i in 1:nrow(lu.settings)){
  file.name = paste(lu.settings$file.name[i], lu.suff, sep="_")
  minDBH    = lu.settings$minDBH[i]
  pharv     = lu.settings$pharv[i]
  aharv     = lu.settings$aharv[i]
  
  # LU Table Header Categories
  LU.header <- list()
  LU.header$WEST.LONGITUDE = lu.bb[1]
  LU.header$EAST.LONGITUDE = lu.bb[2]
  LU.header$SOUTH.LATITUDE = lu.bb[3]
  LU.header$NORTH.LATITUDE = lu.bb[4]
  LU.header$BLOCK.AREA = block.area # Copied from PalEON; not sure if this matters
  LU.header$FIRST.LUYEAR = year.start
  LU.header$LAST.LUYEAR = year.end
  LU.header$N.PFT.HARVEST = length(harvest.pft)
  LU.header$HARVEST.PFT = harvest.pft
  LU.header$MINDBH.1ARY = rep(minDBH, LU.header$N.PFT.HARVEST)
  LU.header$HARVPROB.1ARY = rep(pharv, LU.header$N.PFT.HARVEST)
  LU.header$MINDBH.2ARY = rep(minDBH, LU.header$N.PFT.HARVEST)
  LU.header$HARVPROB.2ARY = rep(pharv, LU.header$N.PFT.HARVEST)
  
  # Building the LU Transition Table -- Unless ED is modified, there should be 19 columns + year as row names
  LU.cols <- c("year", "cp", "pc", "pv", "vp", "cv", "sc", "cs", "sp", "ps", "vs", "sbh", "f_sbh", "vbh", "f_vbh", "sbh2", "f_sbh2", "vbh2", "f_vbh2")
  lu.mat <- array(0, dim=c(length(LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR), length(LU.cols)))
  colnames(lu.mat) <- LU.cols
  lu.mat[,"year"] <- LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR
  lu.mat[,c("sbh", "vbh")] <- biomass.targ # Sets biomass target or flag to ignore
  lu.mat[,c("f_sbh", "f_vbh")] <- aharv # Set area to harvest
  
  # Writing the file
  # First, lets take care of the header
  LU.header2 <- character(length(LU.header))
  for(i in 1:length(names(LU.header))){
    LU.header2[i] <- paste(names(LU.header)[i], "=", paste(LU.header[[i]], collapse=" "), sep=" ")
  }
  
  # Now writing the landuse transition matrix
  lu.mat2 <- character(nrow(lu.mat)+1)
  lu.mat2[1] <- paste(LU.cols, collapse=" ")
  for(i in 1:nrow(lu.mat)){
    lu.mat2[i+1] <- paste(lu.mat[i,], collapse=" ")
  }
  
  # Write the file
  file.LU <- paste0(file.name)
  writeLines(c(LU.header2, lu.mat2), file.LU)
  # close(file.LU)
}