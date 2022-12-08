
#TO FIX EVENTUALLY:

#have sites linked to region
#have mgmt prescription settings for all regions of interest pulled from csv, have code check region
  #then use appropriate settings for that region 
#will have to change harvest start year based on spin-up/burn-in time, don't want to start harvesting before the 
  #random climate fluctuations have settled
#'site_regions' variable assignment currently hardcoded
#
#

#------------------------------------------------------------------------------------------------------------------

#----------------
#Code information
#----------------


#Authors: 
#Christy Rollinson, Morton Arboretum
#Additions Bailey Murphy, University of Wisconsin-Madison
#Created: 6/28/22 



#WHAT THIS CODE DOES: 
#Creates land use transition (.lu) ED files. These prescribe the land use transition matrix, which is how mgmt stuff is integrated
#files are required for each of the 4 management schemes (passive, ecological, preservation, production)
  #even though the passive .lu file is technically sort of a dummy file since it's basically just no mgmt 



#FILES GENERATED:
# 1) .lu files for each of the four MANDIFORE mgmt types per site


#NOTES:
#naming convention: sitename_managementscheme_lat##.#lon##.#.lu

#Core forested NEON sites (10 total, AK dropped) for MANDIFORE modeling: 
# TALL
# OSBS
# UNDE
# HARV
# SCBI
# WREF
# YELL
# NIWO
# ORNL
# SJER

#the below mgmt settings will differ based on region of the US, for the MANDIFORE project these are derived by Paul Duffy 
#from the interviews that Nikki vonHedeman did. 
#currently prescriptions are just for Southeastern US!!!!!!!!!!!!!!!!!!!!!



# Fraction Harvested = 1/harvest rotation interval (years)

#SOUTHEASTERN US:
#  1. Production (clear cut); option for plantation later
#     - MinDBH = 15.88 cm 
#     - Harv Prob below DBH = 0 (nothing cut)
#     - Harv Prob above DBH = 87.5 (87.5% of trees cut)
#     - Mature Harvest Age = 70.75 years
#     - Fraction harvested = 0.014 yr-1 (return interval = 70.75 years, so = 0.014 yr-1)
#  2. Ecological (partial harvest); option for over/understory later
#     - Min DBH = 15.24 cm
#     - Harv Prob below DBH = 0.7 (70% of trees cut)
#     - Harv Prob above DBH = 0.7 (70% of trees cut)
#     - Mature Harvest Age = 10
#     - Fraction Harvested = 0.100 yr-1 (return interval = 10 years, so = 0.100 yr-1)
#  3. Preservation (no prescribed harvest)
#     - MinDBH = 200 cm 
#     - Harv Prob below DBH = 0 (nothing cut)
#     - Harv Prob above DBH = 0 (nothing cut)
#     - Mature Harvest Age =  NA
#     - Fraction harvested = 0.000 yr-1 
#  4. Passive (no active management, occasional harvest)
#     - MinDBH =  15 cm 
#     - Harv Prob below DBH =  1 (everything cut)
#     - Harv Prob above DBH = 1 (everything cut)
#     - Mature Harvest Age =  72.5 years
#     - Fraction harvested = 0.014 yr-1 (return interval = 72.5 years, so = 0.014 yr-1)




#-----------------------------------------------------------------------------------------------------------------

options(scipen=999) # Turn off scientific notation
options(stringsAsFactors = FALSE)

#----------------
#Hardcoded variables
#----------------

#change sites of interest here!!
#character vector of 4-letter NEON site codes
site_list <- c('TALL', 'OSBS', 'UNDE', 'HARV', 'SCBI', 'WREF', 'YELL', 'NIWO', 'ORNL', 'SJER') 

#currently hardcoded, fix later
site_regions <- data.frame(site = site_list,
                          region    = c("southeast", "southeast", "midwest", "northeast", "northeast", "PNW", 
                                        "rockies", "rockies", "appalachian", "PNW"))

ROI <- 'southeast' #region of interest

biomass.targ <- 0 # target biomass removed; set to negative to ignore
year.start <- 2006
year.end <- 2100
block.area <- 1e12 # LU area; dunno what this actually does, from paleon stuff?


#------------------------------------------------------------------------------------------------------------------

#----------------
#Import required data
#----------------

#pull in the management schemes prescriptions (regionally dependent)
mgmt_schemes <- read.csv("management_schemes.csv", header = TRUE)

#subset mgmt_schemes for region of interest
mgmt_schemes <- subset(mgmt_schemes, region == ROI)
#names(mgmt_schemes)

#subset site_regions to pull sites for a region of interest
site_regions <- subset(site_regions, region == ROI)

#pull in the experimental design csv (using the version w/o pft's will update name later)
expdesign_full <- read.csv("ExperimentalDesign_NEON.csv", header = TRUE)

#subset expdesign for just the SE sites (that's all we have mgmt prescription info for right now)
  #and for just a single model, don't need a bunch of replicates
expdesign <- subset(expdesign_full, SiteName %in% site_regions$site & GCM == "GFDL-CM3")


#----------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------------

#--------------------

#start building the .lu file structure

#--------------------

#lu.settings will be updated manually for different mgmt prescriptions/run groups------------------------------------
lu.settings <- data.frame(region = mgmt_schemes$region, 
                          management = mgmt_schemes$management,
                          minDBH    =  mgmt_schemes$MinDBH_cm,
                          pharv_above     = mgmt_schemes$harvest_probability_aboveDBH,
                          pharv_below     = mgmt_schemes$harvest_probability_belowDBH,
                          aharv     = mgmt_schemes$fraction_harvested)




#-------------------------------------------------------------------------------------------------------------------


#NOTE: if only want certain pft's harvested, adjust 'harvest.pft', or can manually edit the .lu files 
#after the fact, the matrix won't change just the 'N.PFT.HARVEST' and 'HARVEST.PFT' at the top of the file

#loop through each row in the experimental design spreadsheet to pull the necessary info and create the .lu files
for(i in 1:nrow(expdesign)){
  latmin <- round(expdesign$lat[i], 2) #trunc(expdesign$lat[i])
  lonmin <- round(expdesign$lon[i], 2) #trunc(expdesign$lon[i])
  # constants during the run
  lu.pref <- expdesign$management_file[i]
  lu.suff <- paste0("lat", latmin, "lon", lonmin, ".lu") #paste0("lat", latmin+0.5, "lon", lonmin-0.5, ".lu")
  
  lu.bb <- c(lonmin-1, lonmin, latmin, latmin+1) # Making this really big just for ease at the moment
  harvest.pft <- as.numeric(unlist(strsplit(paste(expdesign$pft_harvest[i]), "-"))) #added pft_harvest from Paul's prescrip
  
  minDBH    = lu.settings[lu.settings$management==paste(expdesign$management[i]),"minDBH"]
  pharv_above     = lu.settings[lu.settings$management==paste(expdesign$management[i]),"pharv_above"]
  pharv_below     = lu.settings[lu.settings$management==paste(expdesign$management[i]),"pharv_below"]
  aharv     = lu.settings[lu.settings$management==paste(expdesign$management[i]),"aharv"]
  
  # LU Table Header Categories
  LU.header <- list()
  LU.header$WEST.LONGITUDE = lu.bb[1]
  LU.header$EAST.LONGITUDE = lu.bb[2]
  LU.header$SOUTH.LATITUDE = lu.bb[3]
  LU.header$NORTH.LATITUDE = lu.bb[4]
  LU.header$BLOCK.AREA = block.area # Copied from Christy who copied from PalEON; not sure if this matters
  LU.header$FIRST.LUYEAR = year.start
  LU.header$LAST.LUYEAR = year.end
  LU.header$N.PFT.HARVEST = length(harvest.pft)
  LU.header$HARVEST.PFT = harvest.pft
  LU.header$MINDBH = rep(minDBH, LU.header$N.PFT.HARVEST)
  LU.header$HARVPROB.ABOVE = rep(pharv_above, LU.header$N.PFT.HARVEST)
  LU.header$HARVPROB.BELOW = rep(pharv_below, LU.header$N.PFT.HARVEST)
  
  # Building the LU Transition Table -- Unless ED is modified, there should be 19 columns + year as row names
  LU.cols <- c("year", "cp", "pc", "pv", "vp", "vc", "cv", "sc", "cs", "sp", "ps", "vs", "sbh", "f_sbh", "vbh", "f_vbh", "sbh2", "f_sbh2", "vbh2", "f_vbh2")
  lu.mat <- array(0, dim=c(length(LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR), length(LU.cols)))
  colnames(lu.mat) <- LU.cols
  lu.mat[,"year"] <- LU.header$FIRST.LUYEAR:LU.header$LAST.LUYEAR
  lu.mat[,c("sbh", "vbh")] <- biomass.targ # Sets biomass target or flag to ignore
  lu.mat[,c("f_sbh", "f_vbh")] <- aharv # Set area to harvest
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


#------------------------------------------------------------------------------------------------------------



