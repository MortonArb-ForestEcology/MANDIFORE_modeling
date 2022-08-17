# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Create statistical models to predict subdaily meteorology from daily means
# Creator: Christy Rollinson, updated 14 June 2022
# Contact: crollinson@mortonarb.org
# -----------------------------------

# -----------------------------------
# Description
# -----------------------------------
# Apply the statistical models from step 3 to convert the daily, bias-corrected met 
# files from step 2 (daily means) and predict subdaily values.  This gets done by 
# filtering backwards in time starting with the present (where the trianing data is).
#
# There are ways to improve this and speed it up, but hopefully this works for now.
# We whould also probably think about applying this filter approach to the bias-
# correction step to avoid abrupt and unreasonable jumps in climate.
# -----------------------------------


# -----------------------------------
# Workflow
# -----------------------------------
# 0. Load libraries, set up file paths, etc
# ----- Loop through by ensemble member by year ----------
#    1. Use align.met to match temporal resolution to training data
#    2. Predict subdaily values, filtering backwards in time
#    3. Write annual output into .nc files 
#       - separate file for each year/ensemle member; 
#       - all met vars in one annual file (similar to pecan met structure)
# ----- recycle steps 1 - 3 for all years in file ----------
# -----------------------------------


# -----------------------------------
# 0. Load libraries, set up file paths, etc
# -----------------------------------
# Script to prototype temporal downscaling
library(ncdf4)
library(mgcv)
library(MASS)
library(lubridate)
library(ggplot2)
library(stringr)
library(tictoc)
library(parallel)
# library(tictoc)
rm(list=ls())

source("pecan_met_utils/tdm_predict_subdaily_met.R")
source("pecan_met_utils/tdm_lm_ensemble_sims.R")
source("pecan_met_utils/tdm_subdaily_pred.R")
source("pecan_met_utils/align_met.R")


sites.neon <- read.csv("NEON_Field_Site_FOREST_CORE.csv")
wd.base = "../met_raw.v1"
scenarios <- c("rcp45", "rcp85")

ens.hr  <- 1 # Number of hourly ensemble members to create
n.day <- 1 # Number of daily ensemble members to process
# yrs.plot <- c(2015, 1985, 1920, 1875, 1800, 1000, 850)
yrs.plot <- c(2010, 2025, 2050, 2075, 2099)
timestep="3hr"
yrs.sim=2006:2099

yrs.train=NULL

# Setting up parallelization
parallel=FALSE
cores.max = 20

# Set up the appropriate seed
set.seed(0017)
seed.vec <- sample.int(1e6, size=500, replace=F)

for(i in 1:nrow(sites.neon)){
  site.name= sites.neon$field_site_id[i]
  site.lat = sites.neon$field_latitude[i]
  site.lon = sites.neon$field_longitude[i]
  
  path.train <- file.path(wd.base, "3hr", site.name, "NLDAS")
  path.lm <- file.path(wd.base, "3hr_mods.tdm", site.name)
  path.in <- file.path(wd.base, "daily", site.name)
  path.out <- file.path(wd.base, "3hr", site.name)
  
  GCM.list <- dir(path.in)
  
  GCM.list <- GCM.list[!GCM.list %in% c("bias_correct_qaqc", "NLDAS", "ACCESS1-3")]

  # Get rid of known problematic ensemble members
  # GCM.list <- GCM.list[!GCM.list %in% c("ACCESS1-3", "HadGEM2-ES", "HadGEM2-CC", "IPSL-CM5A-MR")]
  
  # -----------------------------------
  # 2. Apply the model
  # -----------------------------------
  # Set & create the output directory
  if(!dir.exists(path.out)) dir.create(path.out, recursive=T)
  print("----------------------------------------")
  print(site.name)
  for(GCM in GCM.list){
    for(SCEN in scenarios){
      print("")
      print(paste(GCM, SCEN, sep=": "))
      # out.scen <- file.path(path.out, GCM, SCEN)
      predict_subdaily_met(outfolder=path.out, in.path=file.path(path.in, GCM, SCEN),
                           in.prefix=paste(GCM, SCEN, sep="_"), lm.models.base=path.lm,
                           path.train=path.train, direction.filter="forward", yrs.predict=yrs.sim,
                           ens.labs = "tdm", resids = FALSE,
                           adjust.pr=1,
                           overwrite = FALSE, seed=seed.vec[1], print.progress = TRUE)
    } # End SCEN
  } # End GCM
  # ----------------------------------
} # End Site Loop (i)




# -----------------------------------

