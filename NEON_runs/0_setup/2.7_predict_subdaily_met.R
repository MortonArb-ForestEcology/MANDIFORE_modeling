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

# wd.base <- "/home/crollinson/met_ensemble/"
# wd.base <- "~/Desktop/Research/met_ensembles/"

# setwd(wd.base)

# Hard-coding numbers for a single site for now
vers=".v1"
site.name= "BART"
site.lat = 44.063889
site.lon = -71.287375

wd.base = file.path("..", paste0("met_raw", vers))
# dat.base <- file.path(wd.base, "data")

# 

path.train <- file.path(wd.base, "3hr", site.name, "NLDAS")
path.lm <- file.path(wd.base, "3hr_mods.tdm", site.name)
path.in <- file.path(wd.base, "daily", site.name)
path.out <- file.path(wd.base, "3hr", site.name)
# path.in <- file.path(dat.base, "met_ensembles", paste0(site.name, vers), "day/ensembles")
# path.out <- file.path(dat.base, "met_ensembles", paste0(site.name, vers), "1hr/ensembles")

scenarios <- c("rcp45", "rcp85")
GCM.list <- dir(path.in)

# Get rid of known problematic ensemble members
# GCM.list <- GCM.list[!GCM.list %in% c("ACCESS1-3", "HadGEM2-ES", "HadGEM2-CC", "IPSL-CM5A-MR")]
GCM.list <- GCM.list[!GCM.list %in% c("bias_correct_qaqc", "NLDAS")]

ens.hr  <- 1 # Number of hourly ensemble members to create
n.day <- 1 # Number of daily ensemble members to process
# yrs.plot <- c(2015, 1985, 1920, 1875, 1800, 1000, 850)
yrs.plot <- c(2010, 2025, 2050, 2075, 2099)
timestep="3hr"
# years.sim=2015:1900
yrs.sim=2006:2099

# Setting up parallelization
parallel=FALSE
cores.max = 20

# Set up the appropriate seed
set.seed(0017)
seed.vec <- sample.int(1e6, size=500, replace=F)
# -----------------------------------

# -----------------------------------
# 2. Apply the model
# -----------------------------------
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_predict_subdaily_met.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_lm_ensemble_sims.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "align_met.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_subdaily_pred.R"))
# source(file.path(path.pecan, "tdm_predict_subdaily_met.R"))
source("pecan_met_utils/tdm_predict_subdaily_met.R")
source("pecan_met_utils/tdm_lm_ensemble_sims.R")
source("pecan_met_utils/tdm_subdaily_pred.R")
source("pecan_met_utils/align_met.R")


# Set & create the output directory
if(!dir.exists(path.out)) dir.create(path.out, recursive=T)

for(GCM in GCM.list){
  # GCM="Ameriflux"
  # tic()
  # Set the directory where the output is & load the file
  # path.gcm <- file.path(path.in, GCM)
  
  # gcm.splt <- strsplit(GCM, "_")[[1]]
  # MOD=gcm.splt[1]
  # SCEN=gcm.splt[2]
  
  # Doing this one ensemble member at at time
  # Figure out what's been done already
  for(SCEN in scenarios){
      out.scen <- file.path(path.out, GCM, SCEN)
      predict_subdaily_met(outfolder=path.out, in.path=file.path(path.in, GCM, SCEN),
                           in.prefix=paste(GCM, SCEN, sep="_"), lm.models.base=path.lm,
                           path.train=path.train, direction.filter="forward", yrs.predict=yrs.sim,
                           ens.labs = "tdm", resids = FALSE,
                           adjust.pr=1,
                           overwrite = FALSE, seed=seed.vec[1], print.progress = TRUE)
  }
}
# -----------------------------------
