# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Address systematic bias specific to each GCM and location (due to scale issues, 
#          model structure, etc.) by correcting it with a higher-resolution empirical product
# Creator: Christy Rollinson, 20 October 2020
# Contact: crollinson@mortonarb.org
# -----------------------------------
# Description
# -----------------------------------
# Bias-correct raw GCM output at daily timestep to match something more accurate
# This script will correct for both mean offsets and seasonal cycles
# -----------------------------------
# General Workflow Components
# -----------------------------------
# 0. Set up file structure, load packages, etc
# 1. Align Data:
# 2. Debias & Save Met
# -----------------------------------
# Met Dataset Workflow
# -----------------------------------
# 1. Set up ensemble structure; copy LDAS into ensemble directories
# 2. Debias each GCM (2006-2099)
# -----------------------------------

rm(list=ls())

# -----------------------------------
# 0. Set up file structure, load packages, etc
# -----------------------------------
# Load libraries
library(ncdf4)
library(mgcv); library(ggplot2)
library(stringr)
library(lubridate)

# Set the working directory
# wd.base <- ".."
# out.base <- wd.base
# setwd(wd.base)

# Setting some important file paths
path.pecan <- "~/Desktop/Research/pecan"

# Defining a site name -- this can go into a function later
vers=".v3"
site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

ens=1
n.ens=1
ens.mems="bc"

# Set up the appropriate seeds to use when adding ensembles
set.seed(1159)
seed.vec <- sample.int(1e6, size=500, replace=F)
seed <- seed.vec[min(ens)] # This makes sure that if we add ensemble members, it gets a new, but reproducible seed

# Setting up some basics for the file structure
out.base <- file.path("..", paste0("met_raw", vers), "daily_bc", site.name)
raw.base <- file.path("..", paste0("met_raw", vers), "daily", site.name)

if(!dir.exists(out.base)) dir.create(out.base, recursive=T)
# -----------------------------------

# -----------------------------------
# Run a loop to do all of the downscaling steps for each GCM and put in 1 place
# -----------------------------------
# Source the functions we need
source(file.path(path.pecan, "modules/data.atmosphere/R", "align_met.R"))
source(file.path(path.pecan, "modules/data.atmosphere/R", "debias_met_regression.R"))



# --------------------------
# 1. Debias 
# --------------------------
GCM.list <- dir(raw.base)
scenarios=c("rcp45", "rcp85")

# Remove NLDAS and known trouble makers
GCM.list <- GCM.list[!GCM.list %in% c("NLDAS", "ACCESS1-3", "HadGEM2-ES", "HadGEM2-CC", "IPSL-CM5A-MR")]

# 1. Align daily aggregated NLDAS with Ameriflux 1 hr
train.path <- file.path(raw.base, "NLDAS")
for(GCM in GCM.list){
  ens.ID=GCM
  
  for(SCEN in scenarios){
    source.path <- file.path(raw.base, GCM, SCEN)
    
    # We're now pulling an ensemble because we've set up the file paths and copied LDAS over 
    # (even though all ensemble members will be identical here)
    met.out <- align.met(train.path, source.path, yrs.train=1980:2019, yrs.source=NULL, n.ens=n.ens, seed=201708, pair.mems = FALSE, mems.train=paste(ens.ID, ens.mems, sep="_"))
    
    # Calculate wind speed if it's not already there
    # Note: right now only set up to do total windspeed and not north/east components
    if(!"wind_speed" %in% names(met.out$dat.source)){
      met.out$dat.source$wind_speed <- sqrt(met.out$dat.source$eastward_wind^2 + met.out$dat.source$northward_wind^2)
    }
    if(!"wind_speed" %in% names(met.out$dat.train)){
      met.out$dat.train$wind_speed <- sqrt(met.out$dat.train$eastward_wind^2 + met.out$dat.train$northward_wind^2)
    }
    
    # 2. Pass the training & source met data into the bias-correction functions; this will get written to the ensemble
    debias.met.regression(train.data=met.out$dat.train, source.data=met.out$dat.source, n.ens=n.ens, vars.debias=NULL, CRUNCEP=FALSE,
                          pair.anoms = FALSE, pair.ens = FALSE, uncert.prop="mean", resids = FALSE, seed=Sys.Date(),
                          outfolder=file.path(out.base), 
                          yrs.save=NULL, ens.name=paste(ens.ID, SCEN, sep="_"), ens.mems=ens.mems, lat.in=site.lat, lon.in=site.lon,
                          save.diagnostics=TRUE, path.diagnostics=file.path(out.base, "bias_correct_qaqc"), 
                          force.sanity=TRUE, sanity.tries=25, sanity.sd=6,
                          parallel = FALSE, n.cores = NULL, overwrite = TRUE, verbose = FALSE) 
    
  }
}
# --------------------------


# -----------------------------------
