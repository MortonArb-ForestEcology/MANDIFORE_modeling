# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Generate the statistical models to predict subdaily meteorology from daily means
# Creator: Christy Rollinson, Updated  14 June 2022
# Contact: crollinson@mortonarb.org
# -----------------------------------

# -----------------------------------
# Description
# -----------------------------------
# Make statistical models that take the daily, bias-corrected met files that come out 
# of step 2 (daily means) and predict subdaily values (e.g. hourly or 3-hourly) using 
# the a training dataset (e.g. NLDAS, GLDAS)
#
# This script just generates and stores the models so that they can be applied and 
# filtered through the bias-corrected met.  There are many ways in which both the 
# models and approach can be sped, up (saving models & betas separately, etc.), but 
# this should hopefully just get it working for now.
# -----------------------------------


# -----------------------------------
# Workflow
# -----------------------------------
# 0. Load Libraries, set up file directories
# 1. Load and format training data
#    1.0 Read data & Make time stamps
#    1.1 Coming up with the daily means that are what we can use as predictors
#    1.2 Setting up a 1-hour lag -- smooth transitions at midnight
#    1.3 Setting up a variable to 'preview' the next day's mean to help get smoother transitions
#    1.4 calculate tmin & tmax as departure from mean; order data
# 2. Train the models for each variable and save them to be read in as needed
#    2.1 Generating all the daily models, save the output as .Rdata files, then clear memory
# -----------------------------------

# ------------------------------------------
# 0. Load Libraries, set up file directories
# ------------------------------------------
# Script to prototype temporal downscaling
library(ncdf4)
library(mgcv)
library(MASS)
library(lubridate)
library(ggplot2)
# library(tictoc)
rm(list=ls())

# wd.base <- "~/Desktop/Research/met_ensembles/"
# setwd(wd.base)


vers=".v1"
site.name= "BART"
site.lat = 44.063889
site.lon = -71.287375

wd.base = file.path("..", paste0("met_raw", vers))

path.train <- file.path(wd.base, "3hr", site.name, "NLDAS")
yrs.train=NULL

path.out <- file.path(wd.base, "3hr_mods.tdm", site.name)
# path.pecan <- "../../../pecan/"

fig.dir <- file.path(path.out, "model_qaqc")

if(!dir.exists(path.out)) dir.create(path.out, recursive = T)
if(!dir.exists(fig.dir)) dir.create(fig.dir, recursive = T)
# ------------------------------------------

# ------------------------------------------
# 2. Generate the sub-daily models
# ------------------------------------------
# Name of dat.train file in netcdf format meeting CF standards
# dat.trian.nc <- ()
# scripts.tdm <- dir(path.pecan, "tdm")
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_generate_subdaily_models.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_temporal_downscale_functions.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "tdm_model_train.R"))
# source(file.path(path.pecan, "modules/data.atmosphere/R", "align_met.R"))
source("pecan_met_utils/tdm_generate_subdaily_models.R")
source("pecan_met_utils/tdm_temporal_downscale_functions.R")
source("pecan_met_utils/tdm_model_train.R")
source("pecan_met_utils/align_met.R")

gen.subdaily.models(outfolder=path.out, path.train=path.train,
                    yrs.train=1990:2019, direction.filter="forward", in.prefix=site.name,
                    n.beta=1, day.window=7, seed=1026, resids = FALSE, 
                    parallel = FALSE, n.cores = NULL, overwrite = FALSE, verbose = FALSE, print.progress=T) 
# ------------------------------------------
