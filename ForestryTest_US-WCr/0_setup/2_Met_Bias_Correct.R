# -----------------------------------
# Script Information
# -----------------------------------
# Purpose: Bias-correction to create a smooth daily met product from multiple sources 
#          of varying temporal frequencies and extents
# Creator: Christy Rollinson, 1 July 2016
# Contact: crollinson@gmail.com
# -----------------------------------
# Description
# -----------------------------------
# Bias-correct raw met data & take monthly variables to daily time step
# The end state of this script is continuous, smoothly daily output from 850-2010+ 
# that can be used as is or fed into the day to subday script to get hourly drivers
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
# 2. Debias CRUNCEP (1 series) using LDAS (1 series)
#    - save 1901-1979 (until LDAS kicks in)
# 3. Debias GCM historical runs (1 time series) using CRUNCEP (n.ens series)
#    - save 1850-1901 (until CRUNCEP kicks in)
# 4. Debias GCM past millennium (1 time series) using GCM Historical (n.ens series)
#    - save 850-1849 (until GCM historical kicks in)
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
# wd.base <- "/Volumes/GoogleDrive/My Drive/Temporal Downscaling Group/Analyses/"
# out.base <- wd.base
# setwd(wd.base)

# Setting some important file paths
path.pecan <- "~/Desktop/Research/pecan"

# Defining a site name -- this can go into a function later
site.name = "WILLOWCREEK"
vers=".v2"
site.lat  =  45.805822 # 45°48′21″N
site.lon  = -90.079722 # 90°04′47″W

ens=1:3
n.ens=length(ens)
ens.mems=stringr::str_pad(ens, 3, "left", pad=0)
# ens.mems="bias-corrected"

# Set up the appropriate seeds to use when adding ensembles
set.seed(1159)
seed.vec <- sample.int(1e6, size=500, replace=F)
seed <- seed.vec[min(ens)] # This makes sure that if we add ensemble members, it gets a new, but reproducible seed

# Setting up some basics for the file structure
out.base <- file.path("../met_downscaled_day")
raw.base <- file.path("../met_raw", site.name)

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
ens.ID="GFDL_CM3_rcp45_r1i1p1"

# 1. Align daily aggregated NLDAS with Ameriflux 1 hr
path.nldas <- file.path(raw.base, "NLDAS_3hr")
path.GCM <- file.path(raw.base, ens.ID)

# We're now pulling an ensemble because we've set up the file paths and copied LDAS over 
# (even though all ensemble members will be identical here)
met.out <- align.met(train.path=path.nldas, source.path=path.GCM, yrs.train=NULL, yrs.source=NULL, n.ens=n.ens, seed=201811, pair.mems = FALSE, mems.train=paste(ens.ID, ens.mems, sep="_"))

# Calculate wind speed if it's not already there
# Note: right now only set up to do total windspeed and not north/east components
if(!"wind_speed" %in% names(met.out$dat.source)){
  met.out$dat.source$wind_speed <- sqrt(met.out$dat.source$eastward_wind^2 + met.out$dat.source$northward_wind^2)
}
if(!"wind_speed" %in% names(met.out$dat.train)){
  met.out$dat.train$wind_speed <- sqrt(met.out$dat.train$eastward_wind^2 + met.out$dat.train$northward_wind^2)
}

# 2. Pass the training & source met data into the bias-correction functions; this will get written to the ensemble
debias.met.regression(train.data=met.out$dat.train, source.data=met.out$dat.source, n.ens=1, 
                      vars.debias=NULL, CRUNCEP=FALSE,
                      pair.anoms = FALSE, pair.ens = FALSE, uncert.prop="mean", resids = FALSE, seed=Sys.Date(),
                      outfolder=file.path(out.base, ens.ID), 
                      yrs.save=NULL, ens.name=ens.ID, ens.mems=ens.mems, lat.in=site.lat, lon.in=site.lon,
                      save.diagnostics=TRUE, path.diagnostics=file.path(out.base, "bias_correct_qaqc"),
                      parallel = FALSE, n.cores = NULL, overwrite = TRUE, verbose = FALSE) 
# --------------------------


# -----------------------------------
# Double check the downscaling
# -----------------------------------
library(ggplot2)
vars.CF <- c("air_temperature_minimum", "air_temperature_maximum", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "surface_downwelling_longwave_flux_in_air", "air_pressure", "specific_humidity", "wind_speed")
vars.short <- c("tair", "precip", "swdown", "lwdown", "press", "qair", "wind")

# -------------------
# Raw data
# -------------------
met.out <- align.met(train.path=path.nldas, source.path=path.GCM, yrs.train=NULL, yrs.source=NULL, n.ens=n.ens, seed=201811, pair.mems = FALSE, mems.train=paste(ens.ID, ens.mems, sep="_"))

met.raw <- data.frame(met.out$dat.train$time)
met.raw$dataset <- "NLDAS"
met.raw$tair <- met.out$dat.train$air_temperature[,1]
met.raw$precip   <- met.out$dat.train$precipitation_flux[,1]
met.raw$swdown   <- met.out$dat.train$surface_downwelling_shortwave_flux_in_air[,1]
met.raw$lwdown   <- met.out$dat.train$surface_downwelling_longwave_flux_in_air[,1]
met.raw$press    <- met.out$dat.train$air_pressure[,1]
met.raw$qair     <- met.out$dat.train$specific_humidity[,1]
met.raw$wind     <- met.out$dat.train$wind_speed[,1]


met.tmp <- data.frame(met.out$dat.source$time)
met.tmp$dataset <- ens.ID
met.tmp$tair <- met.out$dat.source$air_temperature[,1]
met.tmp$precip   <- met.out$dat.source$precipitation_flux[,1]
met.tmp$swdown   <- met.out$dat.source$surface_downwelling_shortwave_flux_in_air[,1]
met.tmp$lwdown   <- met.out$dat.source$surface_downwelling_longwave_flux_in_air[,1]
met.tmp$press    <- met.out$dat.source$air_pressure[,1]
met.tmp$qair     <- met.out$dat.source$specific_humidity[,1]
met.tmp$wind     <- sqrt(met.out$dat.source$eastward_wind[,1]^2 + met.out$dat.source$northward_wind[,1]^2)

met.raw <- rbind(met.raw, met.tmp)

# Aggregate to look at annual means
met.raw.yr1 <- aggregate(met.raw[,vars.short], by=met.raw[,c("Year", "dataset")], FUN=mean)
met.raw.yr1$dataset2 <- as.factor(met.raw.yr1$dataset)
for(i in 1:nrow(met.raw.yr1)){
  met.raw.yr1[i,"dataset"] <- stringr::str_split(met.raw.yr1[i,"dataset2"], "[.]")[[1]][1]
}
met.raw.yr1$dataset <- as.factor(met.raw.yr1$dataset)
summary(met.raw.yr1)

met.raw.yr <- stack(met.raw.yr1[,vars.short])
names(met.raw.yr) <- c("raw", "met.var")
met.raw.yr[,c("Year", "dataset", "dataset2")] <- met.raw.yr1[,c("Year", "dataset", "dataset2")]
summary(met.raw.yr)


library(ggplot2)
met.raw.doy1 <- aggregate(met.raw[,vars.short], by=met.raw[,c("DOY", "dataset")], FUN=mean, na.rm=T)
met.raw.doy1$dataset2 <- as.factor(met.raw.doy1$dataset)
for(i in 1:nrow(met.raw.doy1)){
  met.raw.doy1[i,"dataset"] <- stringr::str_split(met.raw.doy1[i,"dataset2"], "[.]")[[1]][1]
}
met.raw.doy1$dataset <- as.factor(met.raw.doy1$dataset)

met.raw.doy <- stack(met.raw.doy1[,vars.short])
names(met.raw.doy) <- c("raw", "met.var")
met.raw.doy[,c("DOY", "dataset", "dataset2")] <- met.raw.doy1[,c("DOY", "dataset", "dataset2")]
summary(met.raw.doy)


summary(met.raw.doy1)
# summary(met.bias.doy.mean)

png(file.path(path.day.base, "Raw_Annual.png"), height=8, width=10, units="in", res=220)
print(
  ggplot(data=met.raw.yr[,]) + facet_wrap(~met.var, scales="free_y") +
    geom_path(aes(x=Year, y=raw, color=dataset, group=dataset2), size=0.5) +
    # geom_vline(xintercept=c(1850, 1901, 2016), linetype="dashed") +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
)
dev.off()

png(file.path(path.day.base, "Raw_DOY.png"), height=8, width=10, units="in", res=220)
print(
  ggplot(data=met.raw.doy[,]) + facet_wrap(~met.var, scales="free_y") +
    geom_path(data=met.raw.doy[met.raw.doy$dataset=="NLDAS",], aes(x=DOY, y=raw), color="black", size=1) +
    geom_path(data=met.raw.doy[met.raw.doy$dataset!="NLDAS",], aes(x=DOY, y=raw, color=dataset, group=dataset2), size=0.5) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
)
dev.off()
# -------------------

# -------------------
# Bias-Corrected data
# -------------------
met.bias <- align.met(train.path=path.nldas, source.path=file.path(out.base, ens.ID), yrs.train=NULL, yrs.source=NULL, n.ens=n.ens, seed=201811, pair.mems = FALSE, mems.train=paste(ens.ID, ens.mems, sep="_"))

met.raw <- data.frame(met.bias$dat.train$time)
met.raw$dataset <- "NLDAS"
met.raw$tair <- met.bias$dat.train$air_temperature[,1]
met.raw$precip   <- met.bias$dat.train$precipitation_flux[,1]
met.raw$swdown   <- met.bias$dat.train$surface_downwelling_shortwave_flux_in_air[,1]
met.raw$lwdown   <- met.bias$dat.train$surface_downwelling_longwave_flux_in_air[,1]
met.raw$press    <- met.bias$dat.train$air_pressure[,1]
met.raw$qair     <- met.bias$dat.train$specific_humidity[,1]
met.raw$wind     <- met.bias$dat.train$wind_speed[,1]


met.tmp <- data.frame(met.bias$dat.source$time)
met.tmp$dataset <- ens.ID
met.tmp$tair <- met.bias$dat.source$air_temperature[,1]
met.tmp$precip   <- met.bias$dat.source$precipitation_flux[,1]
met.tmp$swdown   <- met.bias$dat.source$surface_downwelling_shortwave_flux_in_air[,1]
met.tmp$lwdown   <- met.bias$dat.source$surface_downwelling_longwave_flux_in_air[,1]
met.tmp$press    <- met.bias$dat.source$air_pressure[,1]
met.tmp$qair     <- met.bias$dat.source$specific_humidity[,1]
met.tmp$wind     <- sqrt(met.bias$dat.source$eastward_wind[,1]^2 + met.bias$dat.source$northward_wind[,1]^2)

met.raw <- rbind(met.raw, met.tmp)

# Aggregate to look at annual means
met.raw.yr1 <- aggregate(met.raw[,vars.short], by=met.raw[,c("Year", "dataset")], FUN=mean)
met.raw.yr1$dataset2 <- as.factor(met.raw.yr1$dataset)
for(i in 1:nrow(met.raw.yr1)){
  met.raw.yr1[i,"dataset"] <- stringr::str_split(met.raw.yr1[i,"dataset2"], "[.]")[[1]][1]
}
met.raw.yr1$dataset <- as.factor(met.raw.yr1$dataset)
summary(met.raw.yr1)

met.raw.yr <- stack(met.raw.yr1[,vars.short])
names(met.raw.yr) <- c("raw", "met.var")
met.raw.yr[,c("Year", "dataset", "dataset2")] <- met.raw.yr1[,c("Year", "dataset", "dataset2")]
summary(met.raw.yr)


library(ggplot2)
met.raw.doy1 <- aggregate(met.raw[,vars.short], by=met.raw[,c("DOY", "dataset")], FUN=mean, na.rm=T)
met.raw.doy1$dataset2 <- as.factor(met.raw.doy1$dataset)
for(i in 1:nrow(met.raw.doy1)){
  met.raw.doy1[i,"dataset"] <- stringr::str_split(met.raw.doy1[i,"dataset2"], "[.]")[[1]][1]
}
met.raw.doy1$dataset <- as.factor(met.raw.doy1$dataset)

met.raw.doy <- stack(met.raw.doy1[,vars.short])
names(met.raw.doy) <- c("raw", "met.var")
met.raw.doy[,c("DOY", "dataset", "dataset2")] <- met.raw.doy1[,c("DOY", "dataset", "dataset2")]
summary(met.raw.doy)


summary(met.raw.doy1)
# summary(met.bias.doy.mean)

png(file.path(path.day.base, "Raw_Annual.png"), height=8, width=10, units="in", res=220)
print(
  ggplot(data=met.raw.yr[,]) + facet_wrap(~met.var, scales="free_y") +
    geom_path(aes(x=Year, y=raw, color=dataset, group=dataset2), size=0.5) +
    # geom_vline(xintercept=c(1850, 1901, 2016), linetype="dashed") +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
)
dev.off()

png(file.path(path.day.base, "Raw_DOY.png"), height=8, width=10, units="in", res=220)
print(
  ggplot(data=met.raw.doy[,]) + facet_wrap(~met.var, scales="free_y") +
    geom_path(data=met.raw.doy[met.raw.doy$dataset=="NLDAS",], aes(x=DOY, y=raw), color="black", size=1) +
    geom_path(data=met.raw.doy[met.raw.doy$dataset!="NLDAS",], aes(x=DOY, y=raw, color=dataset, group=dataset2), size=0.5) +
    scale_x_continuous(expand=c(0,0)) +
    theme_bw()
)
dev.off()
# -------------------

# -----------------------------------




