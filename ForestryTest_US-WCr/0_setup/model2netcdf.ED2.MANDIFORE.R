#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##' Modified from Code to convert ED2.1's HDF5 output into the NACP Intercomparison format (ALMA using netCDF)
##'
##' @name model2netcdf.ED2
##' @title Code to convert ED2's -T- HDF5 output into netCDF format
##'
##' @param outdir Location of ED model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param pft_names Names of PFTs used in the run, vector
##' @param vars.T Tower-frequency variables to extract.  If NULL ignores any available T files
##' @param vars.E Monthly variables to extract.  If NULL ignores any E files.  Currently extracts site means
##' @export
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper, Toni Viskari, Istem Fer
## modified M. Dietze 07/08/12 modified S. Serbin 05/06/13
## refactored by Istem Fer on 03/2018
## further modified by S. Serbin 09/2018
## even further modify by C. Rollinson 11/2018
##'
model2netcdf.ED2 <- function(ed.dir, outdir, sitelat, sitelon, start_date,
                             end_date, vars.T=NULL, vars.E=NULL, pft_names = NULL) {
  
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  flist <- list()
  if(!is.null(vars.T)) flist[["-T-"]] <- dir(ed.dir, "-T-") # tower files
  if(!is.null(vars.E)) flist[["-E-"]] <- dir(ed.dir, "-E-") # monthly files
  
  # check if there are files
  file.check <- sapply(flist, function (f) length(f) != 0)
  
  if (!any(file.check)) {
    
    # no output files
    # PEcAn.logger::logger.warn("WARNING: No output files found for :", outdir)
    warning("WARNING: No output files found for :", ed.dir)
    return(NULL)
    
  } else {
    
    # which output files are there
    ed_res_flag <- names(flist)[file.check]
    
    # extract year info from the file names
    ylist <- lapply(ed_res_flag, function(f) {
      yr <- rep(NA, length(flist[[f]]))
      for (i in seq_along(flist[[f]])) {
        index <- gregexpr(f, flist[[f]][i])[[1]]
        index <- index[1]
        yr[i] <- as.numeric(substr(flist[[f]][i], index + 3, index + 6))
      }
      return(yr)
    })
    
    names(ylist) <- ed_res_flag
  }
  
  # prepare list to collect outputs
  out_list <- vector("list", length(ed_res_flag))
  names(out_list) <- ed_res_flag
  
  # if run failed there might be less years, no output case is handled above
  # we can process whatever is there
  # but of course this upsets ensemble.ts because the outputs are not of
  # same length now
  # two options:
  # (i)  don't process anything
  #      return(NULL)
  # (ii) check whether this is an ensemble run, then return null, otherwise
  # process whatever there is
  # for now I'm going with this, do failed runs also provide information
  # on parameters?
  year_check <- unique(unlist(ylist))
  if (max(year_check) < end_year) {
    warning("Run failed with some outputs.")
    rundir <- gsub(file.path("out"), file.path("run"), outdir)
    readme <- file(file.path(rundir, "README.txt"))
    runtype <- readLines(readme, n = 1)
    close(readme)
    if (grepl("ensemble", runtype)) {
      # PEcAn.logger::logger.info("This is an ensemble run.
      #                           Not processing anything.")
      warning("This is an ensemble run. Not processing anything.")
      
      return(NULL)
    } else {
      # PEcAn.logger::logger.info("This is not an ensemble run.
      #                           Processing existing outputs.")
      warning("This is an ensemble run. Processing existing outputs.")
      end_year <- max(year_check)
    }
  }
  
  # ----- start loop over years
  for (y in start_year:end_year) {
    
    # PEcAn.logger::logger.info(paste0("----- Processing year: ", y))
    print(paste0("----- Processing year: ", y))
    
    # ----- read values from ED output files
    for (i in seq_along(out_list)) {
      rflag <- ed_res_flag[i]
      fcnx  <- paste0("read_", gsub("-", "", rflag), "_files")
      fcn   <- match.fun(fcnx)
      out_list[[rflag]] <- fcn(yr = y, ylist[[rflag]], flist[[rflag]],
                               ed.dir, start_date, end_date,
                               pft_names)
    }
    
    # generate start/end dates for processing
    if (y == strftime(start_date, "%Y")) {
      begin_date <- base::as.Date(strftime(start_date))
    } else {
      begin_date <- base::as.Date(paste0(y, "-01-01"))
    }
    
    if (y == strftime(end_date, "%Y")) {
      ends <- as.numeric(strftime(end_date, "%j"))
    } else {
      ends <- as.numeric(strftime(paste0(y, "-12-31"), "%j"))
    }
    
    # create lat/long nc variables
    lat <- ncdf4::ncdim_def("lat", "degrees_north",
                            vals = as.numeric(sitelat),
                            longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",
                            vals = as.numeric(sitelon),
                            longname = "station_longitude")
    
    # ----- put values to nc_var list
    nc_var <- list()
    for (i in seq_along(out_list)) {
      rflag   <- ed_res_flag[i]
      fcnx    <- paste0("put_", gsub("-", "", rflag), "_values")
      fcn     <- match.fun(fcnx)
      put_out <- fcn(yr = y, nc_var = nc_var, out = out_list[[rflag]],
                     lat = lat, lon = lon, begins = begin_date,
                     ends = ends, pft_names)
      
      nc_var            <- put_out$nc_var
      out_list[[rflag]] <- put_out$out
    }
    
    # ----- write ncdf files
    print("*** Writing netCDF file ***")
    
    # out <- unlist(out_list, recursive = FALSE)
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")),
                           nc_var)
    # define time_bouds for -T- outputs, if exists
    if (!is.null(vars.T) ) {
      ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec = NA)
    }
    # define time_bouds for -E- outputs, if exists
    if (!is.null(vars.E) & file.check[["-E-"]]==TRUE) {
      ncdf4::ncatt_put(nc, "dtime", "bounds", "dtime_bounds", prec = NA)
    }
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (VAR in names(nc_var)) {
      if(VAR=="DTime") next
      # print(VAR)
      out_list[["-E-"]][[VAR]][is.na(out_list[["-E-"]][[VAR]])] <- -999
      ncdf4::ncvar_put(nc, nc_var[[VAR]], out_list[["-E-"]][[VAR]])
      cat(paste(nc_var[[VAR]]$name, nc_var[[VAR]]$longname), file = varfile,
          sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  } # end year-loop
  
  } # model2netcdf.ED2
##-------------------------------------------------------------------------------------------------#



##-------------------------------------------------------------------------------------------------#

##' Function for reading -T- files
##'
##' @details
##'  e.g.    yr = 1999
##'      yfiles = 1999 2000
##'      tfiles = "analysis-T-1999-00-00-000000-g01.h5" "analysis-T-2000-00-00-000000-g01.h5"
##'
##' @param yr the year being processed
##' @param yfiles the years on the filenames, will be used to matched tfiles for that year
##' @export
read_T_files <- function(yr, yfiles, tfiles, outdir, start_date, end_date, vars.T, ...){
  
  # PEcAn.logger::logger.info(paste0("*** Reading -T- file ***"))
  print(paste0("*** Reading -T- file ***"))
  
  # add
  add <- function(dat, col, row, year) {
    ## data is always given for whole year, except it will start always at 0
    ## the left over data is filled with 0's
    if (year == strftime(start_date, "%Y")) {
      start <- (as.numeric(strftime(start_date, "%j")) - 1) * block
    } else {
      start <- 0
    }
    if (year == strftime(end_date, "%Y")) {
      end <- as.numeric(strftime(end_date, "%j")) * block
    } else {
      end <- as.numeric(strftime(paste0(year, "-12-31"), "%j")) * block
    }
    
    dims <- dim(dat)
    if (is.null(dims)) {
      if (length(dat) == 1) {
        if (length(out) < col) {
          out[[col]] <- array(dat, dim = (end - start))
        } else {
          if (start != 0) {
            # PEcAn.logger::logger.warn("start date is not 0 this year, 
            #                           but data already exists in this col", 
            #                           col, "how is this possible?")
            warning("start date is not 0 this year, but data already exists in this col",
                    col, "how is this possible?")
            
          }
          out[[col]] <- abind::abind(out[[col]], 
                                     array(dat, dim = (end - start)), 
                                     along = 1)
        }
        } else {
          # PEcAn.logger::logger.warn("expected a single value")
          warning("expected a single value")
        }
    } else if (length(dims) == 1) {
      dat <- dat[1:(end - start)]
      if (length(out) < col) {
        out[[col]] <- dat
      } else {
        if (start != 0) {
          # PEcAn.logger::logger.warn("start date is not 0 this year, 
          #                           but data already exists in this col",
          #                           col, "how is this possible?")
          warning("start date is not 0 this year, 
                                    but data already exists in this col",
                                    col, "how is this possible?")
          
        }
        out[[col]] <- abind::abind(out[[col]], dat, along = 1)
      }
      } else if (length(dims) == 2) {
        dat <- t(dat)
        dims <- dim(dat)
        dat <- dat[1:(end - start), ]
        if (length(out) < col) {
          out[[col]] <- dat
        } else {
          if (start != 0) {
            # PEcAn.logger::logger.warn("start date is not 0 this year, 
            #                           but data already exists in this 
            #                           col", col, "how is this possible?")
            warning("start date is not 0 this year, 
                                      but data already exists in this 
                                      col", col, "how is this possible?")
            
          }
          out[[col]] <- abind::abind(out[[col]], dat, along = 1)
          }
        } else {
          # PEcAn.logger::logger.debug("-------------------------------------------------------------")
          # PEcAn.logger::logger.debug("col=", col)
          # PEcAn.logger::logger.debug("length=", length(dat))
          # PEcAn.logger::logger.debug("start=", start)
          # PEcAn.logger::logger.debug("end=", end)
          # PEcAn.logger::logger.debug("dims=", dims)
          # PEcAn.logger::logger.warn("Don't know how to handle larger arrays yet.")
          warning("-------------------------------------------------------------")
          warning("col=", col)
          warning("length=", length(dat))
          warning("start=", start)
          warning("end=", end)
          warning("dims=", dims)
          warning("Don't know how to handle larger arrays yet.")
          
        }
    
    ## finally make sure we use -999 for invalid values
    out[[col]][is.null(out[[col]])] <- -999
    out[[col]][is.na(out[[col]])] <- -999
    
    return(out)
      } # end add-function
  
  
  getHdf5Data <- function(nc, var) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    } else {
      warning("Could not find", var, "in ed hdf5 output.")
      return(-999)
    }
  }
  
  CheckED2Version <- function(nc) {
    if ("FMEAN_BDEAD_PY" %in% names(nc$var)) {
      return("Git")
    }
  }
  
  # note that there is always one Tower file per year
  ysel <- which(yr == yfiles)
  
  if (yr < strftime(start_date, "%Y")) {
    warning(yr, "<", strftime(start_date, "%Y"))
    next
  }
  
  if (yr > strftime(end_date, "%Y")) {
    warning(yr, ">", strftime(end_date, "%Y"))
    next
  }
  
  n <- length(ysel)
  out <- list()
  row <- 1
  
  # note that there is always one Tower file per year
  ncT <- ncdf4::nc_open(file.path(outdir, tfiles[ysel]))
  
  ## determine timestep from HDF5 file
  block <- ifelse(lubridate::leap_year(yr) == TRUE,
                  ncT$dim$phony_dim_0$len / 366, # a leaper 
                  ncT$dim$phony_dim_0$len / 365) # non leap
  
  print(paste0("Output interval: ", 86400 / block, " sec"))
  
  
  if (file.exists(file.path(outdir, sub("-T-", "-Y-", tfiles[ysel])))) {
    ncY <- ncdf4::nc_open(file.path(outdir, sub("-T-", "-Y-", tfiles[ysel])))
    slzdata <- getHdf5Data(ncY, "SLZ")
    ncdf4::nc_close(ncY)
  } else {
    warning("Could not find SLZ in Y file, 
                              making a crude assumpution.")
    slzdata <- array(c(-2, -1.5, -1, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05))
  }
  
  ## Check for which version of ED2 we are using.
  ED2vc <- CheckED2Version(ncT)
  
  ## store for later use, will only use last data
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  if (!is.null(ED2vc)) {
    ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row, yr) ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "FMEAN_BDEAD_PY"), 1, row, yr)  ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 2, row, yr)  ## AutoResp
    out <- add(-999, 3, row, yr)  ## CarbPools
    out <- add(getHdf5Data(ncT, "FMEAN_CAN_CO2_PY"), 4, row, yr)  ## CO2CAS
    out <- add(-999, 5, row, yr)  ## CropYield
    out <- add(getHdf5Data(ncT, "FMEAN_GPP_PY"), 6, row, yr)  ## GPP
    out <- add(getHdf5Data(ncT, "FMEAN_RH_PY"), 7, row, yr)  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "FMEAN_GPP_PY") + getHdf5Data(ncT, "FMEAN_PLRESP_PY") + 
                 getHdf5Data(ncT, "FMEAN_RH_PY"), 8, row, yr)  ## NEE
    out <- add(getHdf5Data(ncT, "FMEAN_GPP_PY") - getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 
               9, row, yr)  ## NPP
    out <- add(getHdf5Data(ncT, "FMEAN_RH_PY") + getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 
               10, row, yr)  ## TotalResp
    ## out <- add(getHdf5Data(ncT, 'BDEAD + getHdf5Data(ncT, 'BALIVE,11,row, yr) ## TotLivBiom
    out <- add(-999, 11, row, yr)  ## TotLivBiom
    out <- add(getHdf5Data(ncT, "FAST_SOIL_C_PY") + getHdf5Data(ncT, "STRUCT_SOIL_C_PY") + 
                 getHdf5Data(ncT, "SLOW_SOIL_C_PY"), 12, row, yr)  ## TotSoilCarb
    
    ## depth from surface to frozen layer
    tdepth <- 0
    fdepth <- 0
    soiltemp <- getHdf5Data(ncT, "FMEAN_SOIL_TEMP_PY")
    if (length(dim(soiltemp)) == 3) {
      fdepth <- array(0, dim = dim(soiltemp)[1:2])
      tdepth <- array(0, dim = dim(soiltemp)[1:2])
      for (t in 1:dim(soiltemp)[1]) { # time
        for (p in 1:dim(soiltemp)[2]) { # polygon
          for (i in dim(soiltemp)[3]:2) { # depth
            if (fdepth[t, p] == 0 & soiltemp[t, p, i] < 273.15 & 
                soiltemp[t, p, i - 1] > 273.13) {
              fdepth[t, p] <- i
            }
            if (tdepth[t, p] == 0 & soiltemp[t, p, i] > 273.15 & 
                soiltemp[t, p, i - 1] < 273.13) {
              tdepth[t, p] <- i
            }
          }
          SLZ <- c(slzdata[t, ], 0)
          z1 <- (SLZ[fdepth[t, p] + 1] + SLZ[fdepth[t, p]]) / 2
          z2 <- (SLZ[fdepth[t, p]] + SLZ[fdepth[t, p] - 1]) / 2
          if (fdepth[t, p] > 0) {
            fdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, fdepth[t, p]]) / 
              (soiltemp[t, p, fdepth[t, p] - 1] - soiltemp[t, p, fdepth[t, p]])
          }
          if (tdepth[t, p] > 0) {
            SLZ <- c(slzdata[t, ], 0)
            z1 <- (SLZ[tdepth[t, p] + 1] + SLZ[tdepth[t, p]]) / 2
            z2 <- (SLZ[tdepth[t, p]] + SLZ[tdepth[t, p] - 1]) / 2
            tdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, tdepth[t, p]]) / 
              (soiltemp[t, p, tdepth[t, p] - 1] - soiltemp[t, p, tdepth[t, p]])
          }
        }
      }
    } else {
      # no polygons, just time vs depth?
      fdepth <- array(0, ncol(soiltemp))
      tdepth <- array(0, ncol(soiltemp))
      for (t in 1:ncol(soiltemp)) { # time
        for (d in 2:nrow(soiltemp)) { # depth
          if (fdepth[t] == 0 & soiltemp[d, t] < 273.15 & soiltemp[d - 1, t] > 273.13) {
            fdepth[t] <- d
          }
          if (tdepth[t] == 0 & soiltemp[d, t] > 273.15 & soiltemp[d - 1, t] < 273.13) {
            tdepth[t] <- d
          }
        }
        if (fdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[fdepth[t] + 1] + SLZ[fdepth[t]]) / 2
          z2 <- (SLZ[fdepth[t]] + SLZ[fdepth[t] - 1]) / 2
          fdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[fdepth[t], t]) / 
            (soiltemp[fdepth[t] - 1, t] - soiltemp[fdepth[t], t])
        }
        if (tdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[tdepth[t] + 1] + SLZ[tdepth[t]]) / 2
          z2 <- (SLZ[tdepth[t]] + SLZ[tdepth[t] - 1]) / 2
          tdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[tdepth[t], t]) / 
            (soiltemp[tdepth[t] - 1, t] - soiltemp[tdepth[t], t])
        }
      }
    }
    
    out <- add(fdepth, 13, row, yr)  ## Fdepth
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_DEPTH_PY"), 14, row, yr)  ## SnowDepth (ED2 currently groups snow in to surface water)
    out <- add(1 - getHdf5Data(ncT, "FMEAN_SFCW_FLIQ_PY"), 15, row, yr)  ## SnowFrac (ED2 currently groups snow in to surface water)
    out <- add(tdepth, 16, row, yr)  ## Tdepth
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_CO2_PY"), 17, row, yr)  ## CO2air
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_RLONG_PY"), 18, row, yr)  ## Lwdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_PRSS_PY"), 19, row, yr)  ## Psurf
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_SHV_PY"), 20, row, yr)  ## Qair
    out <- add(getHdf5Data(ncT, "FMEAN_PCPG_PY"), 21, row, yr)  ## Rainf
    ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
    ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
    ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
    ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row, yr) ## Swdown
    ##out <- add(getHdf5Data(ncT, 'FMEAN_PAR_L_BEAM_PY')+
    ##           getHdf5Data(ncT, 'FMEAN_PAR_L_DIFF_PY'),22,row, yr) ## Swdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_PAR_PY"), 22, row, yr)  ## Swdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_TEMP_PY"), 23, row, yr)  ## Tair
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_VELS_PY"), 24, row, yr)  ## Wind
    ## out <- add(getHdf5Data(ncT, 'FMEAN_ATM_RLONG_PY')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row,
    ## yr) ## Lwnet
    out <- add(-999, 25, row, yr)  ## Lwnet
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT,
    ## 'AVG_VAPOR_GC')*2272000,26,row, yr) ## Qg
    out <- add(-999, 26, row, yr)  ## Qg
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row, yr) ## Qh
    out <- add(getHdf5Data(ncT, "FMEAN_SENSIBLE_AC_PY"), 27, row, yr)  ## Qh
    out <- add(getHdf5Data(ncT, "FMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "FMEAN_VAPOR_WC_PY") + 
                 getHdf5Data(ncT, "FMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 28, row, yr)  ## Qle
    out <- add(-999, 29, row, yr)  ## Swnet
    out <- add(-999, 30, row, yr)  ## RootMoist
    out <- add(getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 31, row, yr)  ## Tveg
    out <- add(getHdf5Data(ncT, "ZBAR"), 32, row, yr)  ## WaterTableD
    out <- add(-999, 33, row, yr)  ## fPAR
    ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
    ## out <- add(lai,34,row, yr) ## LAI******************
    ## out <- add(getHdf5Data(ncT, 'FMEAN_LAI_PY'),34,row, yr) ## LAI - no longer using FMEAN LAI
    
    ## OLD - to be deprecated
    #laidata <- getHdf5Data(ncT,"LAI_PY")
    #if(length(dim(laidata)) == 3){
    #  out <- add(apply(laidata,3,sum),34,row,yr)
    #} else {
    #  out <- add(-999,34,row, yr)
    #}
    
    # code changes proposed by MCD, tested by SPS 20160607
    laidata <- getHdf5Data(ncT, "LAI_PY")
    if (length(dim(laidata)) == 3) {
      laidata <- apply(laidata, 3, sum)
      out <- add(array(laidata, dim = length(laidata)), 34, row, yr)
    } else {
      out <- add(-999, 34, row, yr)
    }
    
    ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
    fliq <- NA  #getHdf5Data(ncT, 'FMEAN_SFCW_FLIQ_PY')
    out <- add(1 - fliq, 35, row, yr)  ## SMFrozFrac
    out <- add(fliq, 36, row, yr)  ## SMLiqFrac
    ## This needs to be soil wetness, i.e. multilple levels deep
    out <- add(getHdf5Data(ncT, "FMEAN_SOIL_WATER_PY"), 37, row, yr)  ## SoilWater  **********
    ## out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
    out <- add(soiltemp, 38, row, yr)  ## SoilTemp
    out <- add(-999, 39, row, yr)  ## SoilWet
    out <- add(getHdf5Data(ncT, "FMEAN_ALBEDO_PY"), 40, row, yr)  ## Albedo
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_TEMP_PY"), 41, row, yr)  ## SnowT (ED2 currently groups snow in to surface water)
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_MASS_PY"), 42, row, yr)  ## SWE (ED2 currently groups snow in to surface water)
    out <- add(getHdf5Data(ncT, "FMEAN_LEAF_TEMP_PY"), 43, row, yr)  ## VegT
    out <- add(getHdf5Data(ncT, "FMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "FMEAN_VAPOR_WC_PY") + 
                 getHdf5Data(ncT, "FMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 44, 
               row, yr)  ## Evap
    out <- add(getHdf5Data(ncT, "FMEAN_QRUNOFF_PY"), 45, row, yr)  ## Qs
    out <- add(getHdf5Data(ncT, "BASEFLOW"), 46, row, yr)  ## Qsb
    
    out <- add(getHdf5Data(ncT, "FMEAN_ROOT_RESP_PY") + getHdf5Data(ncT, "FMEAN_ROOT_GROWTH_RESP_PY") + 
                 getHdf5Data(ncT, "FMEAN_RH_PY"), 47, row, yr)  ## SoilResp
    out$SLZ <- slzdata
    
  } else {
    ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row, yr) ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "AVG_BDEAD"), 1, row, yr)  ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "AVG_PLANT_RESP"), 2, row, yr)  ## AutoResp
    out <- add(-999, 3, row, yr)  ## CarbPools
    out <- add(getHdf5Data(ncT, "AVG_CO2CAN"), 4, row, yr)  ## CO2CAS
    out <- add(-999, 5, row, yr)  ## CropYield
    out <- add(getHdf5Data(ncT, "AVG_GPP"), 6, row, yr)  ## GPP
    out <- add(getHdf5Data(ncT, "AVG_HTROPH_RESP"), 7, row, yr)  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "AVG_GPP") + getHdf5Data(ncT, "AVG_PLANT_RESP") + getHdf5Data(ncT, 
                                                                                               "AVG_HTROPH_RESP"), 8, row, yr)  ## NEE
    out <- add(getHdf5Data(ncT, "AVG_GPP") - getHdf5Data(ncT, "AVG_PLANT_RESP"), 9, row, 
               yr)  ## NPP
    out <- add(getHdf5Data(ncT, "AVG_HTROPH_RESP") + getHdf5Data(ncT, "AVG_PLANT_RESP"), 
               10, row, yr)  ## TotalResp
    ## out <- add(getHdf5Data(ncT, 'AVG_BDEAD + getHdf5Data(ncT, 'AVG_BALIVE,11,row, yr) ##
    ## TotLivBiom
    out <- add(-999, 11, row, yr)  ## TotLivBiom
    out <- add(getHdf5Data(ncT, "AVG_FSC") + getHdf5Data(ncT, "AVG_STSC") + 
                 getHdf5Data(ncT, "AVG_SSC"), 12, row, yr)  ## TotSoilCarb
    ## depth from surface to frozen layer
    tdepth <- 0
    fdepth <- 0
    soiltemp <- getHdf5Data(ncT, "AVG_SOIL_TEMP")
    if (length(dim(soiltemp)) == 3) {
      fdepth <- array(0, dim = dim(soiltemp)[1:2])
      tdepth <- array(0, dim = dim(soiltemp)[1:2])
      for (t in 1:dim(soiltemp)[1]) { # time
        for (p in 1:dim(soiltemp)[2]) { # polygon
          for (i in dim(soiltemp)[3]:2) { # depth
            if (fdepth[t, p] == 0 & soiltemp[t, p, i] < 273.15 & 
                soiltemp[t, p, i - 1] > 273.13) {
              fdepth[t, p] <- i
            }
            if (tdepth[t, p] == 0 & soiltemp[t, p, i] > 273.15 & 
                soiltemp[t, p, i - 1] < 273.13) {
              tdepth[t, p] <- i
            }
          }
          SLZ <- c(slzdata[t, ], 0)
          z1 <- (SLZ[fdepth[t, p] + 1] + SLZ[fdepth[t, p]]) / 2
          z2 <- (SLZ[fdepth[t, p]] + SLZ[fdepth[t, p] - 1]) / 2
          if (fdepth[t, p] > 0) {
            fdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, fdepth[t, p]]) / 
              (soiltemp[t, p, fdepth[t, p] - 1] - soiltemp[t, p, fdepth[t, p]])
          }
          if (tdepth[t, p] > 0) {
            SLZ <- c(slzdata[t, ], 0)
            z1 <- (SLZ[tdepth[t, p] + 1] + SLZ[tdepth[t, p]]) / 2
            z2 <- (SLZ[tdepth[t, p]] + SLZ[tdepth[t, p] - 1]) / 2
            tdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, tdepth[t, p]]) / 
              (soiltemp[t, p, tdepth[t, p] - 1] - soiltemp[t, p, tdepth[t, p]])
          }
        }
      }
    } else {
      # no polygons, just time vs depth?
      fdepth <- array(0, ncol(soiltemp))
      tdepth <- array(0, ncol(soiltemp))
      for (t in 1:ncol(soiltemp)) { # time
        for (d in 2:nrow(soiltemp)) { # depth
          if (fdepth[t] == 0 & soiltemp[d, t] < 273.15 & soiltemp[d - 1, t] > 273.13) {
            fdepth[t] <- d
          }
          if (tdepth[t] == 0 & soiltemp[d, t] > 273.15 & soiltemp[d - 1, t] < 273.13) {
            tdepth[t] <- d
          }
        }
        if (fdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[fdepth[t] + 1] + SLZ[fdepth[t]]) / 2
          z2 <- (SLZ[fdepth[t]] + SLZ[fdepth[t] - 1]) / 2
          fdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[fdepth[t], t]) / 
            (soiltemp[fdepth[t] - 1, t] - soiltemp[fdepth[t], t])
        }
        if (tdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[tdepth[t] + 1] + SLZ[tdepth[t]]) / 2
          z2 <- (SLZ[tdepth[t]] + SLZ[tdepth[t] - 1]) / 2
          tdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[tdepth[t], t]) / 
            (soiltemp[tdepth[t] - 1, t] - soiltemp[tdepth[t], t])
        }
      }
    }
    
    out <- add(fdepth, 13, row, yr)  ## Fdepth
    out <- add(getHdf5Data(ncT, "AVG_SNOWDEPTH"), 14, row, yr)  ## SnowDepth
    out <- add(1 - getHdf5Data(ncT, "AVG_SNOWFRACLIQ"), 15, row, yr)  ## SnowFrac
    out <- add(tdepth, 16, row, yr)  ## Tdepth
    out <- add(getHdf5Data(ncT, "AVG_ATM_CO2"), 17, row, yr)  ## CO2air
    out <- add(getHdf5Data(ncT, "AVG_RLONG"), 18, row, yr)  ## Lwdown
    out <- add(getHdf5Data(ncT, "AVG_PRSS"), 19, row, yr)  ## Psurf
    out <- add(getHdf5Data(ncT, "AVG_ATM_SHV"), 20, row, yr)  ## Qair
    out <- add(getHdf5Data(ncT, "AVG_PCPG"), 21, row, yr)  ## Rainf
    ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
    ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
    ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
    ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row, yr) ## Swdown
    out <- add(getHdf5Data(ncT, "AVG_PAR_BEAM") + getHdf5Data(ncT, "AVG_PAR_DIFFUSE"), 
               22, row, yr)  ## Swdown
    out <- add(getHdf5Data(ncT, "AVG_ATM_TMP"), 23, row, yr)  ## Tair
    out <- add(getHdf5Data(ncT, "AVG_VELS"), 24, row, yr)  ## Wind
    ##out <- add(getHdf5Data(ncT, 'AVG_RLONG')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row, yr) ## Lwnet
    out <- add(-999, 25, row, yr)  ## Lwnet
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT,
    ## 'AVG_VAPOR_GC')*2272000,26,row, yr) ## Qg
    out <- add(-999, 26, row, yr)  ## Qg
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row, yr) ## Qh
    out <- add(getHdf5Data(ncT, "AVG_SENSIBLE_AC"), 27, row, yr)  ## Qh
    out <- add(getHdf5Data(ncT, "AVG_EVAP"), 28, row, yr)  ## Qle
    out <- add(-999, 29, row, yr)  ## Swnet
    out <- add(-999, 30, row, yr)  ## RootMoist
    out <- add(getHdf5Data(ncT, "AVG_TRANSP"), 31, row, yr)  ## Tveg
    out <- add(getHdf5Data(ncT, "ZBAR"), 32, row, yr)  ## WaterTableD
    out <- add(-999, 33, row, yr)  ## fPAR
    ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
    ## out <- add(lai,34,row, yr) ## LAI******************
    out <- add(getHdf5Data(ncT, "LAI"), 34, row, yr)  ## LAI
    ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
    fliq <- NA  #getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')
    out <- add(1 - fliq, 35, row, yr)  ## SMFrozFrac
    out <- add(fliq, 36, row, yr)  ## SMLiqFrac
    ## This needs to be soil wetness, i.e. multilple levels deep
    out <- add(getHdf5Data(ncT, "AVG_SOIL_WATER"), 37, row, yr)  ## SoilWater  **********
    ## out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
    out <- add(soiltemp, 38, row, yr)  ## SoilTemp
    out <- add(-999, 39, row, yr)  ## SoilWet
    out <- add(getHdf5Data(ncT, "AVG_ALBEDO"), 40, row, yr)  ## Albedo
    out <- add(getHdf5Data(ncT, "AVG_SNOWTEMP"), 41, row, yr)  ## SnowT
    out <- add(getHdf5Data(ncT, "AVG_SNOWMASS"), 42, row, yr)  ## SWE
    out <- add(getHdf5Data(ncT, "AVG_VEG_TEMP"), 43, row, yr)  ## VegT
    out <- add(getHdf5Data(ncT, "AVG_EVAP") + getHdf5Data(ncT, "AVG_TRANSP"), 44, row, 
               yr)  ## Evap
    out <- add(getHdf5Data(ncT, "AVG_RUNOFF"), 45, row, yr)  ## Qs
    out <- add(getHdf5Data(ncT, "BASEFLOW"), 46, row, yr)  ## Qsb     
    out <- add(getHdf5Data(ncT, "AVG_ROOT_RESP") + getHdf5Data(ncT, "AVG_ROOT_MAINTENANCE") + 
                 getHdf5Data(ncT, "AVG_HTROPH_RESP"), 47, row, yr)  ## SoilResp
    out$SLZ <- slzdata
  }
  
  ncdf4::nc_close(ncT)
  
  return(out)
  
  } # read_T_files


##-------------------------------------------------------------------------------------------------#

##' Function for put -T- values to nc_var list
##' @export
put_T_values <- function(yr, nc_var, out, lat, lon, begins, ends, ...){
  
  s <- length(nc_var)
  
  ## Conversion factor for umol C -> kg C
  Mc <- 12.017  #molar mass of C, g/mol
  umol2kg_C <- Mc * udunits2::ud.convert(1, "umol", "mol") * udunits2::ud.convert(1, "g", "kg")
  yr2s      <- udunits2::ud.convert(1, "s", "yr")
  
  # TODO - remove this function and replace with ifelse statements inline below (SPS)
  conversion <- function(col, mult) {
    ## make sure only to convert those values that are not -999
    out[[col]][out[[col]] != -999] <- out[[col]][out[[col]] != -999] * mult
    return(out)
  }
  
  checkTemp <- function(col) {
    out[[col]][out[[col]] == 0] <- -999
    return(out)
  }
  
  
  # ----- define ncdf dimensions
  #### setup output time and time bounds
  ## Create a date vector that contains each day of the model run for each output year (e.g. "2001-07-15", "2001-07-16"....)
  ## and which is the correct length for each full or partial year
  output_date_vector <- seq(begins, by = "day", length.out = length(lubridate::yday(begins):ends))
  ## Calculate model output frequency per day (e.g. 0.02083333)
  model_timestep_s <- length(output_date_vector) / length(out[[1]])
  iter_per_day <- round(1 / model_timestep_s) ## e.g. 48
  ## Create a timesteps vector (e.g. 0.00000000 0.02083333 0.04166667 0.06250000 0.08333333 0.10416667 ...)
  timesteps <- head(seq(0, 1, by = 1 / iter_per_day), -1)
  ## Create a new date vector where each day is repeated by iter_per_day 
  ## (e.g. "2001-07-15" "2001-07-15" "2001-07-15" "2001-07-15" "2001-07-15" ...)
  sub_dates <- rep(output_date_vector, each = iter_per_day)
  ## Generate a vector of julian dates from sub_dates (e.g. 196 196 196 196 196 196 ...)
  jdates <- lubridate::yday(sub_dates)
  ## Create a fractional DOY vector using jdates, subtract by 1 to be 0 index
  ## (e.g. 195.0000 195.0208 195.0417 195.0625 195.0833 195.1042)
  ## which yields, e.g. as.Date(195.0000,origin="2001-01-01"), "2001-07-15" 
  tvals <- (jdates + timesteps) - 1
  ## Create time bounds to populate time_bounds variable
  bounds <- array(data = NA, dim = c(length(tvals), 2))
  bounds[, 1] <- tvals
  bounds[, 2] <- bounds[, 1] + (1 / iter_per_day)
  bounds <- round(bounds, 4)  # create time bounds for each timestep in t, t+1; t+1, t+2... format
  ####
  
  t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", yr, "-01-01 00:00:00"), 
                        vals = tvals, 
                        calendar = "standard", unlim = TRUE)
  time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                    longname = "history time interval endpoint dimensions", 
                                    vals = 1:2, units = "")
  
  slzdata <- out$SLZ
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  zg <- ncdf4::ncdim_def("SoilLayerMidpoint", "meters", c(slzdata[1:length(dz)] + dz / 2, 0))
  
  # currently unused
  #dims  <- list(lon = lon, lat = lat, time = t)
  #dimsz <- list(lon = lon, lat = lat, time = t, nsoil = zg)
  
  # ----- fill list
  
  out <- conversion(1, udunits2::ud.convert(1, "t ha-1", "kg m-2"))  ## tC/ha -> kg/m2
  nc_var[[s + 1]] <- ncdf4::ncvar_def("AbvGrndWood", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Above ground woody biomass")
  out <- conversion(2, umol2kg_C)  ## umol/m2 s-1 -> kg/m2 s-1
  nc_var[[s + 2]] <- ncdf4::ncvar_def("AutoResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Autotrophic Respiration")
  nc_var[[s + 3]] <- ncdf4::ncvar_def("CarbPools", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Size of each carbon pool")
  nc_var[[s + 4]] <- ncdf4::ncvar_def("CO2CAS", units = "ppmv", dim = list(lon, lat, t), missval = -999, 
                                      longname = "CO2CAS")
  nc_var[[s + 5]] <- ncdf4::ncvar_def("CropYield", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Crop Yield")
  out <- conversion(6, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 6]] <- ncdf4::ncvar_def("GPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Gross Primary Productivity")
  out <- conversion(7, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 7]] <- ncdf4::ncvar_def("HeteroResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Heterotrophic Respiration")
  out <- conversion(8, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 8]] <-  ncdf4::ncvar_def("NEE", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Net Ecosystem Exchange")
  out <- conversion(9, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 9]] <- ncdf4::ncvar_def("NPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Net Primary Productivity")
  out <- conversion(10, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 10]] <- ncdf4::ncvar_def("TotalResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Respiration")
  nc_var[[s + 11]] <- ncdf4::ncvar_def("TotLivBiom", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total living biomass")
  nc_var[[s + 12]] <- ncdf4::ncvar_def("TotSoilCarb", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Soil Carbon")
  nc_var[[s + 13]] <- ncdf4::ncvar_def("Fdepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Frozen Thickness")
  nc_var[[s + 14]] <- ncdf4::ncvar_def("SnowDepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total snow depth")
  nc_var[[s + 15]] <- PEcAn.utils::mstmipvar("SnowFrac", lat, lon, t, zg) # not standard
  nc_var[[s + 16]] <- ncdf4::ncvar_def("Tdepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Active Layer Thickness")
  nc_var[[s + 17]] <- ncdf4::ncvar_def("CO2air", units = "umol mol-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface CO2 concentration")
  nc_var[[s + 18]] <- ncdf4::ncvar_def("LWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Surface incident longwave radiation")
  nc_var[[s + 19]] <- ncdf4::ncvar_def("Psurf", units = "Pa", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Surface pressure")
  nc_var[[s + 20]] <- ncdf4::ncvar_def("Qair", units = "kg kg-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface specific humidity")
  nc_var[[s + 21]] <- ncdf4::ncvar_def("Rainf", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Rainfall rate")
  nc_var[[s + 22]] <- ncdf4::ncvar_def("SWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Surface incident shortwave radiation")
  out <- checkTemp(23)
  nc_var[[s + 23]] <- ncdf4::ncvar_def("Tair", units = "K", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface air temperature")
  nc_var[[s + 24]] <- ncdf4::ncvar_def("Wind", units = "m s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface module of the wind")
  nc_var[[s + 25]] <- ncdf4::ncvar_def("LWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Net Longwave Radiation")
  nc_var[[s + 26]] <- ncdf4::ncvar_def("Qg", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Ground heat")
  nc_var[[s + 27]] <- ncdf4::ncvar_def("Qh", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Sensible heat")
  out <- conversion(28, PEcAn.data.atmosphere::get.lv())  ## kg m-2 s-1 -> W m-2
  nc_var[[s + 28]] <- ncdf4::ncvar_def("Qle", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Latent heat")
  nc_var[[s + 29]] <- ncdf4::ncvar_def("SWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Net shortwave radiation")
  nc_var[[s + 30]] <- PEcAn.utils::mstmipvar("RootMoist", lat, lon, t, zg)   # not standard
  nc_var[[s + 31]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Transpiration")
  nc_var[[s + 32]] <- PEcAn.utils::mstmipvar("WaterTableD", lat, lon, t, zg) # not standard
  
  nc_var[[s + 33]] <- ncdf4::ncvar_def("fPAR", units = "", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Absorbed fraction incoming PAR")
  nc_var[[s + 34]] <- ncdf4::ncvar_def("LAI", units = "m2 m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Leaf Area Index")
  nc_var[[s + 35]] <- PEcAn.utils::mstmipvar("SMFrozFrac", lat, lon, t, zg)  # not standard
  nc_var[[s + 36]] <- PEcAn.utils::mstmipvar("SMLiqFrac", lat, lon, t, zg)   # not standard
  nc_var[[s + 37]] <- ncdf4::ncvar_def("SoilMoist", units = "kg m-2", dim = list(lon, lat, zg, t), missval = -999, 
                                       longname = "Average Layer Soil Moisture")
  out <- checkTemp(38)
  nc_var[[s + 38]] <- ncdf4::ncvar_def("SoilTemp", units = "K", dim = list(lon, lat, zg, t), missval = -999, 
                                       longname = "Average Layer Soil Temperature")
  nc_var[[s + 39]] <- ncdf4::ncvar_def("SoilWet", units = "", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Soil Wetness")
  nc_var[[s + 40]] <- PEcAn.utils::mstmipvar("Albedo", lat, lon, t, zg)      # not standard
  out <- checkTemp(41)
  nc_var[[s + 41]] <- PEcAn.utils::mstmipvar("SnowT", lat, lon, t, zg)       # not standard
  nc_var[[s + 42]] <- ncdf4::ncvar_def("SWE", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Snow Water Equivalent")
  out <- checkTemp(43)
  nc_var[[s + 43]] <- PEcAn.utils::mstmipvar("VegT", lat, lon, t, zg)        # not standard
  nc_var[[s + 44]] <- ncdf4::ncvar_def("Evap", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Evaporation")
  nc_var[[s + 45]] <- ncdf4::ncvar_def("Qs", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Surface runoff")
  nc_var[[s + 46]] <- ncdf4::ncvar_def("Qsb", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Subsurface runoff")
  out <- conversion(47, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 47]] <- ncdf4::ncvar_def("SoilResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Soil Respiration")
  # Remove SLZ from output before finalizing list.  replace with time_bounds
  if(!is.null(out[["SLZ"]])){
    out[["SLZ"]] <- NULL
  }
  out_length <- length(out)
  out[[out_length + 1]] <- c(rbind(bounds[, 1], bounds[, 2]))
  nc_var[[s + (out_length + 1)]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                                     longname = "history time interval endpoints", 
                                                     dim=list(time_interval,time = t), 
                                                     prec = "double")
  
  return(list(nc_var = nc_var, out = out))
  
} # put_T_values


##-------------------------------------------------------------------------------------------------#

##' Function for reading -E- files
##'
##' @details
##'  e.g.     yr = 1999
##'      yfiles = 1999 1999 1999 1999 1999 1999 1999 2000 2000 2000 2000
##'      efiles = "analysis-E-1999-06-00-000000-g01.h5" "analysis-E-1999-07-00-000000-g01.h5"
##'               "analysis-E-1999-08-00-000000-g01.h5" "analysis-E-1999-09-00-000000-g01.h5"
##'               "analysis-E-1999-10-00-000000-g01.h5" "analysis-E-1999-11-00-000000-g01.h5"
##'               "analysis-E-1999-12-00-000000-g01.h5" "analysis-E-2000-01-00-000000-g01.h5"
##'               "analysis-E-2000-02-00-000000-g01.h5" "analysis-E-2000-03-00-000000-g01.h5"
##'               "analysis-E-2000-04-00-000000-g01.h5"
##'
##' pft_names  : character vector with names of PFTs
##' pft_names <- c("temperate.Early_Hardwood", "temperate.Late_Hardwood")
##'
##' @param yr the year being processed
##' @param yfiles the years on the filenames, will be used to matched efiles for that year
##'
##' @export
read_E_files <- function(yr, yfiles, efiles, outdir, start_date, end_date, pft_names, vars.E, ...){
  PFTs <- data.frame(PFT=1:17,
                     Description=c("C4 grass", "Early tropical", "Mid Tropical", "Late tropical", "Temperate C3 Grass", 
                                   "North Pine", "South Pine", "Late conifer", 
                                   "Early hardwood", "Mid hardwood", "Late hardwood", 
                                   "C3 crop", "C3 pasture", "C4 crop", "C4 pasture", "C3 grass", "Araucaria"))
  
  print(paste0("*** Reading -E- file ***"))
  
  add <- function(dat, var.name) {
    
    dims <- dim(dat)
    
    if (is.null(dims)) {
      if (length(dat) == 1) {
        if (!var.name %in% names(out)) {
          out[[var.name]] <- array(dat, dim = 1)
        } else {
          out[[var.name]] <- abind::abind(out[[var.name]], array(dat, dim = 1), along = 1)
        }
      } else {
        warning("expected a single value")
      }
    } else if(length(dims)==1){
      if(!var.name %in% names(out)){
        out[[var.name]] <- array(dat)
      } else {
        # Check to see if we need to add rows so we can store cohort-level data
        if(dim(out[[var.name]])[1] == length(dat)){
          out[[var.name]] <- cbind(out[[var.name]], array(dat))
        } else {
          row.fill <- abs(length(dat) - nrow(out[[var.name]]))
          if(nrow(out[[var.name]])>length(dat)){
            dat.fill <- array(0, dim=row.fill)
            out[[var.name]] <- cbind(out[[var.name]], c(array(dat), dat.fill))
          } else {
            if(length(dim(out[[var.name]]))==1){
              dat.fill <- array(0, dim=row.fill)
              out[[var.name]] <- cbind(c(out[[var.name]], dat.fill), array(dat))
            } else {  
              col.fill <- ncol(out[[var.name]])
              dat.fill <- array(0, dim=c(row.fill, col.fill))
              out[[var.name]] <- cbind(rbind(out[[var.name]], dat.fill), array(dat))
            }
          }
          
          
        }
        
      }
    } else if (length(dims)==2) {
      dat <- t(dat)
      dims <- dim(dat)
      # dat <- dat[1:(nrow(dat)), ]
      if (! var.name %in% names(out)) {
        out[[var.name]] <- dat
      } else {
        out[[var.name]] <- abind::abind(out[[var.name]], dat, along = 1)
      }
    } else {
      stop("can't handle arrays with >2 dimensions yet")
    }
    return(out)
    
    ## finally make sure we use -999 for invalid values
    out[[var.name]][is.null(out[[var.name]])] <- -999
    out[[var.name]][is.na(out[[var.name]])] <- -999
    
    return(out)
  } # end add-function
  
  
  getHdf5Data <- function(nc, var) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    } else {
      warning("Could not find", var, "in ed hdf5 output.")
      return(-999)
    }
  }
  
  CheckED2Version <- function(nc) {
    if ("MMEAN_BDEAD_PY" %in% names(nc$var)) {
      return("Git")
    }
  }
  
  
  # there are multiple -E- files per year
  ysel <- which(yr == yfiles)
  
  # grab year-month info from file names, e.g. "199906"
  times <- gsub(
    "(.*)\\-(.*)\\-(.*)\\-(.*)\\-(.*)", "\\1\\2",
    sapply(
      strsplit(efiles, "-E-"), 
      function(x) x[2] # Select only the part of each name after res.flag
    )
  )
  
  # lets make it work for a subset of vars fist
  # TODO :  read all (or more) variables, functionality exists, see below
  varnames.cohort <- c("DBH", "DDBH_DT", "NPLANT", "BA_CO", "AGB_CO", "BALIVE", "BDEAD", "LAI_CO")
  varnames.std <- c("DBH", "DDBH_DT", "Density", "BasalArea", "AbvGrndBiom", "TotLivBiom", "TotDeadBiom", "LAI")

  #, "BasalArea", "AbvGrndBiom", "TotLivBiom", "TotDeadBiom", "LAI"
  # out <- add(getHdf5Data(ncT, "BASAL_AREA_PY"), "BasalArea")  ## Aboveground Biomass
  # out <- add(getHdf5Data(ncT, "AGB_PY"), "AbvGrndBiom")  ## Aboveground Biomass
  # out <- add(getHdf5Data(ncT, "BALIVE_PY"), "TotLivBiom")  ## Aboveground Biomass
  # out <- add(getHdf5Data(ncT, "BDEAD_PY"), "TotDeadBiom")  ## Aboveground Biomass
  # out <- add(getHdf5Data(ncT, "MMEAN_LAI_PY"), "LAI")  ## Aboveground Biomass
  
  # List of vars to extract includes the requested one, plus others needed below 
  vars <- c(varnames.cohort, "PFT", "AREA", "PACO_N")
  
  # list to collect outputs
  ed.dat <- list()
  out <- list()
  
  # loop over the files for that year
  for(i in ysel) {
    
    ncT <- ncdf4::nc_open(file.path(outdir, efiles[i]))
    allvars <- names(ncT$var)
    
    if(i==ysel[1]) out$SLZ <- ncdf4::ncvar_get(ncT, "SLZ")
    # -------
    # Drivers
    # -------
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_CO2_PY"), "CO2air")  ## CO2air
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_RLONG_PY"), "LWdown")  ## Lwdown
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_PRSS_PY"), "Psurf")  ## Psurf
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_SHV_PY"), "Qair")  ## Qair
    out <- add(getHdf5Data(ncT, "MMEAN_PCPG_PY"), "Rainf")  ## Rainf
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_PAR_PY"), "SWdown")  ## Swdown
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_TEMP_PY"), "Tair")  ## Tair
    out <- add(getHdf5Data(ncT, "MMEAN_ATM_VELS_PY"), "Wind")  ## Wind
    out <- add(getHdf5Data(ncT, 'MMEAN_ATM_RLONG_PY')-getHdf5Data(ncT, 'MMEAN_RLONGUP_PY'), "LWnet") ## Lwnet
    
    out <- add(getHdf5Data(ncT, "MMEAN_SFCW_DEPTH_PY"), "SnowDepth")  ## SnowDepth (ED2 currently groups snow in to surface water)
    out <- add(1 - getHdf5Data(ncT, "MMEAN_SFCW_FLIQ_PY"), "SnowFrac")  ## SnowFrac (ED2 currently groups snow in to surface water)
    
    # -------
    
    # -------
    # Site-Scale Outputs
    # -------
    # Carbon Fluxes
    out <-  add(getHdf5Data(ncT, "IGNITION_RATE")*12, "Fire_flux")  ## Fire_flux -- *12 = per month to per year
    out <- add(getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "AutoResp")  ## AutoResp
    out <- add(getHdf5Data(ncT, "MMEAN_CAN_CO2_PY"), "CO2CAS")  ## CO2CAS
    out <- add(getHdf5Data(ncT, "MMEAN_GPP_PY"), "GPP")  ## GPP
    out <- add(getHdf5Data(ncT, "MMEAN_RH_PY"), "HeteroResp")  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "MMEAN_GPP_PY") + getHdf5Data(ncT, "MMEAN_PLRESP_PY") + 
                 getHdf5Data(ncT, "MMEAN_RH_PY"), "NEE")  ## NEE
    out <- add(getHdf5Data(ncT, "MMEAN_GPP_PY") - getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "NPP")  ## NPP
    out <- add(getHdf5Data(ncT, "MMEAN_RH_PY") + getHdf5Data(ncT, "MMEAN_PLRESP_PY"), "TotalResp")  ## TotalResp
    out <- add(getHdf5Data(ncT, "MMEAN_TRANSP_PY"), "Tveg")  ## Tveg
    out <- add(getHdf5Data(ncT, "MMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "MMEAN_VAPOR_WC_PY") +
                 getHdf5Data(ncT, "MMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "MMEAN_TRANSP_PY"), "Evap")  ## Evap

    # Site-level variables related to water & carbon stress
    out <- add(getHdf5Data(ncT, "MMEAN_AVAILABLE_WATER_PY"), "WaterAvail")  ## WaterAvail=Available Water kg/m2
    out <- add(getHdf5Data(ncT, "MMEAN_A_CLOSED_PY"), "A_Closed")  ## Minimum assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_CO2_PY"), "A_CO2")  ## CO2-limited assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_LIGHT_PY"), "A_Light")  ## Light-limited assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_NET_PY"), "A_Net")  ## Actual assimilation rate; umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_A_OPEN_PY"), "A_Open")  ##  Assimilation rate (no soil moist. stress); umol/m2l/s
    out <- add(getHdf5Data(ncT, "MMEAN_FSW_PY"), "MoistStress")  ## Moisture stress
    out <- add(getHdf5Data(ncT, "MMEAN_FS_OPEN_PY"), "NetStress")  ## Net stress factor

    # Site-Level Soil Characterstics 
    out <- add(getHdf5Data(ncT, "MMEAN_FAST_SOIL_C_PY") + getHdf5Data(ncT, "MMEAN_STRUCT_SOIL_C_PY") + 
                 getHdf5Data(ncT, "MMEAN_SLOW_SOIL_C_PY"), "TotSoilCarb")  ## TotSoilCarb
    
    # Site-Level Soil Characteristics by depth
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_WATER_PY"), "SoilMoist")  ## SoilWater  **********
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_TEMP_PY"), "SoilTemp")  ## SoilTemp
    out <- add(getHdf5Data(ncT, "MMEAN_SOIL_MSTPOT_PY"), "SoilMstPot")  ## SoilMstPot = Soil Matric Potential (m); not MsTMIP
    
    # Site-Level Community Characteristics by PFT
    #, "BasalArea", "AbvGrndBiom", "TotLivBiom", "TotDeadBiom", "LAI"
    # out <- add(getHdf5Data(ncT, "BASAL_AREA_PY"), "BasalArea")  ## Aboveground Biomass
    # out <- add(getHdf5Data(ncT, "AGB_PY"), "AbvGrndBiom")  ## Aboveground Biomass
    # out <- add(getHdf5Data(ncT, "BALIVE_PY"), "TotLivBiom")  ## Aboveground Biomass
    # out <- add(getHdf5Data(ncT, "BDEAD_PY"), "TotDeadBiom")  ## Aboveground Biomass
    # out <- add(getHdf5Data(ncT, "MMEAN_LAI_PY"), "LAI")  ## Aboveground Biomass
    # -------
    
    # -------
    # Cohort/PFT-Scale Drivers
    # -------
    if (!is.null(vars)) allvars <- allvars[ allvars %in% vars ]
    
    if (length(ed.dat) == 0){
      
      for (j in 1:length(allvars)) {
        ed.dat[[j]] <- list()
        ed.dat[[j]][[1]] <- ncdf4::ncvar_get(ncT, allvars[j])
      }
      names(ed.dat) <- allvars
      
    } else {
      
      # 2nd and more months
      t <- length(ed.dat[[1]]) + 1
      
      for (j in 1:length(allvars)) {
        
        k <- which(names(ed.dat) == allvars[j])
        
        if (length(k)>0) {
          
          ed.dat[[k]][[t]] <- ncdf4::ncvar_get(ncT, allvars[j])
          
        } else { ## add a new ed.datiable. ***Not checked (shouldn't come up?)
          
          ed.dat[[length(ed.dat)+1]] <- list()    # Add space for new ed.datiable
          ed.dat[[length(ed.dat)]][1:(t-1)] <- NA # Give NA for all previous time points
          ed.dat[[length(ed.dat)]][t] <- ncdf4::ncvar_get(ncT, allvars[j]) # Assign the value of the new ed.datiable at this time point
          names(ed.dat)[length(ed.dat)] <- allvars[j]
          
        }
      }      
    }
    # -------
    ncdf4::nc_close(ncT)
  } # end ysel-loop
  
  # for now this function does not read any ED variable that has soil as a dimension
  soil.check <- grepl("soil", pft_names)
  if (any(soil.check)) {
    # for now keep soil out
    pft_names <- pft_names[!(soil.check)]
  }
  
  pft_names=PFTs$Description
  pfts <- PFTs$PFT
  npft <- length(pft_names)
  # data(pftmapping, package = "PEcAn.ED2")
  # pfts <- sapply(pft_names, function(x) pftmapping$ED[pftmapping$PEcAn == x]) 
  
  # out <- list()
  for (VAR in c(varnames.std, "DBH_Tree", "DDBH_DT_Tree", "Density_Tree", "BasalArea_Tree")) {
    out[[VAR]] <- array(NA, c(length(ysel), npft))
  }

  
  # Aggregate over PFT and DBH bins  
  for (i in seq_along(ysel)) {
    
    # Get additional cohort-level variables required
    pft        <- ed.dat$PFT[[i]]
    dbh        <- ed.dat$DBH[[i]]      # cm / plant --unused?
    plant.dens <- ed.dat$NPLANT[[i]]   # plant / m2
    
    # Get patch areas. In general patches aren't the same area, so this is needed to area-weight when averaging up to site level. Requires minor finnagling to convert patch-level AREA to a cohort-length variable. 
    patch.area <- ed.dat$AREA[[i]]    # m2  -- one entry per patch
    pacoN      <- ed.dat$PACO_N[[i]]  # number of cohorts per patch
    patch.area <- rep(patch.area, pacoN)  # patch areas, repped out to one entry per cohort
    
    # Now can get number of plants per cohort, which will be used for weighting. Note that area may have been (often/always is?) a proportion of total site area, rather than an absolute measure. In which case this nplant is a tiny and meaningless number in terms of actual number of plants. But that doesn't matter for weighting purposes. 
    nplant <- plant.dens * patch.area
    
    
    # Not all ED cohort variables are in per-plant units. This code would not be applicable to them without modification.
    # However, it does handle two special cases. For NPLANT, it performs no weighting, but simply sums over cohorts in the PFT. 
    # For MMEAN_MORT_RATE_CO, it first sums over columns representing different mortality types first, then proceeds with weighting. 
    
    for (k in 1:npft) {
      ind <- (pft == pfts[k])
      ind.10cm <- (pft==pfts[k] & dbh>=10)
      
      if (any(ind)) {
        for (j in seq_along(varnames.cohort)) {
          if(varnames.cohort[j] == "NPLANT") {
            # Return the total number of plants in the bin
            out$Density[i,k] <- sum(nplant[ind])
          } else if (varnames.cohort[j] == "MMEAN_MORT_RATE_CO") {
            # Sum over all columns 
            mort <- apply(ed.dat$MMEAN_MORT_RATE_CO[[i]][ind,, drop=F], 1, sum, na.rm = T)
            out$MMEAN_MORT_RATE_CO[i,k] <- sum(mort * nplant[ind]) / sum(nplant[ind])
          } else if(varnames.cohort[j] %in% c("DBH", "DDBH_DT")) {
            # For all others, just get mean weighted by nplant; 
            # note: because these are individual plant measures and not /m2 we do want to relativize by total
            out[[varnames.std[j]]][i,k] <- sum(ed.dat[[varnames.cohort[j]]][[i]][ind] * nplant[ind])/sum(nplant[ind]) 
          } else if(varnames.cohort[j]=="LAI") {
            out[[varnames.std[j]]][i,k] <- sum(ed.dat[[varnames.cohort[j]]][[i]][ind] * patch.area[ind]) 
          } else {
            # For non-individual number (thinks per m2, we can just sum the relativized numbers)
            out[[varnames.std[j]]][i,k] <- sum(ed.dat[[varnames.cohort[j]]][[i]][ind] * nplant[ind]) 
          }
          dimnames(out[[varnames.std[j]]]) <- list(months = times[ysel], pft = pft_names)
        }
      }  #end any(ind)
      if(any(ind.10cm)){
        out$Density_Tree[i,k] <- sum(nplant[ind.10cm])
        out$DBH_Tree[i,k] <- sum(ed.dat[["DBH"]][[i]][ind.10cm] * nplant[ind.10cm])/sum(nplant[ind.10cm]) 
        out$DDBH_DT_Tree[i,k] <- sum(ed.dat[["DBH_DT"]][[i]][ind.10cm] * nplant[ind.10cm])/sum(nplant[ind.10cm]) 
        out[["BasalArea_Tree"]][i,k] <- sum(ed.dat[["BA_CO"]][[i]][ind.10cm] * nplant[ind.10cm]) 
        
      }
    }
    
  }
  out$PFT <- pfts # will write this to the .nc file
  
  return(out)
  
} # read_E_files

##-------------------------------------------------------------------------------------------------#

##' Function for put -E- values to nc_var list
##' @export
put_E_values <- function(yr, nc_var, out, lat, lon, begins, ends, pft_names, ...){
  
  PFTs <- data.frame(PFT=1:17,
                     Description=c("C4 grass", "Early tropical", "Mid Tropical", "Late tropical", "Temperate C3 Grass", 
                                   "North Pine", "South Pine", "Late conifer", 
                                   "Early hardwood", "Mid hardwood", "Late hardwood", 
                                   "C3 crop", "C3 pasture", "C4 crop", "C4 pasture", "C3 grass", "Araucaria"))
  
  s <- length(nc_var)
  
  # even if this is a SA run for soil, currently we are not reading any variable that has a soil dimension
  # "soil" will be passed to read.output as pft.name from upstream, when it's not part of the attribute it will read the sum
  soil.check <- grepl("soil", pft_names)
  if(any(soil.check)){
    # for now keep soil out
    pft_names <- pft_names[!(soil.check)]
  }
  pfts <- PFTs$PFT
  pft_names <- PFTs$Description
  # data(pftmapping, package = "PEcAn.ED2")
  # pfts <- sapply(pft_names, function(x) pftmapping$ED[pftmapping$PEcAn == x]) 
  
  # ----- fill list
  
  ##### setup output time and time bounds
  ## Create a date vector that contains each month of the model run (e.g. "2001-07-01" "2001-08-01" "2001-09-01"....)
  ## and which is the correct length for each full or partial year
  output_date_vector <- seq(lubridate::floor_date(begins,"month"), by = "month", length.out = dim(out[[2]])[2] )
  ## Create a vector of the number of days in each month by year
  ## (e.g. 31 31 30 31 30 31)
  num_days_per_month <- lubridate::days_in_month(output_date_vector)
  ## Update num_days_per_month and output_date_vector if model run did not start on the first day of a month
  ## e.g. "2001-07-15" "2001-08-01", 17 31
  if (lubridate::yday(begins) != lubridate::yday(output_date_vector[1])) {
    temp <- num_days_per_month[1] - ((lubridate::yday(begins) - lubridate::yday(output_date_vector[1])))
    num_days_per_month[1] <- temp
    output_date_vector[1] <- begins
  }
  ## Create a vector of output month julian dates (e.g. 196 213 244 274 305 335)
  jdates <- lubridate::yday(output_date_vector)
  ## Create a 0 index dtime variable 
  dtvals <- jdates - 1 # convert to 0 index
  ## Create monthly time bounds to populate dtime_bounds variable
  bounds <- array(data = NA, dim = c(length(dtvals), 2))
  bounds[, 1] <- dtvals 
  bounds[, 2] <- bounds[, 1] + num_days_per_month 
  bounds <- round(bounds, 4) # create time bounds for each timestep in t, t+1; t+1, t+2... format
  #####
  
  t <- ncdf4::ncdim_def(name = "dtime", units = paste0("days since ", yr, "-01-01 00:00:00"), 
                        vals = dtvals, calendar = "standard", unlim = TRUE)
  time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                    longname = "history time interval endpoint dimensions", 
                                    vals = 1:2, units = "")
  p <- ncdf4::ncdim_def(name = "pft", units = "unitless", vals = pfts, 
                        longname = "Plant Functional Type", unlim = TRUE)
  slzdata <- out$SLZ
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  zg <- ncdf4::ncdim_def("SoilLayerMidpoint", "meters", c(slzdata[1:length(dz)] + dz/2, 0))
  
  # NOTE : the order of dimensions is going to be important for read.output
  # this was the fist case of reading pft-specific outputs at the time
  # but checking base/utils/data/standard_vars.csv "pft" should come after "time" as a dimension
  # e.g. when NEE is pft-specific for some model output it will be the 4th dimension
  # lon / lat / time / pft 
  # from read.output's perspective, dimension of pft will be the same for NEE there and DBH here
  

  ## Conversion factor for umol C -> kg C
  Mc <- 12.017  #molar mass of C, g/mol
  umol2kg_C <- Mc * udunits2::ud.convert(1, "umol", "mol") * udunits2::ud.convert(1, "g", "kg")
  yr2s      <- udunits2::ud.convert(1, "s", "yr")
  # pl2m2 <- out$Density
  
  # TODO - remove this function and replace with ifelse statements inline below (SPS)
  conversion <- function(var.name, mult) {
    out[[var.name]][is.na(out[[var.name]])] <- -999
    ## make sure only to convert those values that are not -999
    out[[var.name]][out[[var.name]] != -999] <- out[[var.name]][out[[var.name]] != -999] * mult
    return(out)
  }
  
  checkTemp <- function(var.name) {
    out[[var.name]][out[[var.name]] == 0] <- -999
    return(out)
  }
  
  # -------
  # Drivers
  # -------
  nc_var[["CO2air"]] <- ncdf4::ncvar_def("CO2air", units = "umol mol-1", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Near surface CO2 concentration")
  nc_var[["LWdown"]] <- ncdf4::ncvar_def("LWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Surface incident longwave radiation")
  nc_var[["Psurf"]] <- ncdf4::ncvar_def("Psurf", units = "Pa", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Surface pressure")
  nc_var[["Qair"]] <- ncdf4::ncvar_def("Qair", units = "kg kg-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface specific humidity")
  nc_var[["Rainf"]] <- ncdf4::ncvar_def("Rainf", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Rainfall rate")
  nc_var[["SWdown"]] <- ncdf4::ncvar_def("SWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Surface incident shortwave radiation")
  out <- checkTemp("Tair")
  nc_var[["Tair"]] <- ncdf4::ncvar_def("Tair", units = "K", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface air temperature")
  nc_var[["Wind"]] <- ncdf4::ncvar_def("Wind", units = "m s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Near surface module of the wind")
  nc_var[["LWnet"]] <- ncdf4::ncvar_def("LWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Net Longwave Radiation")
  
  # Snow Vars
  nc_var[["SnowDepth"]] <- ncdf4::ncvar_def("SnowDepth", units = "m", dim = list(lon, lat, t), missval = -999,
                                            longname = "Total snow depth")
  nc_var[["SnowFrac"]] <- ncdf4::ncvar_def("SnowFrac", units = "m", dim = list(lon, lat, t), missval = -999,
                                            longname = "Snow Fraction Liquid")
  # -------
  
  # -------
  # Site Vars
  # -------
  # Fire/Disturbance
  out <- conversion("Fire_flux", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["Fire_flux"]] <- ncdf4::ncvar_def("Fire_flux", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                            longname = "Fire emissions")
  
  # Carbon Fluxes
  out <- conversion("AutoResp", umol2kg_C)  ## umol/m2 s-1 -> kg/m2 s-1
  nc_var[["AutoResp"]] <- ncdf4::ncvar_def("AutoResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                           longname = "Autotrophic Respiration")
  nc_var[["CO2CAS"]] <- ncdf4::ncvar_def("CO2CAS", units = "ppmv", dim = list(lon, lat, t), missval = -999, 
                                         longname = "CO2CAS")
  nc_var[["GPP"]] <- ncdf4::ncvar_def("GPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Gross Primary Productivity")
  out <- conversion("HeteroResp", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["HeteroResp"]] <- ncdf4::ncvar_def("HeteroResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                             longname = "Heterotrophic Respiration")
  out <- conversion("NEE", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["NEE"]] <-  ncdf4::ncvar_def("NEE", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Net Ecosystem Exchange")
  out <- conversion("NPP", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["NPP"]] <- ncdf4::ncvar_def("NPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                      longname = "Net Primary Productivity")
  out <- conversion("TotalResp", yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[["TotalResp"]] <- ncdf4::ncvar_def("TotalResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                            longname = "Total Respiration")
  
  # Water Fluxes
  nc_var[["Tveg"]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Transpiration")
  nc_var[["Evap"]] <- ncdf4::ncvar_def("Evap", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Total Evaporation")
  
  # Non-Standard Carbon Fluxes & Stress Measures
  nc_var[["WaterAvail"]] <- ncdf4::ncvar_def("WaterAvail", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                             longname = "Available Water")
  
  out <- conversion("A_Closed", umol2kg_C)
  nc_var[["A_Closed"]] <- ncdf4::ncvar_def("A_Closed", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                           longname = "Minimum assimilation rate")
  out <- conversion("A_CO2", umol2kg_C)
  nc_var[["A_CO2"]] <- ncdf4::ncvar_def("A_CO2", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "CO2-limited assimilation rate")
  out <- conversion("A_Light", umol2kg_C)
  nc_var[["A_Light"]] <- ncdf4::ncvar_def("A_Light", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                          longname = "Light-limited assimilation rate")
  out <- conversion("A_Net", umol2kg_C)
  nc_var[["A_Net"]] <- ncdf4::ncvar_def("A_Net", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                        longname = "Acutal net assimilation rate")
  out <- conversion("A_Open", umol2kg_C)
  nc_var[["A_Open"]] <- ncdf4::ncvar_def("A_Open", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                         longname = "Non-moisture limited assimilation rate")
  nc_var[["Tveg"]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                       longname = "Transpiration")
  nc_var[["MoistStress"]] <- ncdf4::ncvar_def("MoistStress", units = "fraction; 1 = no stress", dim = list(lon, lat, t), 
                                              missval = -999, 
                                              longname = "Moisture Stress Factor; 1 = no stress")
  nc_var[["NetStress"]] <- ncdf4::ncvar_def("NetStress", units = "fraction; 1 = no stress", dim = list(lon, lat, t), 
                                            missval = -999, 
                                            longname = "Net Stress factor; 1 = no stress")
  # Soil Vars
  # TotSoilCarb
  nc_var[["SoilMoist"]] <- ncdf4::ncvar_def("SoilMoist", units = "kg m-2", dim = list(lon, lat, zg, t), missval = -999, 
                                            longname = "Average Layer Soil Moisture")
  out <- checkTemp("SoilTemp")
  nc_var[["SoilTemp"]] <- ncdf4::ncvar_def("SoilTemp", units = "K", dim = list(lon, lat, zg, t), missval = -999, 
                                           longname = "Average Layer Soil Temperature")
  nc_var[["SoilMstPot"]] <- ncdf4::ncvar_def("SoilMstPot", units = "m", dim = list(lon, lat, zg, t), missval = -999, 
                                             longname = "Average Layer Soil Matric Potential")
  # -------
  
  # -------
  # PFT-Level
  # -------
  nc_var[["DBH"]] <- ncdf4::ncvar_def("DBH", units = "cm", dim = list(lon, lat, t, p), missval = -999, 
                                      longname = "Diameter at breast height")
  nc_var[["DBH_Tree"]] <- ncdf4::ncvar_def("DBH_Tree", units = "cm", dim = list(lon, lat, t, p), missval = -999, 
                                      longname = "Diameter at breast height, >10cm DBH")
  nc_var[["DDBH_DT"]] <- ncdf4::ncvar_def("DDBH_DT", units = "cm yr-1", dim = list(lon, lat, t, p), missval = -999, 
                                          longname = "Rate of change in dbh")
  nc_var[["DDBH_DT_Tree"]] <- ncdf4::ncvar_def("DDBH_DT_Tree", units = "cm yr-1", dim = list(lon, lat, t, p), missval = -999, 
                                          longname = "Rate of change in dbh, >10cm DBH")
  nc_var[["Density"]] <- ncdf4::ncvar_def("Density", units = "plant m-2", dim = list(lon, lat, t, p), missval = -999, 
                                          longname = "Plant density")
  nc_var[["Density_Tree"]] <- ncdf4::ncvar_def("Density_Tree", units = "plant m-2", dim = list(lon, lat, t, p), missval = -999, 
                                          longname = "Plant density, >10cm DBH")
  
  nc_var[["BasalArea"]] <- ncdf4::ncvar_def("BasalArea", units = "cm m-2", dim = list(lon, lat, t, p),
                                            missval = -999, longname = "Cohort basal area")
  nc_var[["BasalArea_Tree"]] <- ncdf4::ncvar_def("BasalArea_Tree", units = "cm m-2", dim = list(lon, lat, t, p),
                                            missval = -999, longname = "Cohort basal area, >10cm DBH")
  nc_var[["AbvGrndBiom"]] <- ncdf4::ncvar_def("AbvGrndBiom", units = "kg C m-2", dim = list(lon, lat, t, p), 
                                              missval = -999, longname = "Cohort above ground biomass")
  nc_var[["TotLivBiom"]] <- ncdf4::ncvar_def("TotLivBiom", units = "kg C m-2", dim = list(lon, lat, t, p), 
                                             missval = -999, longname = "Cohort living biomass")
  nc_var[["TotDeadBiom"]] <- ncdf4::ncvar_def("TotDeadBiom", units = "kg C m-2", dim = list(lon, lat, t, p), 
                                              missval = -999, longname = "Total non-living biomass")
  
  nc_var[["LAI"]] <- ncdf4::ncvar_def("LAI", units = "m2 m-2", dim = list(lon, lat, t, p), 
                                      missval = -999, longname = "Cohort Leaf Area Index")
  # nc_var[["MortRate"]] <- ncdf4::ncvar_def("MortRate", units = "unknown", dim = list(lon, lat, t, p), 
  #                                          missval = -999, longname = "Mortality Rate")
  # -------
  
  # longname of this variable will be parsed by read.output
  # so that read.output has a way of accessing PFT names
  nc_var[["PFT"]] <- ncdf4::ncvar_def("PFT", units = "", dim = list(p),  
                                      longname = paste(pft_names, collapse=",")) 
  
  out_length <- length(out)
  out[[out_length + 1]] <- c(rbind(bounds[, 1], bounds[, 2]))
  nc_var[["DTime"]] <- ncdf4::ncvar_def(name = "dtime_bounds", units = "", 
                                      longname = "monthly history time interval endpoints", 
                                      dim = list(time_interval,dtime = t), 
                                      prec = "double")
  return(list(nc_var = nc_var, out = out))
  
} # put_E_values




#' S-file contents are not written to standard netcdfs but are used by read_restart
#' from SDA's perspective it doesn't make sense to write and read to ncdfs because ED restarts from history files
#' 
#' @param sfile history file name e.g. "history-S-1961-01-01-000000-g01.h5"
#' @param outdir path to run outdir, where the -S- file is
#' @param pft_names string vector, names of ED2 pfts in the run, e.g. c("temperate.Early_Hardwood", "temperate.Late_Conifer")
#' @param pecan_names string vector, pecan names of requested variables, e.g. c("AGB", "AbvGrndWood")
#' 
#' @export
read_S_files <- function(sfile, outdir, pft_names, pecan_names = NULL){
  
  print(paste0("*** Reading -S- file ***"))
  
  # commonly used vars
  if(is.null(pecan_names)) pecan_names <- c("AGB", "AbvGrndWood", "GWBI", "DBH")
  
  ed_varnames <- pecan_names
  
  # TODO: ed.var lookup function can also return deterministically related variables
  
  # translate pecan vars to ED vars
  trans_out    <- translate_vars_ed(ed_varnames)
  ed_varnames  <- trans_out$vars  # variables to read from history files
  ed_derivs    <- trans_out$expr  # derivations to obtain pecan standard variables
  add_vars     <- trans_out$addvars # these are the vars -if there are any- that won't be updated by analysis, but will be used in write_restart
  ed_units     <- trans_out$units # might use
  
  # List of vars to extract includes the requested one, plus others needed below 
  add_vars <- c(add_vars, "PFT", "AREA", "PACO_N", "NPLANT","DAGB_DT", "BDEAD", "DBH", 
                "BSTORAGE", "BALIVE", "BLEAF", "BROOT", "BSEEDS_CO", "BSAPWOODA", "BSAPWOODB")
  vars <- c(ed_varnames, add_vars) 
  
  # list to collect outputs
  ed.dat <- list()
  
  nc <- ncdf4::nc_open(file.path(outdir, sfile))
  allvars <- names(nc$var)
  if (!is.null(vars)) allvars <- allvars[ allvars %in% vars ]
  
  for (j in seq_along(allvars)) {
    ed.dat[[j]] <- list()
    ed.dat[[j]] <- ncdf4::ncvar_get(nc, allvars[j])
  }
  names(ed.dat) <- allvars
  
  ncdf4::nc_close(nc)
  
  
  # for now this function does not read any ED variable that has soil as a dimension
  soil.check <- grepl("soil", pft_names)
  if (any(soil.check)) {
    # for now keep soil out
    pft_names <- pft_names[!(soil.check)]
  }
  
  npft <- length(pft_names)
  data(pftmapping, package = "PEcAn.ED2")
  pft_nums <- sapply(pft_names, function(x) pftmapping$ED[pftmapping$PEcAn == x]) 
  
  out <- list()
  for (varname in pecan_names) {
    out[[varname]] <- array(NA, npft)
  }
  
  
  # Get cohort-level variables 
  pft        <- ed.dat$PFT
  plant_dens <- ed.dat$NPLANT  # Cohort stem density -- plant/m2
  dbh        <- ed.dat$DBH # used in allometric eqns -- dbh
  
  # Get patch areas. In general patches aren't the same area, so this is needed to area-weight when averaging up to site level. Requires minor finnagling to convert patch-level AREA to a cohort-length variable. 
  patch_area  <- ed.dat$AREA    # unitless, a proportion of total site area  -- one entry per patch (always add up to 1)
  paco_n      <- ed.dat$PACO_N  # number of cohorts per patch
  
  patch_index <- rep(1:length(paco_n), times = paco_n)
  
  # read xml to extract allometric coeffs later
  configfile <- paste0(gsub("/out/", "/run/", outdir), "/config.xml")
  pars <- XML::xmlToList(XML::xmlParse(configfile))
  # remove non-pft sublists
  pars[names(pars) != "pft"] <- NULL
  # pass pft numbers as sublist names
  names(pars) <- pft_nums
  
  # Aggregate
  for (l in seq_along(pecan_names)) {
    
    variable <- convert.expr(ed_derivs[l])  # convert
    expr <- variable$variable.eqn$expression
    
    sapply(variable$variable.eqn$variables, function(x) assign(x, ed.dat[[x]], envir = .GlobalEnv))
    tmp.var <- eval(parse(text = expr)) # parse
    
    if (ed_units[l] %in% c("kg/m2")) { # does this always mean this is a patch-level variable w/o per-pft values?
      out[[pecan_names[l]]] <- NA
      out[[pecan_names[l]]] <- sum(tmp.var * patch_area, na.rm = TRUE)
      
    } else {# per-pft vars
      for(k in seq_len(npft)) {
        ind <- (pft == pft_nums[k])
        
        if (any(ind)) {
          # check for different variables/units?
          if (pecan_names[l] == "GWBI") {
            # use allometric equations to calculate GWBI from DDBH_DT
            ddbh_dt <- tmp.var
            ddbh_dt[!ind] <- 0
            dagb_dt <- ed.dat$DAGB_DT
            dagb_dt[!ind] <- 0
            
            # get b1Bl/b2Bl/dbh_adult from xml
            # these are in order so you can use k, but you can also extract by pft
            small   <- dbh <= as.numeric(pars[[k]]$dbh_adult)
            ddbh_dt[small]  <- as.numeric(pars[[k]]$b1Bl_small) / 2 * ddbh_dt[small]  ^ as.numeric(pars[[k]]$b2Bl_small)
            ddbh_dt[!small] <- as.numeric(pars[[k]]$b1Bl_large) / 2 * ddbh_dt[!small] ^ as.numeric(pars[[k]]$b2Bl_large)
            gwbi_ch <- dagb_dt - ddbh_dt
            #     kgC/m2/yr = kgC/plant/yr  *   plant/m2  
            plant2cohort <- gwbi_ch * plant_dens
            cohort2patch <- tapply(plant2cohort, list("patch" = patch_index), sum, na.rm = TRUE)
            out[[pecan_names[l]]][k] <- sum(cohort2patch * patch_area, na.rm = TRUE)
            
          } else if (ed_units[l] %in% c("kgC/plant")) {
            pft.var <- tmp.var
            pft.var[!ind] <- 0
            #     kgC/m2 = kgC/plant  *   plant/m2  
            plant2cohort <- pft.var * plant_dens
            # sum cohorts to aggrete to patches
            cohort2patch <- tapply(plant2cohort, list("patch" = patch_index), sum, na.rm = TRUE)
            # scale up to site-level 
            out[[pecan_names[l]]][k] <- sum(cohort2patch*patch_area, na.rm = TRUE)
            
          }
        }  #any(ind)-if SHOULD THERE BE AN ELSE? DOES ED2 EVER DRIVES SOME PFTs TO EXTINCTION? 
      } #k-loop
      
    }# per-pft or not
  } #l-loop
  
  
  # pass everything, unaggregated
  out$restart <- ed.dat
  
  
  return(out)
  
} # read_S_files
##-------------------------------------------------------------------------------------------------#
### EOF