# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"
path.out <- "extract.v3"
pfts.grass <- 5
pfts.trees <- 6:11

if(!dir.exists(path.out)) dir.create(path.out)

dat.base <- "../1_runs/MortonArb_ed_runs.v3/"
runs.raw <- dir(dat.base, "statCO2")  # This gets a list of model ensemble members for static CO2 only; adding increases in CO2 that will make trees more efficient has been a low priority because ED is unrealistically sensitive
runs.process <- dir(path.out)

runs.raw <- runs.raw[!paste(runs.raw, "Site.csv", sep="_") %in% (runs.process)]

yr2sec <- 1/(365*24*60*60)
dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24
# runs.all <- data.frame()
for(RUNID in runs.raw){
  co.list   <- list()
  pch.list  <- list()
  site.list <- list()
  
#  # This isn't needed anymore since we've cut down what gets even fed into this loop
#  # Check to see if we've already extracted this output 
#  if(any(grepl(RUNID, dir(path.out)))) next
  
  run.splt <- stringr::str_split(RUNID, "_")[[1]] # Splits apart the run name to extract different bits of info
  
  # Some models have different num. hypens, so use model name as index
  mod.splt <- length(stringr::str_split(run.splt[3], "-")[[1]])
  
  # Extracting Monthly Output
  #  -- Right now I'm also saving monthly output and we could do daily or subdaily if we wanted, 
  #     but that slows it down and requires MASSIVE storage space for runs like we're doing
  f.mo <- dir(file.path(dat.base, RUNID, "analy"), "-E-")
  
  if(length(f.mo)==0) next
  
  # Checking to make sure that it ran to 2099 if we're going to work on it
  if(strsplit(strsplit(f.mo[length(f.mo)], "_")[[1]][6], "-")[[1]][3]!="2099") next
  
  print("")
  print(RUNID)
  
  # Setting up a loop to go through each year of model output
  pb <- txtProgressBar(min=0, max=length(f.mo), style=3); #pb.ind=0
  for(i in 1:length(f.mo)){
    setTxtProgressBar(pb, i)
    
    # Splitting apart the file name so we can pull out important info such as the year
    f.split <- strsplit(f.mo[i], "-")[[1]]
    yr.now <- as.numeric(f.split[mod.splt+2])
    mo.now <- as.numeric(f.split[mod.splt+3])
    lab.now <- paste(yr.now, mo.now, sep="-")
    
    # This opens a connection to the hdf5 file; hdf5 is an precursor to netcdf, so ncdf4 will open those files
    fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", f.mo[i])) 
    # summary(fnow$var) # This will list what variables we can extract; drilling into a particular variable can get longer description of what that variable means 
    # test <- ncdf4::ncvar_get(fnow, "CB") # This is a sumple way to extract all the data so you can get a feel for it
    # names(fnow$var)
    soil.depth <- ncdf4::ncvar_get(fnow, "SLZ") # We won't get all the different layers, but we'll at least get the top & bottom
    
    # Setting up a dataframe of site level information
    # -- Note: We'll add to this with the patch and cohort level data, but I find it easier to work top-down
    # -- Note: _PY indicates "polygon", which means they're site level variables; ATM = "atmosphere", so those are drivers
    # names(fnow$var)[grep("_PY", names(fnow$var))]
    site.list[[lab.now]] <- data.frame(site=run.splt[1],
                                       GCM=run.splt[3],
                                       rcp=run.splt[4],
                                       co2=run.splt[5],
                                       Management=run.splt[6],
                                       level="site",
                                       year=yr.now,
                                       month=mo.now,
                                       
                                       # Drivers
                                       tair=ncdf4::ncvar_get(fnow, "MMEAN_ATM_TEMP_PY"), # K
                                       precipf=ncdf4::ncvar_get(fnow, "MMEAN_PCPG_PY")*sec2mo[mo.now], #kg/m2/mo
                                       lwdown=ncdf4::ncvar_get(fnow, "MMEAN_ATM_RLONG_PY"), # W/m2
                                       swdown=ncdf4::ncvar_get(fnow, "MMEAN_ATM_RSHORT_PY"), # W/m2
                                       qair=ncdf4::ncvar_get(fnow, "MMEAN_ATM_SHV_PY"), #kg/kg
                                       psurf=ncdf4::ncvar_get(fnow, "MMEAN_ATM_PRSS_PY"), # Pa
                                       wind=ncdf4::ncvar_get(fnow, "MMEAN_ATM_VELS_PY"), # m/s
                                       CO2=ncdf4::ncvar_get(fnow, "MMEAN_ATM_TEMP_PY"), # umol/umol
                                       
                                       # Forest structure (trees only)
                                       basal.area.tree = sum(ncdf4::ncvar_get(fnow, "BASAL_AREA_PY")[pfts.trees,]), # cm2/m2??
                                       density.tree = sum(ncdf4::ncvar_get(fnow, "NPLANT_PY")[pfts.trees,]), # tree/m2??
                                       
                                       # Carbon Storage & Flux
                                       agb=sum(ncdf4::ncvar_get(fnow, "AGB_PY")), # kg/m2??
                                       gpp=ncdf4::ncvar_get(fnow, "MMEAN_GPP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                       npp=ncdf4::ncvar_get(fnow, "MMEAN_NPP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                       nee=-ncdf4::ncvar_get(fnow, "MMEAN_NEP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                       cwd=ncdf4::ncvar_get(fnow, "MMEAN_CWD_C_PY"), # kgC/m2
                                       soil.c.fast=ncdf4::ncvar_get(fnow, "MMEAN_FAST_SOIL_C_PY"), # kgC/m2
                                       soil.c.slow=ncdf4::ncvar_get(fnow, "MMEAN_SLOW_SOIL_C_PY"), # kgC/m2
                                       soil.c.struc=ncdf4::ncvar_get(fnow, "MMEAN_STRUCT_SOIL_C_PY"), # kgC/m2
                                     
                                       # Energy Flux
                                       albedo.short=ncdf4::ncvar_get(fnow, "MMEAN_ALBEDO_PY"),
                                       par.ground=ncdf4::ncvar_get(fnow, "MMEAN_PAR_GND_PY"),
                                       
                                       # Soil, Hydrology & Water Flux
                                       soil.moist.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_WATER_PY")[length(soil.depth)], # m3/m3
                                       soil.moist.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_WATER_PY")[1], # m3/m3
                                       soil.moistpot.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_MSTPOT_PY")[length(soil.depth)], # ???
                                       soil.moistpot.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_MSTPOT_PY")[1], # ???
                                       soil.temp.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_TEMP_PY")[length(soil.depth)], # K
                                       soil.temp.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_TEMP_PY")[1], # K
                                       runoff.surface=ncdf4::ncvar_get(fnow, "MMEAN_RUNOFF_PY")*sec2mo[mo.now], # kg/m2/mo??
                                       runoff.subsurf=ncdf4::ncvar_get(fnow, "MMEAN_DRAINAGE_PY")*sec2mo[mo.now], # kg/m2/mo??
                                       transp = ncdf4::ncvar_get(fnow, "MMEAN_TRANSP_PY")*sec2mo[mo.now], # kg/m2/mo
                                       snow.depth=ncdf4::ncvar_get(fnow, "MMEAN_SFCW_DEPTH_PY"), # m
                                       swe=ncdf4::ncvar_get(fnow, "MMEAN_SFCW_MASS_PY"), # kg/m2
                                       
                                       
                                       # Other
                                       lai=sum(ncdf4::ncvar_get(fnow, "MMEAN_LAI_PY"))
                                       )

    # this is a dataframe of "patch" level information.  Patches are analogous to forest "stands", but the number and identity of patches will change through time based on how similar the trees in different stands are.
    # Cohorts are _analogous_ to unique trees and make up a stand.
    pch.list[[lab.now]] <- data.frame(site=run.splt[1],
                                      GCM=run.splt[3],
                                      rcp=run.splt[4],
                                      co2=run.splt[5],
                                      Management=run.splt[6],
                                      level="patches",
                                      year=yr.now,
                                      month=mo.now,
                                      
                                      # Patch-level data
                                      age=ncdf4::ncvar_get(fnow, "AGE"),
                                      area.patch=ncdf4::ncvar_get(fnow, "AREA"),
                                      n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                                      patch.co.start=ncdf4::ncvar_get(fnow, "PACO_ID")
                                      )
    
    pch.list[[lab.now]]$patchID <- 1:nrow(pch.list[[lab.now]])
    
    # # This is the cohort data, which is not dissimilar for tree-level data, but is similar to the patch 
    # #   data in that "trees" of similar size and same type (PFT) and gives each one a density in the patch
    # #   PFT= plant functional type =~ "species" (but not really)
    co.list[[lab.now]] <- data.frame(site=run.splt[1],
                                     GCM=run.splt[3],
                                     rcp=run.splt[4],
                                     co2=run.splt[5],
                                     Management=run.splt[6],
                                     level="cohorts",
                                     year=yr.now,
                                     month=mo.now,
                                     
                                     # Co-hort level data
                                     pft=ncdf4::ncvar_get(fnow, "PFT"), # 5=grass; 9=early hard; 10=mid hard; 11=late hard
                                     dbh=ncdf4::ncvar_get(fnow, "DBH"), # cm
                                     ba=ncdf4::ncvar_get(fnow, "BA_CO"), # cm2/tree
                                     carbs=ncdf4::ncvar_get(fnow, "CB")[13,], # Carbon Balance (carb reserves)? scale = 0(?) to 1; 1 = not stressed
                                     stress = ncdf4::ncvar_get(fnow, "CBR_BAR"), # Running mean relative carbon balance based on water + light stress; scale = -1 to 1
                                     height=ncdf4::ncvar_get(fnow, "HITE"), # meters
                                     lai=ncdf4::ncvar_get(fnow, "LAI_CO"),
                                     agb=ncdf4::ncvar_get(fnow, "AGB_CO"), # kgC/tree
                                     dens.pch=ncdf4::ncvar_get(fnow, "NPLANT") #trees/m2 PER PATCH
                                     )
    
    ncdf4::nc_close(fnow) # Closing the connection to the netcdf file; this is important otherwise your computer will get confused
  
    # -----------------
    # Merging patch and cohort information to help scale
    # -----------------
    # step 1: add in patch area etc. to the cohorts so we can scale up
    #   -- essentially need to do weighted averages to bring things to a forest-scale
    for(j in 1:nrow(pch.list[[lab.now]])){
      rows.pch <- (pch.list[[lab.now]]$patch.co.start[j]):(pch.list[[lab.now]]$patch.co.start[j]-1+pch.list[[lab.now]]$n.cohorts[j])
      
      co.list[[lab.now]][rows.pch, c("patchID", "patch.area", "patch.age")] <- pch.list[[lab.now]][j,c("patchID", "area.patch", "age")]
      
      co.list[[lab.now]][rows.pch,"dens.wt.pch"] <- co.list[[lab.now]]$dens.pch[rows.pch]/sum(co.list[[lab.now]]$dens.pch[rows.pch])
    }
    # tail(co.list[[lab.now]])
    
    # This is the weighted density of the trees that will let us scale to the forest
    # Trees / patch * patch/forest = trees/forest in units trees/m2
    co.list[[lab.now]]$dens.site <- co.list[[lab.now]]$dens.pch*co.list[[lab.now]]$patch.area
    co.list[[lab.now]]$dens.wt.site <- co.list[[lab.now]]$dens.site/sum(co.list[[lab.now]]$dens.site)
    
    
    # Extracting the tree data to make life easier
    dat.tree <- data.frame(pft=co.list[[lab.now]]$pft[co.list[[lab.now]]$pft %in% pfts.trees],
                           dbh=co.list[[lab.now]]$dbh[co.list[[lab.now]]$pft %in% pfts.trees],
                           hts=co.list[[lab.now]]$height[co.list[[lab.now]]$pft %in% pfts.trees],
                           pch=co.list[[lab.now]]$patchID[co.list[[lab.now]]$pft %in% pfts.trees],
                           wt.pch =co.list[[lab.now]]$dens.wt.pch[co.list[[lab.now]]$pft %in% pfts.trees],
                           wt.site=co.list[[lab.now]]$dens.wt.site[co.list[[lab.now]]$pft %in% pfts.trees]
                           )
    # -----------------

    # -----------------
    # Doing some aggregation of data to the patch level
    # AGB, BAtree, mean/max/sd height, mean/max/sd DBH
    # There's an R function called weighted.mean
    # To get weighted sd:     
    #      x=test.df$dbh
    #      w=test.df$dens/sum(test.df$dens)
    #      sqrt(sum(w * (x-weighted.mean(x, w))^2))
    # -----------------
    # Just doing this patches as a loop to make life easier at the moment
    pch.list[[lab.now]][,c("height.max", "height.mean", "height.sd", "dbh.max", "dbh.mean", "dbh.sd")] <- NA
    
    for(j in 1:nrow(pch.list[[lab.now]])){
      dat.pch <- dat.tree[dat.tree$pch==j,]
      if(nrow(dat.pch)==0) next
      
      pch.list[[lab.now]]$height.max[j] <- max(dat.pch$hts)
      pch.list[[lab.now]]$height.mean[j] <- weighted.mean(dat.pch$hts, dat.pch$wt.pch)
      pch.list[[lab.now]]$height.sd[j] <- sqrt(sum(dat.pch$wt.pch * (dat.pch$hts-pch.list[[lab.now]]$height.mean[j])^2))
      
      pch.list[[lab.now]]$dbh.max[j] <- max(dat.pch$dbh)
      pch.list[[lab.now]]$dbh.mean[j] <- weighted.mean(dat.pch$dbh, dat.pch$wt.pch)
      pch.list[[lab.now]]$dbh.sd[j] <- sqrt(sum(dat.pch$wt.pch * (dat.pch$dbh-pch.list[[lab.now]]$dbh.mean[j])^2))
      
    }
    
    # summary(pch.list[[lab.now]])
    # -----------------
    
    # -----------------
    # Doing some aggregation to the site level
    # To get to the site level total from cohort level: sum(x * dens.wt)
    # -----------------
    # AGB, BAtree, mean/max/sd height, mean/max/sd DBH
    site.list[[lab.now]]$height.mean <- weighted.mean(dat.tree$hts, dat.tree$wt.site)
    site.list[[lab.now]]$height.sd <- sqrt(sum(dat.tree$wt.site * (dat.tree$hts-site.list[[lab.now]]$height.mean)^2))

    site.list[[lab.now]]$dbh.mean <- weighted.mean(dat.tree$dbh, dat.tree$wt.site)
    site.list[[lab.now]]$dbh.sd <- sqrt(sum(dat.tree$wt.site * (dat.tree$dbh-site.list[[lab.now]]$dbh.mean)^2))
    # -----------------
    
    
    } # End File Loop
  
  site.df <- dplyr::bind_rows(site.list)
  pch.df <- dplyr::bind_rows(pch.list)
  co.df <- dplyr::bind_rows(co.list)
  rm(site.list, pch.list, co.list)
  
  write.csv(site.df, file.path(path.google, "output", paste(RUNID, "Site.csv", sep="_")), row.names=F) # Just save the site-level data on Google Too
  
  # Save locally (smallest file to biggest file)
  write.csv(site.df, file.path(path.out, paste(RUNID, "Site.csv", sep="_")), row.names=F)
  write.csv(pch.df, file.path(path.out, paste(RUNID, "Patch.csv", sep="_")), row.names=F)
  write.csv(co.df, file.path(path.out, paste(RUNID, "Cohort.csv", sep="_")), row.names=F)
  
  
} # End run loop

