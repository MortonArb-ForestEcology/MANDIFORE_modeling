

#-----------------------------------------------------------------------------------------------------------------

#12/21/22

#Adapted from 0_ExtractOutput_BAM.R (adapted from C Rollinson 1_ExtractOutput.R) to extract ED output from Morton Arb runs for the Learning Partnership people
#Not doing area weighted averaging, b/c in comparisons against just screening out the tiny trees and calc
  #things like normal it didn't actually really change anything, and it just takes more time to run
#broke code up into two parts so can run first part on server and second part on local machine
  #first part JUST extracts, doesn't do any scaling from cohort to patch

#-----------------------------------------------------------------------------------------------------------------

#Code inputs: 
#raw ED monthly output as netcdf

#what this code does:
#pulls in the raw ED monthly output as netcdf, looping through each year of output
#builds dataframe of site-level info (site, GCM, mgmt type, ats variables, soil/water/carbon/s(x) vars, etc.)
#builds df of patch level info (age, patch area, number of cohorts, etc.)
#builds df of cohort-level info (pft, dbh, height, lai, density, carbon balance, etc.)


#Code outputs:
#csv's for ED outputs at site, patch, cohort scale 
  #JUST directly extracted values, no scaling of cohort level variables to site level!!


#-----------------------------------------------------------------------------------------------------------------


#----------------
#install/load required packages
#----------------

library(lubridate)
#install.packages("stringr")
library(stringr)


#----------------
#Hard-coded variables
#----------------

path.out <- "/home/crollinson/MANDIFORE_modeling/MortonArb/1_runs/LearningPartnership_BAM/MortonArb_RAW_EXTRACTED_LP.v2" #first time around forgot to have patchID added to cohort df
#path.out <- "D:/Learning_partnership/extract_test" #testing, saving to external hard drive

pfts.grass <- 5
pfts.trees <- 6:11

dat.base <- "/home/crollinson/MANDIFORE_modeling/MortonArb/1_runs/MortonArb_ed_runs.v3" 
#dat.base <- "D:/Learning_partnership/raw_output_test"

#----------------
#Load data 
#----------------

if(!dir.exists(path.out)) dir.create(path.out)

#added [] to start from file #57, realized it stopped running when I turned off my computer last night
runs.raw <- dir(dat.base)[57:128] 


yr2sec <- 1/(365*24*60*60)
dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24
# runs.all <- data.frame()
#the empty lists get set up but the .nc files never get opened, maybe just do RUNID[1] type thing and manually go through each file
for(RUNID in runs.raw){
  co.list   <- list() #cohort
  pch.list  <- list() #patch
  site.list <- list() #site
#} #BAM added for testing, removing won't make all code loop through right though

  run.splt <- stringr::str_split(RUNID, "_")[[1]] # Splits apart the run name to extract different bits of info

# Some models have different num. hypens, so use model name as index
  mod.splt <- length(stringr::str_split(run.splt[3], "-")[[1]])

# Extracting Monthly Output
  f.mo <- dir(file.path(dat.base, RUNID, "analy"), "-E-")

  if(length(f.mo)==0) next

# Checking to make sure that it ran to 2099 if we're going to work on it
#if(strsplit(strsplit(f.mo[length(f.mo)], "_")[[1]][7], "-")[[1]]!="2099") next #think "_")[[1]][6] should be "_")[[1]][7] so pulls end year, got rid of [[1]][3]!="2099" in [[1]]!="2099", it didn't correspond to anything

#print("")
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
  #summary(fnow$var) # This will list what variables we can extract; drilling into a particular variable can get longer description of what that variable means 
  # test <- ncdf4::ncvar_get(fnow, "CB") # This is a simple way to extract all the data so you can get a feel for it
  # names(fnow$var)
  soil.depth <- ncdf4::ncvar_get(fnow, "SLZ") # We won't get all the different layers, but we'll at least get the top & bottom
  
  # Setting up a dataframe of site level information
  # -- Note: We'll add to this with the patch and cohort level data, but I find it easier to work top-down
  # -- Note: _PY indicates "polygon", which means they're site level variables; ATM = "atmosphere", so those are drivers
  #BAM:  _SI also means site I think, but only variables related to ats (precip, etc.) have this, so think _PY for all the ones of interest
  # names(fnow$var)[grep("_PY", names(fnow$var))]
  site.list[[lab.now]] <- data.frame(site=run.splt[1],
                                     GCM=run.splt[3],
                                     rcp=run.splt[4],
                                     co2=run.splt[5],
                                     Management=run.splt[6],
                                     level="site",
                                     year=yr.now,
                                     month=mo.now,
                                     
                                     # Climate Drivers
                                     tair=ncdf4::ncvar_get(fnow, "MMEAN_ATM_TEMP_PY"), # K
                                     precipf=ncdf4::ncvar_get(fnow, "MMEAN_PCPG_PY")*sec2mo[mo.now], #kg/m2/mo
                                     lwdown=ncdf4::ncvar_get(fnow, "MMEAN_ATM_RLONG_PY"), # W/m2
                                     swdown=ncdf4::ncvar_get(fnow, "MMEAN_ATM_RSHORT_PY"), # W/m2
                                     qair=ncdf4::ncvar_get(fnow, "MMEAN_ATM_SHV_PY"), #kg/kg
                                     psurf=ncdf4::ncvar_get(fnow, "MMEAN_ATM_PRSS_PY"), # Pa
                                     wind=ncdf4::ncvar_get(fnow, "MMEAN_ATM_VELS_PY"), # m/s
                                     CO2=ncdf4::ncvar_get(fnow, "MMEAN_ATM_CO2_PY"), # umol/mol
                                     VPD=ncdf4::ncvar_get(fnow, "MMEAN_ATM_VPDEF_PY"), # Pa
                                     #PAR.ats=ncdf4::ncvar_get(fnow, "MMEAN_ATM_PAR_PY"), # W/m2, BAM
                                     
                                     # Forest structure (trees only)
                                     #both of these are now calc w/weighted avg's using patch/cohort data
                                     #basal.area.tree = sum(ncdf4::ncvar_get(fnow, "BASAL_AREA_PY")[pfts.trees,]), # cm2/m2
                                     #BAM: this can't be right, get densities of like 150 trees/m2...jk think it is, counts teeny tiny stems
                                     #density.tree = sum(ncdf4::ncvar_get(fnow, "NPLANT_PY")[pfts.trees,]), # trees/m2
                                     
                                     # Carbon Storage & Flux
                                     agb=sum(ncdf4::ncvar_get(fnow, "AGB_PY")), # kg/m2
                                     gpp=ncdf4::ncvar_get(fnow, "MMEAN_GPP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                     npp=ncdf4::ncvar_get(fnow, "MMEAN_NPP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                     nee=-ncdf4::ncvar_get(fnow, "MMEAN_NEP_PY")*yr2sec*sec2mo[mo.now], # kgC/m2/mo
                                     cwd=ncdf4::ncvar_get(fnow, "MMEAN_CWD_C_PY"), # kgC/m2
                                     soil.c.fast=ncdf4::ncvar_get(fnow, "MMEAN_FAST_SOIL_C_PY"), # kgC/m2
                                     soil.c.slow=ncdf4::ncvar_get(fnow, "MMEAN_SLOW_SOIL_C_PY"), # kgC/m2
                                     soil.c.struc=ncdf4::ncvar_get(fnow, "MMEAN_STRUCT_SOIL_C_PY"), # kgC/m2
                                     #auto.resp=ncdf4::ncvar_get(fnow, "MMEAN_PLRESP_PY")*yr2sec*sec2mo[mo.now], #kgC/m2/mo, BAM
                                     #hetero.resp=ncdf4::ncvar_get(fnow, "MMEAN_RH_PY")*yr2sec*sec2mo[mo.now], #kgC/m2/mo, BAM
                                     
                                     # Energy Flux
                                     albedo.short=ncdf4::ncvar_get(fnow, "MMEAN_ALBEDO_PY"), #unitless
                                     par.ground=ncdf4::ncvar_get(fnow, "MMEAN_PAR_GND_PY"), #W/m2
                                     #Cnet.rad=ncdf4::ncvar_get(fnow, "MMEAN_RNET_PY"), #W/m2, BAM
                                     #sensible.heat=ncdf4::ncvar_get(fnow, "MMEAN_SENSIBLE_GC_PY"), #W/m2, BAM
                                     
                                     # Soil, Hydrology & Water Flux
                                     soil.moist.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_WATER_PY")[length(soil.depth)], # m3/m3
                                     soil.moist.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_WATER_PY")[1], # m3/m3
                                     soil.moistpot.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_MSTPOT_PY")[length(soil.depth)], # m, some unit for matric potential
                                     soil.moistpot.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_MSTPOT_PY")[1], # ???
                                     soil.temp.surf=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_TEMP_PY")[length(soil.depth)], # K
                                     soil.temp.deep=ncdf4::ncvar_get(fnow, "MMEAN_SOIL_TEMP_PY")[1], # K
                                     transp = ncdf4::ncvar_get(fnow, "MMEAN_TRANSP_PY")*sec2mo[mo.now], # kg/m2/mo
                                     snow.depth=ncdf4::ncvar_get(fnow, "MMEAN_SFCW_DEPTH_PY"), # m
                                     swe=ncdf4::ncvar_get(fnow, "MMEAN_SFCW_MASS_PY"), # kg/m2, snow water equivalent
                                     #canopy.conduct=ncdf4::ncvar_get(fnow, "MMEAN_CAN_GGND_PY")*sec2mo[mo.now], #m/mo, BAM
                                     #stomatal.conduct=ncdf4::ncvar_get(fnow, "MMEAN_LEAF_GSW_PY")*sec2mo[mo.now], #kg/m2leaf/mo, BAM
                                     #vapor.flux=ncdf4::ncvar_get(fnow, "MMEAN_VAPOR_GC_PY")*sec2mo[mo.now], #kg/m2/mo, BAM
                                     
                                     
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
                                    area.patch=ncdf4::ncvar_get(fnow, "AREA"), #m2
                                    n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                                    patch.co.start=ncdf4::ncvar_get(fnow, "PACO_ID")
                                    #water.deficit=ncdf4::ncvar_get(fnow, "AVG_MONTHLY_WATERDEF"), #kg/m2/30days, BAM added
                                    #LW.out=ncdf4::ncvar_get(fnow, "MMEAN_RLONGUP_PY") #W/m2 BAM added
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
                                   ba=ncdf4::ncvar_get(fnow, "BA_CO"), # cm2
                                   #stress = ncdf4::ncvar_get(fnow, "CBR_BAR"), # Running mean relative carbon balance based on water + light stress; scale = -1 to 1
                                   height=ncdf4::ncvar_get(fnow, "HITE"), # meters
                                   lai=ncdf4::ncvar_get(fnow, "LAI_CO"),
                                   agb=ncdf4::ncvar_get(fnow, "AGB_CO"), # kgC/tree
                                   dens.pch=ncdf4::ncvar_get(fnow, "NPLANT"), #trees/m2 PER PATCH of a certain cohort
                                   crown.area=ncdf4::ncvar_get(fnow, "CROWN_AREA_CO") #m2 BAM
                                   #mort.rate=ncdf4::ncvar_get(fnow, "MMEAN_MORT_RATE_CO")[1] #1/yr, reported as fraction of the population
  )
  
  ncdf4::nc_close(fnow) # Closing the connection to the netcdf file; this is important otherwise your computer will get confused
  
  #need to add patchID to the cohort df
  for(j in 1:nrow(pch.list[[lab.now]])){
    rows.pch <- (pch.list[[lab.now]]$patch.co.start[j]):(pch.list[[lab.now]]$patch.co.start[j]-1+pch.list[[lab.now]]$n.cohorts[j])

    co.list[[lab.now]][rows.pch, c("patchID", "patch.area", "patch.age")] <- pch.list[[lab.now]][j,c("patchID", "area.patch", "age")]
  }
  
} # End File Loop 


  site.df <- dplyr::bind_rows(site.list)
  pch.df <- dplyr::bind_rows(pch.list)
  co.df <- dplyr::bind_rows(co.list)

  rm(site.list, pch.list, co.list)

#export these three df's, then can just run the actual extraction on server and download the 3 df's, then
  #run the rest of the code on personal machine
  write.csv(site.df, file.path(path.out, paste(RUNID, "Site_extract.csv", sep="_")), row.names=F)
  write.csv(pch.df, file.path(path.out, paste(RUNID, "Patch_extract.csv", sep="_")), row.names=F)
  write.csv(co.df, file.path(path.out, paste(RUNID, "Cohort_extract.csv", sep="_")), row.names=F)

  rm(site.df, co.df, pch.df)

}

