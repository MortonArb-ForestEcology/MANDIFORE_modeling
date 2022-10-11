# Comparing size distributions between understory and overstory thin attempts
#library(ggplot2)

#path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"

dat.base <- "../../../../crollinson/MANDIFORE_modeling/MortonArb/1_runs/MortonArb_ed_runs.v3/"
runs.done <- dir(dat.base, "statCO2")  # This gets a list of model ensemble members for static CO2 only; adding increases in CO2 that will make trees more efficient has been a low priority because ED is unrealistically sensitive

#runs.all <- data.frame()
for(RUNID in runs.done){
  run.splt <- stringr::str_split(RUNID, "_")[[1]] # Splits apart the run name to extract different bits of info
  
  # Some models have different num. hypens, so use model name as index
  mod.splt <- length(stringr::str_split(run.splt[3], "-")[[1]])
  
  # For our own sanity for now, lets just work with the Yearly Output.  
  #  -- Right now I'm also saving monthly output and we could do daily or subdaily if we wanted, 
  #     but that slows it down and requires MASSIVE storage space for runs like we're doing
  f.yr <- dir(file.path(dat.base, RUNID, "analy"), "-E-")
  if(identical(f.yr, character(0))){
    print("NO FILE FOUND")
  } else{
    print("")
    print(RUNID)
    
    # Setting up a loop to go through each year of model output
    df.run <- data.frame()
    df.run <- data.frame()
    for(FILE in f.yr){
      
      
      # Splitting apart the file name so we can pull out important info such as the year
      f.split <- strsplit(FILE, "-")[[1]]
      yr.now <- as.numeric(f.split[mod.splt+2])
      mo.now <- as.numeric(f.split[mod.splt+3])
      
      # This opens a connection to the hdf5 file; hdf5 is an precursor to netcdf, so ncdf4 will open those files
      fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", FILE)) 
      # summary(fnow$var) # This will list what variables we can extract; drilling into a particular variable can get longer description of what that variable means 
      # test <- ncdf4::ncvar_get(fnow, "CB") # This is a sumple way to extract all the data so you can get a feel for it
      
      # this is a dataframe of "patch" level information.  Patches are analogous to forest "stands", but the number and identity of patches will change through time based on how similar the trees in different stands are.
      # Cohorts are _analogous_ to unique trees and make up a stand.
      df.pch <- data.frame(year=yr.now,
                           month=mo.now,
                           patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"), # Something that links cohorts ot pathces; i _think_ which cohort row each patch starts on
                           n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"), # How many cohorts are in a patch 
                           patch.area = ncdf4::ncvar_get(fnow, "AREA"), # How big each patch is -- what percent of the area is taken up by forests or stands like each one?
                           patch.age = ncdf4::ncvar_get(fnow, "AGE"), # How "old" a patch is -- time since last major disturbance.
                           temp.air= ncdf4::ncvar_get(fnow, "MMEAN_ATM_TEMP_PY")-273.16,
                           CO2.air = ncdf4::ncvar_get(fnow, "MMEAN_ATM_CO2_PY"),
                           precip  = ncdf4::ncvar_get(fnow, "MMEAN_PCPG_PY"),
                           runoff = ncdf4::ncvar_get(fnow, "MMEAN_RUNOFF_PY"),
                           temp.ground=ncdf4::ncvar_get(fnow, "MMEAN_GND_TEMP_PY")-273.16, # Just getting surface soil temp
                           par.ground = ncdf4::ncvar_get(fnow, "MMEAN_PAR_GND_PY")) 
      df.pch$patchID <- 1:nrow(df.pch)
      # summary(df.pch)
      
      # This is the cohort data, which is not dissimilar for tree-level data, but is similar to the patch 
      #   data in that "trees" of similar size and same type (PFT) and gives each one a density in the patch
      #   PFT= plant functional type =~ "species" (but not really)
      df.co <- data.frame(year=yr.now,
                          month=mo.now,
                          PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")), # Which type of tree is this
                          Density=ncdf4::ncvar_get(fnow, "NPLANT"), # What is the density for this cohort; (trees/m2)
                          DBH=ncdf4::ncvar_get(fnow, "DBH"), # Diameter for this cohort (cm/tree)
                          AGB=ncdf4::ncvar_get(fnow, "AGB_CO"), # Aboveground biomass for the cohort (kg/tree)
                          BA=ncdf4::ncvar_get(fnow, "BA_CO"), # Basal area for the cohort (cm2/tree)
                          Stress=ncdf4::ncvar_get(fnow, "CBR_BAR"), #This is a runnign carbon balance average for the tree; ranges from -1 to 1 I think; this might get us stress; computed from light- and water-induced stress
                          LAI = ncdf4::ncvar_get(fnow, "LAI_CO")) 
      # summary(df.co)
      
      ncdf4::nc_close(fnow) # Closing the connection to the netcdf file; this is important otherwise your computer will get confused
      
      # Merging patch and cohort information
      
      # step 1: add in patch area etc. to the cohorts so we can scale up 
      #   -- essentially need to do weighted averages to bring things to a forest-scale
      for(i in 1:nrow(df.pch)){
        df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("temp.air", "CO2.air", "precip", "runoff", "temp.ground", "par.ground", "patchID", "patch.area", "patch.age")] <- df.pch[i,c("temp.air", "CO2.air", "precip", "runoff", "temp.ground", "par.ground","patchID", "patch.area", "patch.age")]
      }
      # tail(df.co)
      
      # This is the weighted density of the trees that will let us scale to the forest
      # Trees / patch * patch/forest = trees/forest in units trees/m2
      df.co$dens.wt <- df.co$Density*df.co$patch.area
      
      # Using the density to created weighted values for certain groupings
      for(PFT in unique(df.co$PFT)){
        df.co[df.co$PFT==PFT,"pft.wt"] <- df.co$dens.wt[df.co$PFT==PFT]/sum(df.co$dens.wt[df.co$PFT==PFT])
        df.co[df.co$PFT==PFT & df.co$DBH>45,"pft.wt.g45"] <- df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45]/sum(df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45])
      }
      summary(df.co)
      
      df.run <- rbind(df.run, df.co)
    }
    
    # Extracting the key info abotu which ensemble member this is all from
    # run.splt
    df.run$RunID <- as.factor(RUNID)
    df.run$GCM <- as.factor(run.splt[3])
    df.run$RCP <- as.factor(run.splt[4])
    df.run$CO2 <- as.factor(run.splt[5])
    df.run$Management <- as.factor(substr(run.splt[6], 5, nchar(run.splt[6])))
    
    df.run$AGB.wt <- df.run$AGB*df.run$dens.wt
    df.run$BA.wt <- df.run$BA*df.run$dens.wt
    df.run$Stress.wt.pft <- df.run$Stress*df.run$pft.wt
    df.run$Stress.wt.pft.g45 <- df.run$Stress*df.run$pft.wt.g45
    
    write.csv(df.run, file.path(paste0("../data/", RUNID, "_Cohort_Monthly.csv")), row.names = F)
    #runs.all <- rbind(runs.all, df.run)
    # Clean up: Turning things into factors; unit transformations; prettier labels
    # NOTE: This is output at the cohort level and is very unweildy; probably don't want to make a single csv as we build the data.
    #runs.all$Management <- factor(runs.all$Management, levels=c("None", "Under", "Shelter", "Gap"))
    #runs.all$CO2 <- ifelse(runs.all$CO2 == "statC02", "static", "dynamic")
    #--------------------------#
    #FOR PATCH LEVEL CSV
    #--------------------------#
    
    # Aggregating data from cohorts to PFT-scale (so 1 value per PFT per year)
    runs.pch <- aggregate(df.run[df.run$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft", "Stress.wt.pft.g45")],
                          by=df.run[df.run$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground", "patchID", "PFT")],
                          FUN=sum, na.rm=T)
    summary(runs.pch); dim(runs.pch)
    
    
    # Getting the number of 45cm (~18") DBH stems; same as above, but just for big trees
    runs.pch2 <- aggregate(df.run[df.run$PFT!=5 & df.run$DBH>45,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft.g45")],
                           by=df.run[df.run$PFT!=5 & df.run$DBH>45,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground", "patchID", "PFT")],
                           FUN=sum)
    names(runs.pch2)[16:19] <- c("AGB.g45", "BA.g45", "Density.g45", "Stress.g45")
    summary(runs.pch2); dim(runs.pch2)
    
    
    # Same as above, but for saplings; including <1 cm is things that die very quickly (seedlings)
    runs.pch3 <- aggregate(df.run[df.run$PFT!=5 & df.run$DBH>1 & df.run$DBH<10,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft")],
                           by=df.run[df.run$PFT!=5 & df.run$DBH>1 & df.run$DBH<10,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground", "patchID", "PFT")],
                           FUN=sum)
    names(runs.pch3)[16:19] <- c("AGB.sap", "BA.sap", "Density.sap", "Stress.sap")
    summary(runs.pch3); dim(runs.pch2)
    
    runs.pch <- merge(runs.pch, runs.pch2, all.x=T)
    runs.pch[is.na(runs.pch$AGB.g45), c("AGB.g45", "BA.g45", "Density.g45")] <- 0
    runs.pch <- merge(runs.pch, runs.pch3, all.x=T)
    runs.pch[is.na(runs.pch$AGB.sap), c("AGB.sap", "BA.sap", "Density.sap")] <- 0
    summary(runs.pch)
    
    runs.pch.tot <- aggregate(df.run[df.run$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt")],
                              by=df.run[df.run$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground", "patchID")],
                              FUN=sum)
    names(runs.pch.tot)[15:17] <- c("AGB.tot", "BA.tot", "Density.Tot")
    summary(runs.pch.tot)
    
    
    runs.pch <- merge(runs.pch, runs.pch.tot, all.x=T)
    runs.pch$AGB.prop <- runs.pch$AGB.wt/runs.pch$AGB.tot
    runs.pch$BA.prop <- runs.pch$BA.wt/runs.pch$BA.tot
    summary(runs.pch)
    
    write.csv(runs.pch, file.path(paste0("../data/", RUNID, "_Patch_Monthly.csv")), row.names = F)
    
    #--------------------------#
    #FOR SITE LEVEL CSV
    #--------------------------#
    # Aggregating data from cohorts to PFT-scale (so 1 value per PFT per year)
    runs.site <- aggregate(df.run[df.run$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft", "Stress.wt.pft.g45")],
                           by=df.run[df.run$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground", "PFT")],
                           FUN=sum, na.rm=T)
    summary(runs.site); dim(runs.site)
    
    
    # Getting the number of 45cm (~18") DBH stems; same as above, but just for big trees
    runs.site2 <- aggregate(df.run[df.run$PFT!=5 & df.run$DBH>45,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft.g45")],
                            by=df.run[df.run$PFT!=5 & df.run$DBH>45,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground",  "PFT")],
                            FUN=sum)
    names(runs.site2)[15:18] <- c("AGB.g45", "BA.g45", "Density.g45", "Stress.g45")
    summary(runs.site2); dim(runs.site2)
    
    
    # Same as above, but for saplings; including <1 cm is things that die very quickly (seedlings)
    runs.site3 <- aggregate(df.run[df.run$PFT!=5 & df.run$DBH>1 & df.run$DBH<10,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft")],
                            by=df.run[df.run$PFT!=5 & df.run$DBH>1 & df.run$DBH<10,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month","temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground",  "PFT")],
                            FUN=sum)
    names(runs.site3)[15:18] <- c("AGB.sap", "BA.sap", "Density.sap", "Stress.sap")
    summary(runs.site3); dim(runs.site2)
    
    runs.site <- merge(runs.site, runs.site2, all.x=T)
    runs.site[is.na(runs.site$AGB.g45), c("AGB.g45", "BA.g45", "Density.g45")] <- 0
    runs.site <- merge(runs.site, runs.site3, all.x=T)
    runs.site[is.na(runs.site$AGB.sap), c("AGB.sap", "BA.sap", "Density.sap")] <- 0
    summary(runs.site)
    
    runs.site.tot <- aggregate(df.run[df.run$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt")],
                               by=df.run[df.run$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "month", "temp.air","CO2.air", "precip", "runoff","temp.ground", "par.ground")],
                               FUN=sum)
    names(runs.site.tot)[14:16] <- c("AGB.tot", "BA.tot", "Density.Tot")
    summary(runs.site.tot)
    
    
    runs.site <- merge(runs.site, runs.site.tot, all.x=T)
    runs.site$AGB.prop <- runs.site$AGB.wt/runs.site$AGB.tot
    runs.site$BA.prop <- runs.site$BA.wt/runs.site$BA.tot
    summary(runs.site)
    
    write.csv(runs.site, file.path(paste0("../data/", RUNID, "_Cohort_Monthly.csv")), row.names = F)
  }
}