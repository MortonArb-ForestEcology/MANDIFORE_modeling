# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"
path.out <- "extract.v3"

if(!dir.exists(path.out)) dir.create(path.out)

dat.base <- "../1_runs/MortonArb_ed_runs.v3/"
runs.done <- dir(dat.base, "statCO2")  # This gets a list of model ensemble members for static CO2 only; adding increases in CO2 that will make trees more efficient has been a low priority because ED is unrealistically sensitive

yr2sec <- 1/(365*24*60*60)
dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24
# runs.all <- data.frame()
for(RUNID in runs.done){
  co.df   <- data.frame()
  pch.df  <- data.frame()
  site.df <- data.frame()
  
  run.splt <- stringr::str_split(RUNID, "_")[[1]] # Splits apart the run name to extract different bits of info
  
  # Some models have different num. hypens, so use model name as index
  mod.splt <- length(stringr::str_split(run.splt[3], "-")[[1]])
  
  # Extracting Monthly Output
  #  -- Right now I'm also saving monthly output and we could do daily or subdaily if we wanted, 
  #     but that slows it down and requires MASSIVE storage space for runs like we're doing
  f.mo <- dir(file.path(dat.base, RUNID, "analy"), "-E-")
  
  # Checking to make sure that it ran to 2099 if we're going to work on it
  if(strsplit(strsplit(f.mo[length(f.mo)], "_")[[1]][6], "-")[[1]][3]!="2099") next
  
  print("")
  print(RUNID)
  
  # Setting up a loop to go through each year of model output
  pb <- txtProgressBar(min=0, max=length(f.mo), style=3); #pb.ind=0
  # df.run <- data.frame()
  # df.run <- data.frame()
  for(i in 1:length(f.mo)){
    # pb.ind=pb.ind+1
    setTxtProgressBar(pb, i)
    
    # Splitting apart the file name so we can pull out important info such as the year
    f.split <- strsplit(f.mo[i], "-")[[1]]
    yr.now <- as.numeric(f.split[mod.splt+2])
    mo.now <- as.numeric(f.split[mod.splt+3])
    
    # This opens a connection to the hdf5 file; hdf5 is an precursor to netcdf, so ncdf4 will open those files
    fnow <- ncdf4::nc_open(file.path(dat.base, RUNID, "analy", f.mo[i])) 
    # summary(fnow$var) # This will list what variables we can extract; drilling into a particular variable can get longer description of what that variable means 
    # test <- ncdf4::ncvar_get(fnow, "CB") # This is a sumple way to extract all the data so you can get a feel for it
    # names(fnow$var)
    soil.depth <- ncdf4::ncvar_get(fnow, "SLZ") # We won't get all the different layers, but we'll at least get the top & bottom
    
    # Setting up a dataframe of site level information
    # -- Note: We'll add to this with the patch and cohort level data, but I find it easier to work top-down
    # -- Note: _PY indicates "polygon", which means they're site level variables; ATM = "atmosphere", so those are drivers
    df.site <- data.frame(year=yr.now,
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

                          # Carbon Storage & Flux
                          agb=sum(ncdf4::ncvar_get(fnow, "AGB_PY")), # kg/m2??
                          basal.area = sum(ncdf4::ncvar_get(fnow, "BASAL_AREA_PY")), # cm2/m2??
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
                          
                          # Energetics
                          albedo=ncdf4::ncvar_get(fnow, "MMEAN_ALBEDO_PY"),
                          
                          # Other
                          lai=sum(ncdf4::ncvar_get(fnow, "MMEAN_LAI_PY"))
                          )
    
    # this is a dataframe of "patch" level information.  Patches are analogous to forest "stands", but the number and identity of patches will change through time based on how similar the trees in different stands are.
    # Cohorts are _analogous_ to unique trees and make up a stand.
    # df.pch <- data.frame(year=yr.now,
    #                      month=mo.now,
    #                      patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"), # Something that links cohorts ot pathces; i _think_ which cohort row each patch starts on
    #                      n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"), # How many cohorts are in a patch 
    #                      patch.area = ncdf4::ncvar_get(fnow, "AREA"), # How big each patch is -- what percent of the area is taken up by forests or stands like each one?
    #                      patch.age = ncdf4::ncvar_get(fnow, "AGE")) # How "old" a patch is -- time since last major disturbance.
    # df.pch$patchID <- 1:nrow(df.pch)
    # # summary(df.pch)
    # 
    # # This is the cohort data, which is not dissimilar for tree-level data, but is similar to the patch 
    # #   data in that "trees" of similar size and same type (PFT) and gives each one a density in the patch
    # #   PFT= plant functional type =~ "species" (but not really)
    # df.co <- data.frame(year=yr.now,
    #                     month=mo.now,
    #                     PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")), # Which type of tree is this
    #                     Density=ncdf4::ncvar_get(fnow, "NPLANT"), # What is the density for this cohort; (trees/m2)
    #                     DBH=ncdf4::ncvar_get(fnow, "DBH"), # Diameter for this cohort (cm/tree)
    #                     AGB=ncdf4::ncvar_get(fnow, "AGB_CO"), # Aboveground biomass for the cohort (kg/tree)
    #                     BA=ncdf4::ncvar_get(fnow, "BA_CO"), # Basal area for the cohort (cm2/tree)
    #                     Stress=ncdf4::ncvar_get(fnow, "CBR_BAR")) # This is a runnign carbon balance average for the tree; ranges from -1 to 1 I think; this might get us stress; computed from light- and water-induced stress
    # # summary(df.co)
    # 
    ncdf4::nc_close(fnow) # Closing the connection to the netcdf file; this is important otherwise your computer will get confused
    # 
    # # Merging patch and cohort information
    # 
    # # step 1: add in patch area etc. to the cohorts so we can scale up 
    # #   -- essentially need to do weighted averages to bring things to a forest-scale
    # for(i in 1:nrow(df.pch)){
    #   df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("patchID", "patch.area", "patch.age")] <- df.pch[i,c("patchID", "patch.area", "patch.age")]
    # }
    # # tail(df.co)
    # 
    # # This is the weighted density of the trees that will let us scale to the forest
    # # Trees / patch * patch/forest = trees/forest in units trees/m2
    # df.co$dens.wt <- df.co$Density*df.co$patch.area
    # 
    # # Using the density to created weighted values for certain groupings
    # for(PFT in unique(df.co$PFT)){
    #   df.co[df.co$PFT==PFT,"pft.wt"] <- df.co$dens.wt[df.co$PFT==PFT]/sum(df.co$dens.wt[df.co$PFT==PFT])
    #   df.co[df.co$PFT==PFT & df.co$DBH>45,"pft.wt.g45"] <- df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45]/sum(df.co$dens.wt[df.co$PFT==PFT & df.co$DBH>45])
    # }
    # summary(df.co)
    # 
    # df.run <- rbind(df.run, df.co)
    
    site.df <- rbind(site.df, df.site) # This is the slow way of doing it, but it'll work for now
  } # End File Loop
  
  write.csv(site.df, file.path(path.out, paste(RUNID, "Site.csv", sep="_")), row.names=F)
  write.csv(site.df, file.path(path.google, "output", paste(RUNID, "Site.csv", sep="_")), row.names=F) # Just save the site-level data on Google Too
  # Extracting the key info abotu which ensemble member this is all from
  # run.splt
  # df.run$RunID <- as.factor(RUNID)
  # df.run$GCM <- as.factor(run.splt[3])
  # df.run$RCP <- as.factor(run.splt[4])
  # df.run$CO2 <- as.factor(run.splt[5])
  # df.run$Management <- as.factor(substr(run.splt[6], 5, nchar(run.splt[6])))
  # 
  # runs.all <- rbind(runs.all, df.run)
}

# Clean up: Turning things into factors; unit transformations; prettier labels
# NOTE: This is output at the cohort level and is very unweildy; probably don't want to make a single csv as we build the data.
# runs.all$Management <- factor(runs.all$Management, levels=c("None", "Under", "Shelter", "Gap"))
# runs.all$CO2 <- car::recode(runs.all$CO2, "'CO2'='dynamic'; 'statCO2'='static'")
# runs.all$AGB.wt <- runs.all$AGB*runs.all$dens.wt
# runs.all$BA.wt <- runs.all$BA*runs.all$dens.wt
# runs.all$Stress.wt.pft <- runs.all$Stress*runs.all$pft.wt
# runs.all$Stress.wt.pft.g45 <- runs.all$Stress*runs.all$pft.wt.g45
# summary(runs.all)

write.csv(runs.all, file.path(path.google, "output", "Summary_PFTs_Cohort_Year.csv"), row.names=F)

summary(runs.all[runs.all$GCM=="ACCESS1-0" & runs.all$year>2030 & runs.all$PFT!=5,])

# ------------------------------------
# Glance at and aggregate the data
# Notably, the aggregation functions turn the cohort data to forest-level data
# Note: PFT 5 is grass and does weird things, so we leave it out with size-based metrics
# ------------------------------------
ggplot(data=runs.all[runs.all$PFT!=5 & runs.all$DBH>10,])+
  facet_grid(Management ~ GCM) +
  geom_histogram(aes(x=DBH, fill=PFT))

ggplot(data=runs.all[runs.all$PFT!=5 & runs.all$DBH>10,])+
  facet_grid(Management ~ GCM) +
  geom_histogram(aes(x=Stress, fill=PFT))

summary(runs.all[runs.all$GCM=="bcc-csm1-1" & runs.all$PFT!=5,])

# Aggregating data from cohorts to PFT-scale (so 1 value per PFT per year)
runs.mo <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft", "Stress.wt.pft.g45")],
                     by=runs.all[runs.all$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                     FUN=sum, na.rm=T)
summary(runs.mo); dim(runs.mo)


# Getting the number of 45cm (~18") DBH stems; same as above, but just for big trees
runs.mo2 <- aggregate(runs.all[runs.all$PFT!=5 & runs.all$DBH>45,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft.g45")],
                      by=runs.all[runs.all$PFT!=5 & runs.all$DBH>45,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                      FUN=sum)
names(runs.mo2)[8:11] <- c("AGB.g45", "BA.g45", "Density.g45", "Stress.g45")
summary(runs.mo2); dim(runs.mo2)


# Same as above, but for saplings; including <1 cm is things that die very quickly (seedlings)
runs.mo3 <- aggregate(runs.all[runs.all$PFT!=5 & runs.all$DBH>1 & runs.all$DBH<10,c("AGB.wt", "BA.wt", "dens.wt", "Stress.wt.pft")],
                      by=runs.all[runs.all$PFT!=5 & runs.all$DBH>1 & runs.all$DBH<10,c("RunID", "GCM", "RCP", "CO2", "Management", "year", "PFT")],
                      FUN=sum)
names(runs.mo3)[8:11] <- c("AGB.sap", "BA.sap", "Density.sap", "Stress.sap")
summary(runs.mo3); dim(runs.mo2)

runs.mo <- merge(runs.mo, runs.mo2, all.x=T)
runs.mo[is.na(runs.mo$AGB.g45), c("AGB.g45", "BA.g45", "Density.g45")] <- 0
runs.mo <- merge(runs.mo, runs.mo3, all.x=T)
runs.mo[is.na(runs.mo$AGB.sap), c("AGB.sap", "BA.sap", "Density.sap")] <- 0
summary(runs.mo)

runs.mo.tot <- aggregate(runs.all[runs.all$PFT!=5,c("AGB.wt", "BA.wt", "dens.wt")],
                         by=runs.all[runs.all$PFT!=5,c("RunID", "GCM", "RCP", "CO2", "Management", "year")],
                         FUN=sum)
names(runs.mo.tot)[7:9] <- c("AGB.tot", "BA.tot", "Density.Tot")
summary(runs.mo.tot)


runs.mo <- merge(runs.mo, runs.mo.tot, all.x=T)
runs.mo$AGB.prop <- runs.mo$AGB.wt/runs.mo$AGB.tot
runs.mo$BA.prop <- runs.mo$BA.wt/runs.mo$BA.tot
summary(runs.mo)

write.csv(runs.mo, file.path(path.google, "output", "Summary_PFTs_Site_Year.csv"), row.names=F)
# ------------------------------------

# ------------------------------------
# Plot the data
# These figures aren't great, but are what's in the google drive folder
# ------------------------------------
path.figs <- file.path(path.google, "figures/ensemble/explore")
# dir.exists(path.figs)

png(file.path(path.figs, "Explore_AGB_by_PFT_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[,]) +
  facet_grid(GCM ~ Management) +
  # facet_wrap( ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=PFT, linetype=RCP), size=1.5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0,max(runs.mo$AGB.wt)), expand=c(0,0)) +
  ggtitle("Aboveground Biomass by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_AGB_by_PFT_Time_v2.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[,]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=Management, linetype=RCP), size=1.5) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0,max(runs.mo$AGB.wt)), expand=c(0,0)) +
  ggtitle("Aboveground Biomass by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_All.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[,]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=PFT, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_All_v2.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[,]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=Management, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_Stress_by_PFT_Time_BigTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[,]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.g45, color=PFT, linetype=RCP)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Stress Trees >45 cm DBH by PFT") +
  theme_bw() 
dev.off()

png(file.path(path.figs, "Explore_AGB_Total_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.tot, color=Management), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)", limits=c(0, max(runs.mo$AGB.tot, na.rm=T))) +
  ggtitle("Total Aboveground Biomass") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_by_PFT_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=PFT, linetype=RCP)) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Basal Area by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_by_PFT_Time_v2.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=Management, linetype=RCP)) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Basal Area by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ Management) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=PFT, linetype=RCP))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.mo$Density.g45)), expand=c(0,0)) +
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Density_by_PFT_Time_BigTrees_v2.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.g45, color=Management, linetype=RCP)) +
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.mo$Density.g45)), expand=c(0,0)) +
  ggtitle("Density Tress >45 cm DBH by PFT") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Density_by_PFT_Time_SmallTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT!="5",]) +
  facet_grid(GCM ~ PFT) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Density.sap,
                color=Management, linetype=RCP))+
  scale_y_continuous(name="Stem Density (stems/m2)", limits=c(0, max(runs.mo$Density.sap)), expand=c(0,0)) +
  ggtitle("Density Saplings 1-10 cm DBH by PFT") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_AGB_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=AGB.wt, color=Management, linetype=RCP), size=1.5) +
  scale_y_continuous(name="Aboveground Biomass (kgC/m2)") +
  ggtitle("Oak Aboveground Biomass (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_BA_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.wt, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Basal Area (cm2/m2)") +
  ggtitle("Oak Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_PropBA_PFT10_Time.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=BA.prop, color=Management, linetype=CO2), size=1.5) +
  scale_y_continuous(name="Proportion") +
  ggtitle("Oak Proportion by Basal Area (PFT 10)") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_All.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress (All Oaks)") +
  theme_bw()
dev.off()


png(file.path(path.figs, "Explore_Stress_PFT10_Time_BigTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.wt.pft.g45, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.5, 0, 0.5, 1), labels = c("-0.5\n(Extremely Bad)", "0\n(Very Bad)", "0.5\n(Stressed)", "1.0\n(Happy)")) +
  ggtitle("Average Oak Stress >45 cm DBH") +
  theme_bw()
dev.off()

png(file.path(path.figs, "Explore_Stress_PFT10_Time_SmallTrees.png"), height=10, width=8, units="in", res=120)
ggplot(data=runs.mo[runs.mo$PFT=="10",]) +
  facet_grid(GCM ~ RCP) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, alpha=0.1) +
  geom_line(aes(x=year, y=Stress.sap, color=Management, linetype=CO2), size=1) +
  scale_y_continuous(name="Stress Index", breaks=c(-0.1, 0, 0.025, 0.04), labels = c("-0.1\n(Even Worse)", "0\n(Very Bad)", "0.025\n(Not Much Better)", "0.04\n(Nowhere close;\nGood = 1!)"), limits=c(-0.0025,0.045)) +
  ggtitle("Average Oak Stress 1-10 cm DBH") +
  theme_bw()
dev.off()

# ------------------------------------
