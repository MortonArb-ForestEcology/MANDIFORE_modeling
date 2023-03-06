
#12/21/22
#Part 2 of the Morton Arb output extraction code for the Learning Partnership people

#just running on local machine instead of server (output still being extracted and want to be able to do simultaneously)

#-----------------------------------------------------------------------------------------------------------------

#Code inputs: 
#monthly ED outputs from Morton arb runs, extracted in ExtractOutput_LP_Part1.R: 
  #site, patch, and cohort csv file for each GCM/mgmt/rcp scenario 
  #file naming format: "MortArb_All_ACCESS1-0_rcp45_statCO2_MgmtGap_Site_extract.csv"

#what this code does:
#takes the csv outputs from part ExtractOutput_LP_Part1.R where the actual extraction is done, and handles the 
#averaging and scaling from cohort to site level (NO weighted averaging)



#Code outputs:
#csv's for ED outputs at site, patch, cohort scale (file for each GCM/mgmt/rcp scenario)


#-----------------------------------------------------------------------------------------------------------------


#----------------
#install/load required packages
#----------------


library(lubridate)
library(stringr)


#----------------
#Hard-coded variables
#----------------

wd <- "D:/Learning_partnership"
setwd(wd)

run.list <- read.csv("RUNID_list.csv", header = FALSE)
run.list <- c(run.list$V1)

#path.out <- "/home/crollinson/MANDIFORE_modeling/MortonArb/1_runs/LearningPartnership_BAM/MortonArb_FINAL_EXTRACTED_LP.v1" 
path.out <- "D:/Learning_partnership/extract_final" # saving to external hard drive

#run.base <- "/home/crollinson/MANDIFORE_modeling/MortonArb/1_runs/MortonArb_ed_runs.v3" #this has the un-extracted model output files, used to pull RUNIDs
#dat.base <- "D:/Learning_partnership/extract_raw_test" #this has the extracted cohort, patch, and site csv's for each RUNID
dat.base <- "D:/Learning_partnership/MortonArb_RAW_EXTRACTED_LP.v2"
#dat.base <- "/home/crollinson/MANDIFORE_modeling/MortonArb/1_runs/LearningPartnership_BAM/MortonArb_RAW_EXTRACTED_LP.v2" 
#run.base <- "D:/Learning_partnership/raw_output_test" #this has the un-extracted model output files, used to pull RUNIDs

pfts.grass <- 5
pfts.trees <- 6:11

yr2sec <- 1/(365*24*60*60)
dpm <- lubridate::days_in_month(1:12)
sec2mo <- dpm*60*60*24

#----------------
#Load data 
#----------------

if(!dir.exists(path.out)) dir.create(path.out)

#run.list <- dir(run.base) #This gets a list of all the RUNIDs
#files.all <- dir(dat.base) #This gets a list of all the extracted files

setwd(dat.base)

pb <- txtProgressBar(min=0, max=length(run.list), style=3);
for(RUNID in run.list){
  setTxtProgressBar(pb, RUNID)
  co.df   <- read.csv(paste0(RUNID, "_Cohort_extract.csv"), header = TRUE) #cohort
  pch.df  <- read.csv(paste0(RUNID, "_Patch_extract.csv"), header = TRUE) #patch
  site.df <- read.csv(paste0(RUNID, "_Site_extract.csv"), header = TRUE) #site
#} #everything below will go within this for loop, so it loops through each RUNID at a time to process


#first go cohort --> patch scale:
co.df_tree <- subset(co.df, pft %in% pfts.trees) #subset to only tree pfts

#tiny seedlings skew averaging, replace small ones with NA
co.df_tree$dbh.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$dbh, NA) 
co.df_tree$ba.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$ba, NA)
co.df_tree$height.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$height, NA)
#co.df_tree$lai.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$lai, NA)
co.df_tree$agb.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$agb, NA)
co.df_tree$dens.pch.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$dens.pch, NA)
co.df_tree$crown.area.tree <- ifelse(co.df_tree$dbh>=12.7 & co.df_tree$height>= 1.37, co.df_tree$crown.area, NA)



#scaling cohort to patch (overall):
co.to.pch.overall_1 <- aggregate(cbind(dbh.tree, ba.tree, height.tree, agb.tree, dens.pch.tree, 
                             crown.area.tree) ~ year + month + patchID,
                       data=co.df_tree, FUN=mean, na.rm = TRUE) 
names(co.to.pch.overall_1) <- c("year", "month", "patchID", "dbh.tree.mean", "ba.tree.mean", "height.tree.mean", 
                                "agb.tree.mean", "dens.pch.tree.mean", "crown.area.tree.mean")

#Also calc SD for dbh, height, crown area
co.to.pch.overall_2 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                                 data=co.df_tree, FUN=sd, na.rm = TRUE) 
names(co.to.pch.overall_2) <- c("year", "month", "patchID", "dbh.tree.sd", "height.tree.sd", "crown.area.tree.sd")

#max for height, dbh, crown area
co.to.pch.overall_3 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                                 data=co.df_tree, FUN=max, na.rm = TRUE) 
names(co.to.pch.overall_3) <- c("year", "month", "patchID", "dbh.tree.max", "height.tree.max", "crown.area.tree.max")


#have to split into two parts if more than 2 things
co.to.pch.overall <- merge(co.to.pch.overall_1, co.to.pch.overall_2, 
                by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

co.to.pch.overall <- merge(co.to.pch.overall, co.to.pch.overall_3, 
                           by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))



#delete temporary df's
rm(co.to.pch.overall_1)
rm(co.to.pch.overall_2)
rm(co.to.pch.overall_3)

#scaling cohort to patch (by PFT):
#christy only used three unique pft's: 9, 10, 11

#subset for each of the 3 PFTs
co.df_tree_PFT9 <- subset(co.df_tree, pft == 9)
co.df_tree_PFT10 <- subset(co.df_tree, pft == 10)
co.df_tree_PFT11 <- subset(co.df_tree, pft == 11)


#start w/PFT 9
co.to.pch.PFT9_1 <- aggregate(cbind(dbh.tree, ba.tree, height.tree, agb.tree, dens.pch.tree, 
                                     crown.area.tree) ~ year + month + patchID,
                               data=co.df_tree_PFT9, FUN=mean, na.rm = TRUE) 
names(co.to.pch.PFT9_1) <- c("year", "month", "patchID", "dbh.tree.mean.PFT9", "ba.tree.mean.PFT9", 
                           "height.tree.mean.PFT9", "agb.tree.mean.PFT9", "dens.pch.tree.mean.PFT9", 
                           "crown.area.tree.mean.PFT9")

#Also calc SD for dbh, height, crown area
co.to.pch.PFT9_2 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                                 data=co.df_tree_PFT9, FUN=sd, na.rm = TRUE) 
names(co.to.pch.PFT9_2) <- c("year", "month", "patchID", "dbh.tree.sd.PFT9", "height.tree.sd.PFT9", 
                             "crown.area.tree.sd.PFT9")

#max for height, dbh, crown area
co.to.pch.PFT9_3 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                                 data=co.df_tree_PFT9, FUN=max, na.rm = TRUE) 
names(co.to.pch.PFT9_3) <- c("year", "month", "patchID", "dbh.tree.max.PFT9", "height.tree.max.PFT9", 
                             "crown.area.tree.max.PFT9")


co.to.pch.PFT9 <- merge(co.to.pch.PFT9_1, co.to.pch.PFT9_2,  
                           by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

co.to.pch.PFT9 <- merge(co.to.pch.PFT9, co.to.pch.PFT9_3, 
                        by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

#delete temporary df's
rm(co.to.pch.PFT9_1)
rm(co.to.pch.PFT9_2)
rm(co.to.pch.PFT9_3)



#PFT 10
co.to.pch.PFT10_1 <- aggregate(cbind(dbh.tree, ba.tree, height.tree, agb.tree, dens.pch.tree, 
                                    crown.area.tree) ~ year + month + patchID,
                              data=co.df_tree_PFT10, FUN=mean, na.rm = TRUE) 
names(co.to.pch.PFT10_1) <- c("year", "month", "patchID", "dbh.tree.mean.PFT10", "ba.tree.mean.PFT10", 
                             "height.tree.mean.PFT10", "agb.tree.mean.PFT10", "dens.pch.tree.mean.PFT10", 
                             "crown.area.tree.mean.PFT10")

#Also calc SD for dbh, height, crown area
co.to.pch.PFT10_2 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                              data=co.df_tree_PFT10, FUN=sd, na.rm = TRUE) 
names(co.to.pch.PFT10_2) <- c("year", "month", "patchID", "dbh.tree.sd.PFT10", "height.tree.sd.PFT10", 
                             "crown.area.tree.sd.PFT10")

#max for height, dbh, crown area
co.to.pch.PFT10_3 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                              data=co.df_tree_PFT10, FUN=max, na.rm = TRUE) 
names(co.to.pch.PFT10_3) <- c("year", "month", "patchID", "dbh.tree.max.PFT10", "height.tree.max.PFT10", 
                             "crown.area.tree.max.PFT10")


co.to.pch.PFT10 <- merge(co.to.pch.PFT10_1, co.to.pch.PFT10_2,  
                        by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

co.to.pch.PFT10 <- merge(co.to.pch.PFT10, co.to.pch.PFT10_3, 
                         by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

#delete temporary df's
rm(co.to.pch.PFT10_1)
rm(co.to.pch.PFT10_2)
rm(co.to.pch.PFT10_3)



#PFT 11
co.to.pch.PFT11_1 <- aggregate(cbind(dbh.tree, ba.tree, height.tree, agb.tree, dens.pch.tree, 
                                     crown.area.tree) ~ year + month + patchID,
                               data=co.df_tree_PFT11, FUN=mean, na.rm = TRUE) 
names(co.to.pch.PFT11_1) <- c("year", "month", "patchID", "dbh.tree.mean.PFT11", "ba.tree.mean.PFT11", 
                              "height.tree.mean.PFT11", "agb.tree.mean.PFT11", "dens.pch.tree.mean.PFT11", 
                              "crown.area.tree.mean.PFT11")

#Also calc SD for dbh, height, crown area
co.to.pch.PFT11_2 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                               data=co.df_tree_PFT11, FUN=sd, na.rm = TRUE) 
names(co.to.pch.PFT11_2) <- c("year", "month", "patchID", "dbh.tree.sd.PFT11", "height.tree.sd.PFT11", 
                              "crown.area.tree.sd.PFT11")

#max for height, dbh, crown area
co.to.pch.PFT11_3 <- aggregate(cbind(dbh.tree, height.tree, crown.area.tree) ~ year + month + patchID, 
                               data=co.df_tree_PFT11, FUN=max, na.rm = TRUE) 
names(co.to.pch.PFT11_3) <- c("year", "month", "patchID", "dbh.tree.max.PFT11", "height.tree.max.PFT11", 
                              "crown.area.tree.max.PFT11")


co.to.pch.PFT11 <- merge(co.to.pch.PFT11_1, co.to.pch.PFT11_2, 
                         by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

co.to.pch.PFT11 <- merge(co.to.pch.PFT11, co.to.pch.PFT11_3, 
                         by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

#delete temporary df's
rm(co.to.pch.PFT11_1)
rm(co.to.pch.PFT11_2)
rm(co.to.pch.PFT11_3)


#now combine the up-scaled cohort data with the existing pch dataframe
pch.df <- merge(pch.df, co.to.pch.overall, 
                by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

pch.df <- merge(pch.df, co.to.pch.PFT9, 
                by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

pch.df <- merge(pch.df, co.to.pch.PFT10, 
                by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

pch.df <- merge(pch.df, co.to.pch.PFT11,
                by.x=c("year", "month", "patchID"), by.y=c("year", "month", "patchID"))

#remove all the intermediate df's
rm(co.df_tree)
rm(co.to.pch.overall)
rm(co.to.pch.PFT9)
rm(co.to.pch.PFT10)
rm(co.to.pch.PFT11)




#Now go patch --> site scale:
#avg across all patches for a given year/month combo

pch.to.site.1 <- aggregate(cbind(dbh.tree.mean, height.tree.mean, agb.tree.mean, 
                                         crown.area.tree.mean, dbh.tree.sd, height.tree.sd, crown.area.tree.sd, 
                                         dbh.tree.max, height.tree.max, crown.area.tree.max, age, 
                                 dbh.tree.mean.PFT9, height.tree.mean.PFT9, agb.tree.mean.PFT9, 
                                 crown.area.tree.mean.PFT9, dbh.tree.sd.PFT9, height.tree.sd.PFT9, crown.area.tree.sd.PFT9, 
                                 dbh.tree.max.PFT9, height.tree.max.PFT9, crown.area.tree.max.PFT9,
                                 dbh.tree.mean.PFT10, height.tree.mean.PFT10, agb.tree.mean.PFT10, 
                                 crown.area.tree.mean.PFT10, dbh.tree.sd.PFT10, height.tree.sd.PFT10, crown.area.tree.sd.PFT10, 
                                 dbh.tree.max.PFT10, height.tree.max.PFT10, crown.area.tree.max.PFT10,
                                 dbh.tree.mean.PFT11, height.tree.mean.PFT11, agb.tree.mean.PFT11, 
                                 crown.area.tree.mean.PFT11, dbh.tree.sd.PFT11, height.tree.sd.PFT11, crown.area.tree.sd.PFT11, 
                                 dbh.tree.max.PFT11, height.tree.max.PFT11, crown.area.tree.max.PFT11) ~ year + month,
                                 data=pch.df, FUN=mean, na.rm = TRUE)



names(pch.to.site.1) <- c("year", "month", "dbh.tree.mean", "height.tree.mean", "agb.tree.mean", 
                                  "crown.area.tree.mean", "dbh.tree.sd", "height.tree.sd", "crown.area.tree.sd", 
                                  "dbh.tree.max", "height.tree.max", "crown.area.tree.max", "age.mean", 
                          "dbh.tree.mean.PFT9", "height.tree.mean.PFT9", "agb.tree.mean.PFT9", 
                          "crown.area.tree.mean.PFT9", "dbh.tree.sd.PFT9", "height.tree.sd.PFT9", "crown.area.tree.sd.PFT9", 
                          "dbh.tree.max.PFT9", "height.tree.max.PFT9", "crown.area.tree.max.PFT9",
                          "dbh.tree.mean.PFT10", "height.tree.mean.PFT10", "agb.tree.mean.PFT10", 
                          "crown.area.tree.mean.PFT10", "dbh.tree.sd.PFT10", "height.tree.sd.PFT10", "crown.area.tree.sd.PFT10", 
                          "dbh.tree.max.PFT10", "height.tree.max.PFT10", "crown.area.tree.max.PFT10",
                          "dbh.tree.mean.PFT11", "height.tree.mean.PFT11", "agb.tree.mean.PFT11", 
                          "crown.area.tree.mean.PFT11", "dbh.tree.sd.PFT11", "height.tree.sd.PFT11", "crown.area.tree.sd.PFT11", 
                          "dbh.tree.max.PFT11", "height.tree.max.PFT11", "crown.area.tree.max.PFT11")

#sum (across all patches) for density and basal area
pch.to.site.2 <- aggregate(cbind(ba.tree.mean, dens.pch.tree.mean, ba.tree.mean.PFT9, 
                                 dens.pch.tree.mean.PFT9, ba.tree.mean.PFT10, dens.pch.tree.mean.PFT10, 
                                 ba.tree.mean.PFT11, dens.pch.tree.mean.PFT11) ~ year + month,
                                   data=pch.df, FUN=sum, na.rm = TRUE)
names(pch.to.site.2) <- c("year", "month", "ba.tree.mean", "dens.pch.tree.mean", "ba.tree.mean.PFT9", 
                          "dens.pch.tree.mean.PFT9", "ba.tree.mean.PFT10", "dens.pch.tree.mean.PFT10", 
                          "ba.tree.mean.PFT11", "dens.pch.tree.mean.PFT11")

#merge the two
pch.to.site <- merge(pch.to.site.1, pch.to.site.2, 
                           by.x=c("year", "month"), by.y=c("year", "month"))
#remove temporary df's
rm(pch.to.site.1)
rm(pch.to.site.2)

#just doing overall, can't get for PFT specific, but have PFT specific density values so that should be enough
total.cohorts <- pch.df %>%
  group_by(year, month) %>% 
  summarise(total.cohorts = sum(n.cohorts, na.rm = TRUE)) 



site.pch.merge <- merge(pch.to.site, total.cohorts, by.x=c("year", "month"), by.y=c("year", "month"))

rm(pch.to.site)
rm(total.cohorts)

#now combine the upscaled patch data with the existing site dataframe
site.df <- merge(site.df, site.pch.merge, by.x=c("year", "month"), by.y=c("year", "month"))

rm(site.pch.merge)




# Save locally (smallest file to biggest file)
write.csv(site.df, file.path(path.out, paste(RUNID, "Site.csv", sep="_")), row.names=F)
write.csv(pch.df, file.path(path.out, paste(RUNID, "Patch.csv", sep="_")), row.names=F)
write.csv(co.df, file.path(path.out, paste(RUNID, "Cohort.csv", sep="_")), row.names=F)

rm(site.df, co.df, pch.df)
} # End looping through runID's




#LP team only wants the .site csv's, so select all of these
setwd(path.out)
filenames <- list.files(pattern = "\\_Site.csv$")

file.copy(from = paste0(path.out,"/", filenames),
          to = paste0(path.out, "/Site_only/", filenames))





#LP adds------------------------------------------------------------------------------------------------------------------
  #subset to just tree PFTs
  #filter by dbh/height for just what counts as 'trees' to screen out tiny stems
  
  #scaling cohort to patch (overall): group basal area, above ground biomass, dbh, density, lai, and tree height by year, 
    #month, patchID, find average for all. Also calc SD for dbh and height, and max for height & dbh (same grouping)
  
  #scaling cohort to patch (by PFT): group basal area, above ground biomass, dbh, density, lai, and tree height by year, 
    #month, patchID, and PFT, find average for all. Also calc SD for dbh and height, and max for height & dbh (same grouping)
    #think would want these added as additional columns so there are still consistent number of rows
      #so maybe subset by each PFT and then calc instead of grouping
  

  
  #scaling patch to site (overall): group mean basal area, mean above ground biomass, mean dbh, SD dbh, max dbh, 
    #mean density, mean lai, mean tree height, SD tree height, max tree height, age, by year, 
    #month, sum (across all patches) for density and basal area, avg (across all patches) for other vars

  #scaling patch to site (by PFT): group mean basal area, mean above ground biomass, mean dbh, SD dbh, max dbh, 
    #mean density, mean lai, mean tree height, SD tree height, max tree height, age, by year, 
    #month, and PFT





#--------------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------------------------


