
#TO FIX EVENTUALLY:
# 1) make matching better by making ED PFT species list more complete, across all 10 sites ~600 species existed in the 
  #NEON data that aren't linked to an ED PFT (through the betydb associations)

# 2) eventually set everything up to loop through site list instead of doing individually

# 3) make working directory paths more universal, simplify/clean up code

# 4) add functionality to look up taxonID's in FIA master tree species list, and add info to PFT_matches for 
  #taxonID's that are in the NEON data but not currently in the PFT_matches.csv list...this list was made from 
  #species entered in bety associated with default ED pfts so its not perfect
  #part of issue is sometimes NEON data has only genus no species info, so taxonID is just general genus one, 
    #while the FIA/USDA code list just has codes for specieis genus/species combos
    #also some issues w/things that would normally be considered shrubs getting recorded as small trees in the NEON data
    #ex: huckleberry, sumac...when really big they're kind of on the border

#----------------------------------------------------------------------------------------------------------------

#6/29/22

#Bailey Murphy
#extract NEON veg data, output site/css/pss files required by ED


#--------------------------------------------------------------------------------------------------------------------

#----------------
#install/load required packages
#----------------

library(neonstore)
library(neonUtilities)
library(dplyr)
library(lubridate)
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------

site_list <- c('TALL', 'OSBS', 'UNDE', 'HARV', 'SCBI', 'WREF', 'YELL', 'NIWO', 'ORNL', 'SJER') 
site_name <- 'SJER' #manually change here, eventually loop through site list instead (lazyyy)

#ONLY HAVE TO DOWNLOAD AND LOAD THE DATA ONCE!!!!

options(stringsAsFactors = FALSE)
options(scipen=999) # Turn off scientific notation

#load soils data 
soils_data <- read.csv("NEON_ED_soil_data.csv", header = TRUE)
#load basic site data
site_data <- read.csv("NEON_Field_Site_FOREST_CORE.csv", header = TRUE)

#load ED PFT matching data
PFT_matches <- read.csv("ED_PFT_matches.csv", header = TRUE)
PFT_matches <- data.frame(PFT_matches$PLANTS_Code, PFT_matches$ED_PFT) #keep only col's that are needed for matching
names(PFT_matches) <- c("taxonID", "ED_PFT")


#----------------
#Download data from NEON
#----------------



#quality-controlled, native sampling resolution data from in-situ measurements of live and standing dead woody 
  #individuals, shrub groups, and non-herbaceous perennial plants from all terrestrial NEON sites with qualifying 
  #woody vegetation. Structure and mapping data are reported per individual per plot. 
#vst_mappingandtagging table contains at least one record per individualID, and provides data that are invariant through 
  #time, including tagID, taxonID, and mapped location (if applicable)
#vst_apparentindividual table contains one record per individualID per eventID, and includes growth form, structure and 
  #plant status data that may be linked to vst_mappingandtagging records via the individualID; records may also be 
  #linked to vst_perplotperyear via the plotID and eventID fields in order to generate plot-level estimates of biomass 
  #and productivity

#will take a couple minutes to download, getting all sites/available years at once
veg_raw <- neonUtilities::loadByProduct(dpID = "DP1.10098.001", site = site_list, check.size = FALSE, package = "basic")


names(veg_raw)
#dataframes included in veg_raw:
# [1] "categoricalCodes_10098" "issueLog_10098"         "readme_10098"           "validation_10098"      
# [5] "variables_10098"        "vst_apparentindividual" "vst_mappingandtagging"  "vst_non-woody"         
# [9] "vst_perplotperyear"     "vst_shrubgroup"

#looks like vst_perplotperyear can be used to get stem density per area estimates
  #totalSampledAreaTrees = Total plot area sampled for single and multi-bole trees
  #totalSampledAreaShrubSapling = Total plot area sampled for saplings, shrubs, and small trees
  #also has elevation
  #area is by plot (m^2), so will have to sum across all plots listed at a site

#vst_apparentindividual has stem diameter, height, growthForm (small tree, sapling, shrub, liana, single bole tree, etc.), 
  #plantStatus (live, dead, downed, live but insect damaged, lost due to burn, etc.)
  #use growthForm to filter out shrubs, lianas, etc
  #use plantStatus to filter out dead trees

#vst_mappingandtagging has taxonID, scientificName, and taxonRank for individual trees

#save variable names/description info to csv
write.csv(veg_raw$variables_10098, "NEON_veg_structure_variables.csv", row.names = FALSE)

# make separate objects for each datatable of interest
apparentindividual <- veg_raw$vst_apparentindividual
mappingandtagging <- veg_raw$vst_mappingandtagging


#Select only the data columns needed from each datatable------------------------


apparentindividual <- apparentindividual[, c(
  "domainID",
  "siteID",
  "plotID",
  "date",
  "individualID", 
  "growthForm", 
  "plantStatus", 
  "stemDiameter", 
  "height"
)] 


mappingandtagging <- mappingandtagging[, c(
  "domainID",
  "siteID",
  "plotID",
  "subplotID",
  "individualID",
  "taxonID", 
  "scientificName",
  "taxonRank"
)] 

#check dims
dim(apparentindividual)
dim(mappingandtagging)

#save dataframes in case needed later
write.csv(apparentindividual, "NEON_veg_apparentindividual.csv", row.names = FALSE)
write.csv(mappingandtagging, "NEON_veg_mappingandtagging.csv", row.names = FALSE)




#Subset everything to just the site of interest------------------------------------

#eventually loop through site_list

site_data_sub <- subset(site_data, site_data$field_site_id == site_name)
soils_data_sub <- subset(soils_data, soils_data$siteID == site_name)
apparentindividual_sub <- subset(apparentindividual, apparentindividual$siteID == site_name)
mappingandtagging_sub <- subset(mappingandtagging, mappingandtagging$siteID == site_name)

#Set up file prefixes--------------------------------------------------------------


site_lat <- site_data_sub$field_latitude
site_lat <- round(as.numeric(site_lat[1]), 2)
site_lon <- site_data_sub$field_longitude
site_lon <- round(as.numeric(site_lon[1]), 2)

file_prefix <- paste0("NEON_", site_name, '.lat', site_lat, 'lon', site_lon)



#Now start making the init files (.site, .css, .pss)--------------------------------------------------------------


#----------------
#Make .site files
#----------------


nsite <- 1 
format <- 1 #file format
sitenum <- 1 #eventually n_site <- 1:length(site_list), then index through
area <- format(round(1.0, 2), nsmall = 1)
TCI <- -7 #just using the ED default values
elev <- site_data_sub$field_mean_elevation_m
slope <- format(round(0.0, 2), nsmall = 1)
aspect <- format(round(0.0, 2), nsmall = 1)
soil <- soils_data_sub$ED_soil_class

site_file <- data.frame(nsite, format, sitenum, area, TCI, elev, slope, aspect, soil)

#want variables output on specific lines in the .site file, so need to write by line
line_1 <- paste("nsite", nsite, "format", format)
line_2 <- paste("sitenum", "area", "TCI", "elev", "slope", "aspect", "soil")
line_3 <- paste(sitenum, area, TCI, elev, slope, aspect, soil)
lines <- c(line_1, line_2, line_3)

file_name <- paste0(file_prefix,".site")

path <- "~/University_of_Wisconsin/PhD/Chapter_3/ED/MANDIFORE_modeling/NEON_runs/init_files/"
setwd(path)

for(line in lines){
  write(line, file = file_name,
        append = TRUE, sep = "\n")
}




#----------------
#Make .css files
#----------------

#Variables needed (in this order): "time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai"

#join apparentindividual and mappingandtagging together by individualID
  #using inner_join includes all rows in x AND y, because we only want rows where we have both DBH/height data and 
  #species info can still have multiple entries for an individualID from apparentindividual_sub, if same 
  #tree measured in diff years
joined_veg <- dplyr::inner_join(mappingandtagging_sub, apparentindividual_sub, by = "individualID")

#remove duplicate cols
joined_veg$domainID.y <- NULL
joined_veg$siteID.y <- NULL
joined_veg$plotID.y <- NULL

#add col for year
joined_veg <- joined_veg %>% 
  mutate(year = year(date))

#vst_apparentindividual has stem diameter, height, growthForm (small tree, sapling, shrub, liana, single bole tree, etc.), 
#plantStatus (live, dead, downed, live but insect damaged, lost due to burn, etc.)
#use growthForm to filter out shrubs, lianas, etc
#use plantStatus to filter out dead trees

#unique(joined_veg$growthForm)
#unique(joined_veg$plantStatus)

#filter out shrubs, lianas, and NA's
growthForm_keep <- c("sapling", "small tree", "single bole tree", "multi-bole tree")
joined_veg <- subset(joined_veg, growthForm %in% growthForm_keep)

#filter out dead trees and NA's
plantStatus_keep <- c("Live", "Live, physically damaged", "Live, insect damaged", "Live, disease damaged", 
                      "Live, broken bole", "Live,  other damage")
joined_veg <- subset(joined_veg, plantStatus %in% plantStatus_keep)


#Get rid of any entries where stemDiameter is NA (originally screened for NA height too but don't actually need to)
joined_veg <- joined_veg[!is.na(joined_veg$stemDiameter),] #joined_veg[!is.na(joined_veg$stemDiameter) & !is.na(joined_veg$height),]

#make sure no NA's exist in taxon info
sum(is.na(joined_veg$taxonID))



#Now need to do species matching to PFTs, without connecting to betydb
#taxonID values match the USDA standardized plant species codes
#before joining, see if any taxonID's are in joined_veg that aren't recorded in PFT_matches
  #look up taxonID's in FIA master tree species list, and add info to PFT_matches 
missed_taxonID <- joined_veg$taxonID[!joined_veg$taxonID %in% PFT_matches$taxonID]
missed_taxonID <- unique(missed_taxonID)
print("Add missing taxonID's to PFT_matches csv: ")
print(missed_taxonID)

joined_veg <- merge(joined_veg, PFT_matches, by = "taxonID") 


#Next set up the patch stuff
  #divide data into patches based on DBH bins
  #patches are groups of trees that have a shared age since last disturb, don't have age data, so just breaking 
  #it up by DBH instead. Christy used 2cm DBH bins, going w/this
temp_df <- data.frame(joined_veg$stemDiameter, joined_veg$plotID.x)
names(temp_df) <- c('dbh', 'plotID')

#breaks up every 2 units, assigns patch number based on dbh bins
temp_df <- temp_df %>% 
  mutate(patch = cut_interval(dbh, length = 2, labels = FALSE))

#need to adjust the patch numbers, if no trees exist for a bin it won't have anything for that label but 
#it'll still increase to the next patch number, basically numbers get skipped
temp_df <- temp_df %>%
  mutate(patch_corrected = dense_rank(patch)) #dense_rank() eliminates gaps between ranks, allows for duplicates of the same rank, and doesn't change order of rows

#check that the above occurred correctly, below two numbers should be equal
length(unique(temp_df$patch))
length(unique(temp_df$patch_corrected))


#now find 'n', density of stems per m^2 within a patch
temp_df <- temp_df %>%
  group_by(patch_corrected) %>%
  mutate(trees_per_patch = length(dbh)) %>%
  mutate(plots_per_patch = length(unique(plotID))) %>%
  mutate(n = trees_per_patch / (plots_per_patch*1600)) #multiplying by 1600 b/c NEON plots are 40x40m



#start building the css df, adding PFT stuff early b/c need it for figuring out cohort
time <- joined_veg$year
css <- data.frame(time, temp_df$patch_corrected, joined_veg$ED_PFT)

#add cohort stuff
  #below creates cohort col where different numerical ID is given to each unique combination of patch and PFT
css$cohort <-as.numeric(as.factor(with(css,paste(temp_df.patch_corrected, joined_veg.ED_PFT, sep="_"))))

#remove pft col, will add again but want in different order
css$joined_veg.ED_PFT <- NULL

#add in other cols
css$dbh <- temp_df$dbh
css$hite <- 0 #deprecated, set = 0
css$pft <- joined_veg$ED_PFT
css$n <- temp_df$n

#all these variables are deprecated, set = 0
css[,c("bdead", "balive", "lai")] <- 0



names(css) <- c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")

write.table(css, paste0(file_prefix,".css"), quote = FALSE, row.names = FALSE)



#----------------
#Make .pss files
#----------------


#need to be in this order: time patch trk age area water fsc stsc stsl ssc psc msn fsn

#time is deprecated here, just setting as earliest data year 
time_pss <- min(css$time)

patch <- unique(css$patch)

pss <- data.frame(time_pss, patch)

pss[,'trk'] <- 1 #disturbance type, 1 = "forest plantation", other runs Christy did used this value

#don't have tree age data from NEON, can estimate from DBH and growth factor (species specific)
  #approx age = DBH (inches) * growth factor
  #method from  International Society of Arboriculture (ISA)
dbh_in <- css$dbh / 2.54 #to get from cm to in
age_estim <- data.frame(css$patch, css$cohort, dbh_in, css$pft)

#instead of going into species specific, set up rough growth factors based on pft
  #selected a few representative species from each pft, looked up growth factors, avg'd per pft
  #used this for growth factors: https://goodcalculators.com/tree-age-calculator/#Tree%20Species%20&%20Growth%20Factors 
age_estim <- age_estim %>%
  mutate(growth_factor = case_when(css.pft == 6 ~ 4.7,
                             css.pft == 7 ~ 5,
                             css.pft == 8 ~ 5.5,
                             css.pft == 9 ~ 3.5,
                             css.pft == 10 ~ 4.8,
                             css.pft == 11 ~ 4.4))

age_estim <- age_estim %>%
  mutate(avg_age = dbh_in * growth_factor)


#next avg for each patch
age_estim <- age_estim %>%
  group_by(css.patch) %>%
  mutate(avg_patch_age = mean(avg_age))


age_estim <- data.frame(age_estim$css.patch, age_estim$avg_patch_age)

names(age_estim) <- c("patch", "avg_patch_age")
age_estim$avg_patch_age <- round(age_estim$avg_patch_age, 2)

pss <- merge(pss, age_estim, by = "patch")

#remove duplicate rows (must be same across all cols)
pss <- pss %>%
  distinct()

pss$area <- 1/length(patch)

pss$water <- 0

#now pull in the soil data, rounding so calc go faster
pss$fsc <- round(soils_data_sub$fsc,5)
pss$stsc <- round(soils_data_sub$stsc, 5)
pss$stsl <- round(soils_data_sub$stsl, 5)
pss$ssc <- round(soils_data_sub$ssc, 5)
pss$psc <- round(soils_data_sub$psc, 5)
pss$msn <- round(soils_data_sub$msn, 5)
pss$fsn <- round(soils_data_sub$fsn, 5)

#fix order of cols & names
pss <- data.frame(pss$time_pss, pss$patch, pss$trk, pss$avg_patch_age, pss$area, pss$water, pss$fsc, 
                  pss$stsc, pss$stsl, pss$ssc, pss$psc, pss$msn, pss$fsn)

names(pss) <- c('time', 'patch', 'trk', 'age', 'area', 'water', 'fsc', 'stsc', 'stsl', 'ssc', 'psc', 'msn', 'fsn')


write.table(pss, paste0(file_prefix,".pss"), quote = FALSE, row.names = FALSE)


#-----------------------------------------------------------------------------------------------------------------------

































