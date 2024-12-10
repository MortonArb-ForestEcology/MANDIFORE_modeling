
#TO FIX EVENTUALLY:

# 1) no option for peat (ED class #12), bedrock (#13), clayey sand (#16), or clayey silt (#17), FIX LATER 

# 2) createTexturedata() function in 'soilassessment' package can sometimes return double classes such 
  #as "SaLo, Lo" implying possibility of a tie for two classes, figure out something later to deal w/this


#-------------------------------------------------------------------------------------------------------------------

#----------------
#Code information
#----------------


#Author: Bailey Murphy, University of Wisconsin-Madison, bamurphy5@wisc.edu
#Created: 6/22/22 

#WHAT THIS CODE DOES: pulls in NEON soils data and formats/calculates variables 
  #that are used in ED site, patch, and ED2IN files
  #this code would be ran prior to building ED2IN and site/pss/css files


#FILES GENERATED:
  # 1) NEON_soil_variables.csv: variable names, descriptions, units, data type, etc. for NEON soil physical/chemical datasets
  
  # 2) NEON_soil_microbe_variables.csv: variable names, descriptions, units, data type, etc. for NEON soil microbial datasets

  # 3) NEON_ED_soil_layers.csv: describes number of soil layers, layer thickness, and bottom depth of each layer
    #for use in ED2INs

  # 4) NEON_ED_soil_data.csv: describes soil variables for use in ED2INs and ED site/pss files



#Core forested NEON sites (10 total, AK dropped) for MANDIFORE modeling: 
# TALL
# OSBS
# UNDE
# HARV
# SCBI
# WREF
# YELL
# NIWO
# ORNL
# SJER


#NOTES:
#NEON datasets all have unique data product ID's associated w/them, these are used to download the datasets of interest
  #DPI codes for required datasets:
  #NEON soil physical and chemical properties (megapit): DP1.00096.001
  #NEON Soil microbial biomass: DP1.10104.001

#NEON soil microbial biomass is calculated using high-throughput Phospholipid Fatty Acids (PLFA) 
  #Determining biomass using PLFA means you're looking at phospholipid fatty acids, which are a key component of the 
  #cell walls of all soil microorganisms, so by determining fatty acid biomass you're effectively determining soil 
  #microbe biomass. These fatty acids in the cell walls of microbes break down super fast once a microbe dies, so 
  #you're essentially only measuring the live microbial biomass w/PLFA




#--------------------------------------------------------------------------------------------------------------------


#----------------
#Hardcoded variables
#----------------

#change sites of interest here!!
#character vector of 4-letter NEON site codes
site_list <- c('TALL', 'OSBS', 'UNDE', 'HARV', 'SCBI', 'WREF', 'YELL', 'NIWO', 'ORNL', 'SJER') 


#----------------
#install/load required packages
#----------------

library(dplyr)
install.packages('soilassessment') #for soil texture classification
library(soilassessment)
library(data)
library(purrr)

## Install NEON libraries
install.packages("devtools")
library(devtools)
devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
devtools::install_github("NEONScience/NEON-utilities/neonUtilities", force = TRUE)
install.packages("BiocManager")
BiocManager::install("rhdf5") #this might not actually get used...check



#---------------------------------------------------------------------------------------------------------------------

#----------------
#Download data from NEON
#----------------

options(stringsAsFactors = FALSE)
options(scipen=999) # Turn off scientific notation


soil_raw <- neonUtilities::loadByProduct(dpID = "DP1.00096.001", site = site_list, check.size = FALSE) #soil physical and chemical properties (megapit)
soil_raw_microbe <- neonUtilities::loadByProduct(dpID = "DP1.10104.001", site = site_list, check.size = FALSE) #Soil microbial biomass



#--------------------------------------------------------------------------------------------------------------

#----------------
#Pull in variables of interest and clean up the data
#----------------


#NEON soil datasets are stacked, have 5 datatables, 3 metdadata tables, and an issue log
# these objects have 'slots'. Access a slot with the $, then also use $ to refer to columns
#example: names(soil_raw) displays the name of each table stacked in the dataset, 
  #soil_raw$mgp_perbulksample would pull the datatable with data related to bulk density

#first save the 'variables' datatables as a csv so you have it for later reference
  #these tables include information on variable names, a brief description, units, datatype, etc.
write.csv(soil_raw$variables_00096, "NEON_soil_variables.csv", row.names = FALSE)
write.csv(soil_raw_microbe$variables_10104, "NEON_soil_microbe_variables.csv", row.names = FALSE)

# make separate objects for each datatable
soil_chem_red <- soil_raw$mgp_perbiogeosample
soil_phys_red <- soil_raw$mgp_perbulksample
soil_microbe_red <- soil_raw_microbe$sme_microbialBiomass
soil_layer_depth <- soil_raw$mgp_perhorizon


#Select only the data columns we need from each datatable------------------------

#units info (see the variables csv's you exported for more info):
#sand/silt/clay info are all %
#C and N total, estimated organic C, are grams per kg of soil
#depths are cm
soil_chem_red <- soil_chem_red[, c(
  "domainID",
  "siteID",
  "pitID",
  "horizonID",
  "collectDate", 
  "horizonName",
  "sandTotal",
  "siltTotal", 
  "clayTotal", 
  "carbonTot",
  "estimatedOC", 
  "nitrogenTot",
  "biogeoTopDepth", 
  "biogeoBottomDepth"
)] 

#Units info:
#bulkDensExclCoarseFrag is bulk density excluding coarse rock fragments >2mm, units are g/cm^3
soil_phys_red <- soil_phys_red[, c(
  "domainID",
  "siteID",
  "pitID",
  "horizonID",
  "collectDate", 
  "horizonName",
  "bulkDensID",
  "bulkDensExclCoarseFrag", 
  "bulkDensTopDepth", 
  "bulkDensBottomDepth"
)] 



#units info:
#"totalLipidConcentration" is the sum of all the biomasses from different microbial groups, units are 
#nanomoles per g of soil, will need microbial biomass in terms of mass per mass of soil for ED

soil_microbe_red <- soil_microbe_red[, c(
  "domainID",
  "siteID",
  "plotID",
  "collectDate", 
  "biomassID",
  "totalLipidConcentration"
)] 

#depths are cm
soil_layer_depth <- soil_layer_depth[, c(
  "domainID",
  "siteID",
  "pitID",
  "horizonID",
  "collectDate", 
  "horizonName",
  "horizonTopDepth", 
  "horizonBottomDepth"
)]

#data is all in different dimensions with multiple values for variables
#we want a single value for each variable of interest per site
#this means for variables with values for each soil horizon at each site, need to do a weighted avg across 
#all soil horizons to calc single value of variables of interest
#for values with multiple replicates for the same site, need to average these (ex: microbial biomass)

dim(soil_chem_red)
dim(soil_phys_red)
dim(soil_microbe_red)
dim(soil_layer_depth)



#First handle the soil layer info needed for the ED2INs---------------------------------------------

#calculate thickness of each soil layer (cm)
soil_layer_depth$layer_thickness <- soil_layer_depth$horizonBottomDepth - soil_layer_depth$horizonTopDepth
#get rid of column 'horizonTopDepth', no longer need
soil_layer_depth$horizonTopDepth <- NULL

  
#calculate total number of soil layers at each site (n_layers)
number_layers <- with(soil_layer_depth, aggregate(horizonName ~ siteID, FUN=function(x){length(unique(x))}))
names(number_layers) <- c("siteID", "n_layers")

#add n_layers col to main dataframe
soil_layer_depth$n_layers <- number_layers$n_layers[match(soil_layer_depth$siteID, number_layers$siteID)]


#convert all the depths from cm to m (ED standard)
#convert layer depth to negative values for ED (#ex: deepest layer at -1.0, next deepest layer at -0.8, etc.)
soil_layer_depth$horizonBottomDepth <- (soil_layer_depth$horizonBottomDepth/100) * -1 
soil_layer_depth$layer_thickness <- soil_layer_depth$layer_thickness/100


#put horizon rows in order of deepest to shallowest for each site
soil_layer_depth <- soil_layer_depth[order(soil_layer_depth$siteID, soil_layer_depth$horizonBottomDepth),]

#export as csv
write.csv(soil_layer_depth, "NEON_ED_soil_layers.csv", row.names = FALSE)




#Next handle the soil chemical and physical variables-------------------------------------------------------------


#Need to do a weighted avg across all soil horizons to calc single value of variables of interest for each site

#First add variables for soil layer depth
soil_chem_red$layer_thickness <- soil_chem_red$biogeoBottomDepth - soil_chem_red$biogeoTopDepth
soil_phys_red$layer_thickness <- soil_phys_red$bulkDensBottomDepth - soil_phys_red$bulkDensTopDepth

#for soil_phys_red, doing weighted avg by layer thickness to end up w/single 'bulkDensExclCoarseFrag' value for each unique siteID
#for soil_chem_red, doing weighted avg by layer thickness to end up w/single 'sandTotal', 'siltTotal', 'clayTotal', 
#''carbonTot', 'estimatedOC', and 'nitrogenTot' values for each unique siteID

#Weighted mean by site, soil_phys_red
soil_phys_red_W <- soil_phys_red %>%                                           
  group_by(siteID) %>% 
  summarise(bulkDens = weighted.mean(bulkDensExclCoarseFrag, layer_thickness, na.rm = TRUE))



#Weighted mean by site, soil_chem_red
soil_chem_red_W1 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(sandTotal = weighted.mean(sandTotal, layer_thickness, na.rm = TRUE)) 

soil_chem_red_W2 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(siltTotal = weighted.mean(siltTotal, layer_thickness, na.rm = TRUE)) 

soil_chem_red_W3 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(clayTotal = weighted.mean(clayTotal, layer_thickness, na.rm = TRUE)) 

soil_chem_red_W4 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(carbonTot = weighted.mean(carbonTot, layer_thickness, na.rm = TRUE))

soil_chem_red_W5 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(estimatedOC = weighted.mean(estimatedOC, layer_thickness, na.rm = TRUE))

soil_chem_red_W6 <- soil_chem_red %>%                                           
  group_by(siteID) %>% 
  summarise(nitrogenTot = weighted.mean(nitrogenTot, layer_thickness, na.rm = TRUE))

#set up list of all the df's you just created
df_list <- list(soil_phys_red_W, soil_chem_red_W1, soil_chem_red_W2, soil_chem_red_W3,
                soil_chem_red_W4, soil_chem_red_W5, soil_chem_red_W6)

#combine all the weighted average values into one dataset, deleting duplicate columns
soil_data <- df_list %>% 
  reduce(full_join, by='siteID')






#fix all the units for soil chemical/physical variables
#current units -> units needed for ED
#bulkDens: g/cm^3 -> kg/m^2
#sandTotal, siltTotal, clayTotal: % -> fraction (divide current value by 100) 
#carbonTot: g/kg -> kg/m^2
#estimatedOC: g/kg -> kg/m^2
#nitrogenTot: g/kg -> kg/m^2

#save a copy before messing w/units just in case #DELETE THIS LATER
soil_data_original <- soil_data

#bulk density is used to convert variables from a mass per unit mass 
  #(ex: kg C/kg soil) to a mass per unit volume (ex: kg C/m^3 soil)
  #BUT the soil variables in ED actually need to be expressed as an area density (aka mass per unit area)
    #which is not how these variables are usually expressed...this means you need to convert bulk density from
    #a density to an area density (area density = density * thickness of object)
    #assuming a layer thickness of 0.01m, so soil volume is sliced into thin layers 
    #(this results in values with correct magnitudes, in range of defaults)
soil_data$bulkDens <- (soil_data$bulkDens * 1000) * 0.01 #now bulk density is converted from g/cm^3 to kg/m^2


soil_data$sandTotal_frac <- soil_data$sandTotal/100
soil_data$siltTotal_frac <- soil_data$siltTotal/100
soil_data$clayTotal_frac <- soil_data$clayTotal/100

#divide by 1000 to go g carbon -> kg carbon, multiply by bulk density to get kg C/m^2 soil
soil_data$carbonTot <- (soil_data$carbonTot/1000) * soil_data$bulkDens
soil_data$estimatedOC <- (soil_data$estimatedOC/1000) * soil_data$bulkDens
soil_data$nitrogenTot <- (soil_data$nitrogenTot/1000) * soil_data$bulkDens





#Next microbial stuff-------------------------------------------------------------------------------------

# >200 samples processed for each site
#see how many years there's data for
unique(year(soil_microbe_red$collectDate))

#avg by year for each site
soil_microbe_red <- soil_microbe_red %>%
  group_by(siteID, year(soil_microbe_red$collectDate)) %>%
  summarise(microbialbiomass = mean(totalLipidConcentration))

#then avg across all years of available data to come up with a single value for each site
soil_microbe_red <- soil_microbe_red %>%
  group_by(siteID) %>%
  summarise(microbialbiomass_avg = mean(microbialbiomass))


#Now fix microbial units
#NEON units are nanomoles per g of soil, want kg/m^2 for ED

#save copy in case mess up
soil_microbe_red_original <- soil_microbe_red #DELETE LATER!

soil_microbe_red$microbialbiomass_avg <- soil_microbe_red$microbialbiomass_avg * 1000 #gets to nanomoles/kg soil
soil_microbe_red$microbialbiomass_avg <- soil_microbe_red$microbialbiomass_avg / (1*10^9) #gets to moles/kg soil
#molecular mass for fatty acid methyl esters C19:0, internal standard used in NEON PLFA protocols  = 312.5 g/mole
soil_microbe_red$microbialbiomass_avg <- soil_microbe_red$microbialbiomass_avg * 312.5 #gets to g/kg soil
soil_microbe_red$microbialbiomass_avg <- soil_microbe_red$microbialbiomass_avg / 1000 #gets to kg biomass/kg soil



#NEON has data for microbial biomass but doesn't break it up into microbial biomass of C (MBC) 
#and microbial biomass of N (MBN), so have to kind of work around it.

#global average microbial C:N:P ratio was 32:3:1 (Wang et al., 2021: https://doi.org/10.1088/1748-9326/abed78)
#Wang synthesis looked at studies that used the chloroform fumigation-extraction (CFE) method to determine soil microbial biomass 
#Microbial C, N, and P concentrations varied within and across
#different ecosystems and spatial scales, but C:N:P ratios varied surprisingly little

MBC_ratio <- 32/36
MBN_ratio <- 3/36
soil_microbe_red$MBC <- (soil_microbe_red$microbialbiomass_avg) * MBC_ratio #units: kg biomass/kg soil
soil_microbe_red$MBN <- (soil_microbe_red$microbialbiomass_avg) * MBN_ratio #units: kg biomass/kg soil




#Add microbial dataframe to main soil_data dataframe
soil_data <- merge(soil_data, soil_microbe_red)




#now to calculate the remaining variables needed for ED------------------------------------------------------
  #ED soil class, SOM, fsc, stsc, stsl, ssc, psc, msn, fsn


#start w/ED soil class 
soil_texture <- createTexturedata(soil_data$clayTotal, soil_data$siltTotal, soil_data$sandTotal)
soil_texture <- appendTextureclass(as.data.frame(soil_texture), method = "USDA") #uses USDA soil classification method

#add new soil texture class column to main soil dataframe
soil_data$texture_class <- soil_texture$TEXCLASS

#now convert the texture classes to ED soil classes (options are 1-17)
  #no option for peat (ED class #12), bedrock (#13), clayey sand (#16), or clayey silt (#17), FIX LATER
soil_data <- soil_data %>%
  mutate(ED_soil_class = case_when(texture_class == "Sa" ~ 1,
                                   texture_class == "LoSa" ~ 2,
                                   texture_class == "SaLo" ~ 3, 
                                   texture_class == "SiLo" ~ 4, 
                                   texture_class == "Lo" ~ 5, 
                                   texture_class == "SaClLo" ~ 6, 
                                   texture_class == "SiClLo" ~ 7,
                                   texture_class == "ClLo" ~ 8, 
                                   texture_class == "SaCl" ~ 9,
                                   texture_class == "SiCl" ~ 10,
                                   texture_class == "CL" ~ 11,
                                   texture_class == "Si" ~ 14,
                                   texture_class == "HCL" ~ 15
  ))


#Next:
#SOM, fsc, stsc, stsl, ssc, psc, msn, fsn

#SOM estimation method from https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx  
#estimate SOM by multiplying total organic carbon by 1.724
soil_data$SOM <- soil_data$estimatedOC * 1.724

soil_data$MBC <- soil_data$MBC * soil_data$bulkDens #converts from kg biomass/kg soil to kg biomass/m^2 soil
soil_data$MBN <- soil_data$MBN * soil_data$bulkDens #converts from kg biomass/kg soil to kg biomass/m^2 soil

soil_data$fsc <- soil_data$MBC #fsc = fast soil C, same as MBC here b/c MBC already multiplied by bulk density
soil_data$stsc <- soil_data$SOM * 10 #stsc = structural soil carbon, this gives rough estimate
soil_data$stsl <- soil_data$stsc #stsl = structural soil lignin, often same as stsc so can just set equal
soil_data$ssc <- soil_data$carbonTot - soil_data$MBC #ssc = slow soil carbon
soil_data$psc <- 0 #deprecated, set = 0
soil_data$N_mineralized <- soil_data$nitrogenTot * 0.015 #approximation, 0.015 is the mineralization factor
soil_data$msn <- (soil_data$nitrogenTot * soil_data$N_mineralized) / (soil_data$MBN + soil_data$N_mineralized) #msn = mineralized soil nitrogen
soil_data$fsn <- (soil_data$nitrogenTot * soil_data$MBN) / (soil_data$MBN + soil_data$N_mineralized) #fsn = fast soil nitrogen


#export df to csv
write.csv(soil_data, "NEON_ED_soil_data.csv", row.names = FALSE)

