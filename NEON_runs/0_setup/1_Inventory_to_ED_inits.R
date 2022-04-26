# Script to convert 2018 East Woods survey data into ED initialization files
# Management Units:
#  - Hidden Lake
#  - Annual Burn
#  - No Burn
#  - Other
# 
# Will also get Web Soil Survey data to capture any underlying differences in soil conditions factor in.
#
# First run: just lump everythign together

# ------------------------------
# 
site.name= "MortonArb"
site.lat = 41.82
site.lon = -88.04

# ------------------------------
# Getting texture & depth from web soil survey
# ------------------------------
area.weight <- c(1.8, 2.0, 3.8, 14.7, 1.6, 0.0, 14.3, 19.8, 32.6, 2.0, 0.3, 0.2, 0.5, 0.0, 6.4, 0.0)/100
depth <- c(33, 25, 20, 92, 89, 72, 99, 94, 79, 98, 73, 200, 200, 15, 200, 200)/100
sand <- c(14.1, 14.2, 8.4, 9.1, 9.0, 62.5, 12.3, 12.4, 12.8, 12.6, 13.2, 44.1, 39.8, 36.6, 11.0, 0)/100
clay <- c(31.7, 33.4, 36.2, 31.7, 32.4, 14.4, 32.7, 34.0, 32.9, 32.0, 33.4, 18.4, 19.6, 28.1, 28.9, 0)/100

depth.weight <- sum(depth*area.weight)
sand.weight <- sum(sand*area.weight)
clay.weight <- sum(clay*area.weight)
# ------------------------------

# ------------------------------
# Get the 2018 East Woods Inventory tree data:
# ------------------------------
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "East Woods Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
# path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")

# 2018 Trees
# tree.2018 <- readxl::read_excel("/Volumes/GoogleDrive/My Drive/East Woods/East Woods Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018/18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx")
tree.2018 <- readxl::read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
names(tree.2018) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH", "Canopy", "Decay", "Vigor", "Notes")
tree.2018$Sampler <- as.factor(tree.2018$Sampler)
tree.2018$PlotID <- as.factor(tree.2018$PlotID)
tree.2018$Spp.Code <- as.factor(tree.2018$Spp.Code)
tree.2018$Spp.Name <- as.factor(tree.2018$Spp.Name)
tree.2018$DBH <- as.numeric(tree.2018$DBH)
tree.2018$Canopy <- as.factor(tree.2018$Canopy)
tree.2018$Decay <- as.factor(tree.2018$Decay)
tree.2018$Vigor <- as.factor(tree.2018$Vigor)
tree.2018$Density <- 1/(pi*8.92^2) # 250m plot
tree.2018 <- tree.2018[!is.na(tree.2018$Date),]
# tree.2018 <- data.frame(tree.2018[!is.na(tree.2018$Date),])
tree.2018 <- data.frame(tree.2018)
tree.2018$Status <- as.factor(ifelse(tree.2018$Canopy=="S", "dead", "live"))
tree.2018 <- tree.2018[tree.2018$DBH>10 & 
                         !(is.na(tree.2018$Status) | tree.2018$Status=="dead" | tree.2018$Spp.Name=="Unidentified") &
                         tree.2018$PlotID %in% dat.gis[dat.gis$MgmtUnit!="Non-Wooded","PlotID"],]
summary(tree.2018)
summary(droplevels(tree.2018$Spp.Name))
# ------------------------------

# ------------------------------
# Convert to ED2 .css files
# ------------------------------
unique(tree.2018$Spp.Name)

tde.09 <- c("Gleditsia triacanthos", "Gymnocladus dioicus", "Populus alba", "Populus deltoides", "Populus grandidentata", "Prunus serotina", "Robinia pseudoacacia", "Salix sp.", "Rhamnus cathartica", "Phellodendron amurense", "Lonicera maackii", "Cercis canadensis")
tdm.10 <- c("Carya cordiformis", "Carya ovata", "Celtis occidentalis", "Crataegus sp.", "Fraxinus americana", "Fraxinus pennsylvanica", "Fraxinus quadrangulata", "Fraxinus sp.", "Juglans nigra", "Morus alba", "Ostrya virginiana", "Quercus alba", "Quercus ellipsoidalis", "Quercus macrocarpa", "Quercus palustris", "Quercus rubra", "Ulmus americana", "Ulmus pumila", "Ulmus rubra")
tdl.11 <- c("Acer negundo", "Acer platanoides", "Acer saccharinum", "Acer saccharum", "Aesculus sp.", "Tilia americana")

tree.2018$PFT <- as.factor(ifelse(tree.2018$Spp.Name %in% tde.09, "9", ifelse(tree.2018$Spp.Name %in% tdm.10, "10", ifelse(tree.2018$Spp.Name %in% tdl.11, "11", NA))))
summary(tree.2018)

# Next step is to simplify trees into DBH bins; just going with 2 cm bins to make life easier
tree.2018$DBH.round <- round(tree.2018$DBH*5, -1)/5
head(tree.2018)

# Aggregate to the plot level to get the initial state and then we can group plots (= patches)
dat.plot <- aggregate(tree.2018[,c("Density")],
                      by=tree.2018[,c("PlotID", "PFT", "DBH.round")],
                      FUN=sum)
summary(dat.plot)


# Add in our management units
dat.gis <- read.csv(file.path(path.ew, "East Woods Inventory 2018/Analyses_Rollinson/data_processed", "point_info_GIS.csv"))
dat.gis$PlotID2 <- dat.gis$PlotID
dat.gis$PlotID <- as.factor(gsub("-", "", dat.gis$PlotID))
dat.gis$MgmtUnit <- ifelse(is.na(dat.gis$wooded), "Non-Wooded", 
                           ifelse(dat.gis$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(dat.gis$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(dat.gis$unit), "Mixed Management", "No Management"))))
dat.gis$MgmtUnit[is.na(dat.gis$MgmtUnit)] <- "No Management"
dat.gis$MgmtUnit <- as.factor(dat.gis$MgmtUnit)
summary(dat.gis)

dat.plot <- dat.plot[dat.plot$PlotID %in% dat.gis[!is.na(dat.gis$wooded) & dat.gis$wooded=="East Woods","PlotID"],]
names(dat.plot) <- c("patch", "pft", "dbh", "n")
dat.plot$cohort <- 1:nrow(dat.plot)
dat.plot$time <- 2018
dat.plot[,c("hite", "bdead", "balive", "lai")] <- 0
summary(dat.plot)
# ------------------------------

# ------------------------------
# ------------------------------
fprefix <- paste0("MortonArb_EastWoods_All_lat", trunc(site.lat)+0.5, "lon", trunc(site.lon)-0.5)
# ------------
# ------------

css <- dat.plot[, c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")]
css$patch <- as.numeric(css$patch)
summary(css)

write.table(css, paste0("../init_files/", fprefix,".css"), quote = FALSE, row.names = FALSE)
# ------------


# ------------
# Creating a pss file to go with the css
# ------------
pss <- data.frame(time=2018,
                  patch=unique(css$patch),
                  trk=1,
                  age=2018-1922, # Just going with the age of the arboretum; the woods are actually older, but it doesn't matter
                  area=1/length(unique(css$patch)),
                  water=0,
                  fsc=1, # Pecan Default
                  stsc=5,# Pecan Default
                  stsl=5,# Pecan Default
                  ssc=0.01,# Pecan Default
                  psc=0, # dummy variable
                  msn=1,# Pecan Default
                  fsn=1)# Pecan Default
# pss <- pss[, c("time", "patch", "trk", "age", "area", "water")]
summary(pss)

# Some Very rough numbers from Meghan
# mean bulk density = 0.88 g/cm3
bd <- 0.88*1e-3 * (100*100) # kg/m3
SOM <- bd*0.104
C.tot = bd*0.0516
C.mic = 1.09e-3*bd
N.tot = bd*0.0043
N.mic = 377.46e-6*bd # g microbial N per g soil
N.min = 13.39e-6*bd

pss$fsn <- N.tot*N.mic/(N.mic+N.min)
pss$msn <- N.tot*N.min/(N.mic+N.min)
pss$fsc <- C.mic 
pss$ssc <- C.tot-C.mic
pss[,c("stsc", "stsl")] <- SOM*10 # hand-wavy to make more comparable to other numbers

summary(pss)
write.table(pss, paste0("../init_files/", fprefix,".pss"), quote = FALSE, row.names = FALSE)

# ------------
# ------------------------------


# ------------------------------
# ------------------------------
# ------------------------------


