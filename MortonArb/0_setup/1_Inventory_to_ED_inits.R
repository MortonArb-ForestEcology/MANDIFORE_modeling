# Script to convert 2018 East Woods survey data into ED initialization files
# Management Units:
#  - Hidden Lake
#  - Annual Burn
#  - No Burn
#  - Other
# 
# Will also get Web Soil Survey data to capture any underlying differences in soil conditions factor in.


# ------------------------------
#
# ------------------------------
dat.gis <- read.csv(file.path(path.ew, "Inventory 2018/Analyses_Rollinson/data_processed", "point_info_GIS.csv"))
dat.gis$PlotID2 <- dat.gis$PlotID
dat.gis$PlotID <- as.factor(gsub("-", "", dat.gis$PlotID))
dat.gis$MgmtUnit <- ifelse(is.na(dat.gis$wooded), "Non-Wooded", 
                           ifelse(dat.gis$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(dat.gis$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(dat.gis$unit), "Mixed Management", "No Management"))))
dat.gis$MgmtUnit[is.na(dat.gis$MgmtUnit)] <- "No Management"
dat.gis$MgmtUnit <- as.factor(dat.gis$MgmtUnit)
summary(dat.gis)
# ------------------------------

# ------------------------------
# Select soils data from Meghan Midgley from the Chicago Wilderness burn soil
# ------------------------------
dat.soil <- read.csv("CWBurns_Midgley_soil_data_master_copy.csv")
dat.soil$Depth <- car::recode(dat.soils$Depth, "'15-May'='5-15'")
# dat.soil <- dat.soil[dat.soil$site.gen=="morton",]
dat.soil[dat.soil$site.gen=="morton", c("Treatment", "perc.clay", "perc.sand", "n.org", "n.min", "n.inorg", "DOC", "c.mineralization")]

summary(dat.soil[dat.soil$Treatment=="Control", c("Treatment", "perc.clay", "perc.sand", "n.org", "n.min", "n.inorg", "DOC", "c.mineralization")])
summary(dat.soil[, c("Treatment", "perc.clay", "perc.sand", "n.org", "n.min", "n.inorg", "DOC", "c.mineralization")])

lm.norg <- nlme::lme(n.org ~ Treatment, random=list(site.gen=~1, PlotID=~1), data=dat.soil, na.action=na.omit)
dat.soil$pred.n.org <- predict(lm.norg, newdata=dat.soil)
summary(lm.norg)
# summary(dat.soil)
# ------------------------------

# ------------------------------
# Get the 2018 East Woods Inventory tree data:
# ------------------------------
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
# path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")

# 2018 Trees
tree.2018 <- readxl::read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Tree Layer")
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

# Try cluster to see how plots shake out with composition/structure
set.seed(09111016)
plot.cluster <- kmeans(dat.plot[,c("DBH.round", "PFT", "x")], centers=10) 
summary(plot.cluster$cluster)

dat.plot$group <- as.factor(plot.cluster$cluster)
summary(dat.plot)

# Add in our management units
dat.plot$
# ------------------------------



