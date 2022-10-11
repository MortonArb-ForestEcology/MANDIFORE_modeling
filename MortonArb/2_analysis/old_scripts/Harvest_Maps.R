# -----------------------------------------------------------
# Take 2018 Survey data and make interpolated maps demostrating what different harvest regimes look like
# Leveraging a lot of stuff form this repository: https://github.com/MortonArb-ForestEcology/EastWoods_Inventory2018
# To interpolate: 
#  1. turn plot data into sparse map by creating a raster
# -----------------------------------------------------------

# -----------------------------------------------------------
# Setting paths, reading packages, etc.
# -----------------------------------------------------------
library(ggplot2); library(raster)

path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
path.2007 <- file.path(path.ew, "Inventory 2007")
path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")
google.gis <- "/Volumes/GoogleDrive/My Drive/East Woods/GIS_files"
morton.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
path.figs <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/figures/"

# -----------------------------------------------------------


# -----------------------------------------------------------
# Read in and format data
# -----------------------------------------------------------
# ------------------------------
# Start by loading the basic spatial data
# ------------------------------
# Add in our management units
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
tree.2018 <- tree.2018[tree.2018$PlotID %in% dat.gis[!is.na(dat.gis$wooded) & dat.gis$wooded=="East Woods","PlotID"],]

summary(tree.2018)
summary(droplevels(tree.2018$Spp.Name))
# ------------------------------

# ------------------------------
# Aggregate to PFTs
# ------------------------------
unique(tree.2018$Spp.Name)

tde.09 <- c("Gleditsia triacanthos", "Gymnocladus dioicus", "Populus alba", "Populus deltoides", "Populus grandidentata", "Prunus serotina", "Robinia pseudoacacia", "Salix sp.", "Rhamnus cathartica", "Phellodendron amurense", "Lonicera maackii", "Cercis canadensis")
tdm.10 <- c("Carya cordiformis", "Carya ovata", "Celtis occidentalis", "Crataegus sp.", "Fraxinus americana", "Fraxinus pennsylvanica", "Fraxinus quadrangulata", "Fraxinus sp.", "Juglans nigra", "Morus alba", "Ostrya virginiana", "Quercus alba", "Quercus ellipsoidalis", "Quercus macrocarpa", "Quercus palustris", "Quercus rubra", "Ulmus americana", "Ulmus pumila", "Ulmus rubra")
tdl.11 <- c("Acer negundo", "Acer platanoides", "Acer saccharinum", "Acer saccharum", "Aesculus sp.", "Tilia americana")

tree.2018$PFT <- as.factor(ifelse(tree.2018$Spp.Name %in% tde.09, "9", ifelse(tree.2018$Spp.Name %in% tdm.10, "10", ifelse(tree.2018$Spp.Name %in% tdl.11, "11", NA))))
summary(tree.2018)
# ------------------------------

# ------------------------------
# Applyign the management scenarios to trees in a list just to keep the data clean
# ------------------------------
dat.management <- list(none=tree.2018,
                       under=tree.2018,
                       shelter=tree.2018,
                       gap=tree.2018)

# Checking harvested area 
0.2 + (1-0.2)*0.2
exp(0.2)


1-(1-0.2)^2
# --------------
# 1) "no management scenario
# --------------
dat.management$none$Management <- "None"
dat.management$none$Density.New <- dat.management$none$Density
summary(dat.management$none$Density.New)
# --------------

# --------------
# 2) Understory harvest
# Harvest.PFTs=c(9, 10, 11)
# SLOG.DBH = c(20, 20, 20),
# SLOG.PROB.G = c(0.01, 0.01, 0.01), # 1% collateral damage
# SLOG.PROB.L = c(0.75, 0.1, 1), # Get rid of all late successional; leave all oaks; 
#Harvest.yrs = data.frame(year=2020:2024, area=0.20)) # Some area won't get touched b/c 20% of 
# --------------
harv.under <- data.frame(PFT=c(9, 10, 11),
                         DBH=c(20, 20, 20), 
                         PROB.G = c(0.01, 0.01, 0.01),
                         PROB.L = c(0.75, 0.1, 1),
                         Area.Cum=1-(1-0.2)^5)

dat.management$under$Management <- "Understory"

for(i in 1:nrow(harv.under)){
  row.under <- which(dat.management$under$PFT==harv.under$PFT[i] & dat.management$under$DBH<harv.under$DBH[i])
  row.over <- which(dat.management$under$PFT==harv.under$PFT[i] & dat.management$under$DBH>=harv.under$DBH[i])
  
  dat.management$under[row.over, "Density.New"] <- dat.management$under[row.over, "Density"]*(1-harv.under$PROB.G[i]*harv.under$Area.Cum[i])
  dat.management$under[row.under, "Density.New"] <- dat.management$under[row.under, "Density"]*(1-harv.under$PROB.L[i]*harv.under$Area.Cum[i])
}
summary(dat.management$under$Density.New)
# --------------

# --------------
# 3) Shelterwood harvest
# --------------
harv.shelter <- data.frame(PFT=c(9, 10, 11),
                           DBH=c(20, 20, 20), 
                           PROB.G = c(0.70, 0.30, 0.70),
                           PROB.L = c(1.0, 1.0, 1.0),
                           Area.Cum=1-(1-0.2)^5)

dat.management$shelter$Management <- "Shelterwood"

for(i in 1:nrow(harv.shelter)){
  row.under <- which(dat.management$shelter$PFT==harv.shelter$PFT[i] & dat.management$shelter$DBH<harv.shelter$DBH[i])
  row.over <- which(dat.management$shelter$PFT==harv.shelter$PFT[i] & dat.management$shelter$DBH>=harv.shelter$DBH[i])
  
  dat.management$shelter[row.over, "Density.New"] <- dat.management$shelter[row.over, "Density"]*(1-harv.shelter$PROB.G[i]*harv.shelter$Area.Cum[i])
  dat.management$shelter[row.under, "Density.New"] <- dat.management$shelter[row.under, "Density"]*(1-harv.shelter$PROB.L[i]*harv.shelter$Area.Cum[i])
}
summary(dat.management$shelter$Density.New)
# --------------

# --------------
# 4) gaps
# --------------
harv.gap <- data.frame(PFT=c(9, 10, 11),
                       DBH=c(20, 20, 20), 
                       PROB.G = c(1.0, 1.0, 1.0),
                       PROB.L = c(1.0, 1.0, 1.0),
                       Area.Cum=1-(1-0.05)^5)

dat.management$gap$Management <- "Gaps"

for(i in 1:nrow(harv.gap)){
  row.under <- which(dat.management$gap$PFT==harv.gap$PFT[i] & dat.management$gap$DBH<harv.gap$DBH[i])
  row.over <- which(dat.management$gap$PFT==harv.gap$PFT[i] & dat.management$gap$DBH>=harv.gap$DBH[i])
  
  dat.management$gap[row.over, "Density.New"] <- dat.management$gap[row.over, "Density"]*(1-harv.gap$PROB.G[i]*harv.gap$Area.Cum[i])
  dat.management$gap[row.under, "Density.New"] <- dat.management$gap[row.under, "Density"]*(1-harv.gap$PROB.L[i]*harv.gap$Area.Cum[i])
}
summary(dat.management$gap$Density.New)

# --------------

dat.tree <- rbind(dat.management$none, dat.management$under, dat.management$shelter, dat.management$gap)
dat.tree$BasalArea.Before <- (pi*(dat.tree$DBH/2)^2)*dat.tree$Density
dat.tree$BasalArea.After <- (pi*(dat.tree$DBH/2)^2)*dat.tree$Density.New
dat.tree$BasalArea.diff <- dat.tree$BasalArea.After-dat.tree$BasalArea.Before
dat.tree$Density.diff <- dat.tree$Density.New - dat.tree$Density
dat.tree$Management <- as.factor(dat.tree$Management)
dat.tree$Management <- factor(dat.tree$Management, levels=c("None", "Understory", "Shelterwood", "Gaps"))
dat.tree$PFT <- car::recode(dat.tree$PFT, "'9'='Early Succ.'; '10'='Mid. Succ.'; '11'='Late Succ.'")
dat.tree$PFT <-factor(dat.tree$PFT, levels=c("Early Succ.", "Mid. Succ.", "Late Succ."))
summary(dat.tree)



png(file.path(path.figs, "Harvest_Size_Histograms_PFT.png"), height=6, width=9.5, units="in", res=180)
ggplot(data=dat.tree) +
  facet_wrap(~Management) +
  geom_rect(xmin=0, xmax=Inf, ymin=-Inf, ymax=0, fill="gray80") +
  geom_histogram(aes(x=DBH, weight=Density.New/length(unique(dat.tree$PlotID))*1e4, fill=PFT), binwidth=10) +
  geom_histogram(aes(x=DBH, weight=Density.diff/length(unique(dat.tree$PlotID))*1e4, fill=PFT), binwidth=10, alpha=0.70) +
  # geom_text(x=125, y=-50, label="Harvested", hjust=1, vjust=1, size=rel(2)) +
  geom_vline(xintercept=20, linetype="dashed", size=0.5) +
  scale_y_continuous(name="Stems per Hectare") +
  scale_x_continuous(name="DBH (cm)", expand=c(0,0)) +
  scale_fill_manual(name="Tree Type", values=c("#1b9e77", "#d95f02", "#7570b3")) +
  theme(legend.position="top",
        legend.title = element_text(size=rel(2), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.key.size = unit(1.5, "lines"),
        axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(2, 2, 2, 2), "lines"))
dev.off()



dat.plot <- aggregate(dat.tree[,c("BasalArea.Before", "BasalArea.After", "BasalArea.diff")],
                      by=dat.tree[,c("Management", "PlotID", "PFT")],
                      FUN=sum)
summary(dat.plot)

dat.tot <- aggregate(dat.tree[,c("BasalArea.Before", "BasalArea.After", "BasalArea.diff")],
                      by=dat.tree[,c("Management", "PlotID")],
                      FUN=sum)
names(dat.tot)[3:5] <- paste0("Tot.", names(dat.tot)[3:5])
summary(dat.tot)

summary(dat.gis)
dat.plot <- merge(dat.plot, dat.tot)
dat.plot <- merge(dat.plot, dat.gis[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16")], all.x=T)
summary(dat.plot)

png(file.path(path.figs, "Harvest_Map_by_PFT.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot) +
  coord_equal() +
  facet_grid(PFT ~ Management) +
  geom_point(aes(x=x.nad83, y=y.nad83, color=cut(BasalArea.After, breaks=c(0,20,40, 60, 80,Inf)), size=cut(BasalArea.After, breaks=c(0,20,40, 60, 80,Inf)))) +
  scale_color_manual(name="Basal Area", values = rev(c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))) +
  scale_size_manual(name="Basal Area", values=c(1,1.5,2,2.5,3)*1.5) +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="gray75"))
dev.off()


png(file.path(path.figs, "Harvest_Map_Loss_by_PFT.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot) +
  coord_equal() +
  facet_grid(PFT ~ Management) +
  ggtitle("Basal Area Lost") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=cut(BasalArea.diff, breaks=c(0, -.001, -5, -10, -15, -20, -25)), size=cut(BasalArea.diff, breaks=c(0,-.001,  -5, -10, -15, -20, -25)))) +
  scale_color_manual(name="Basal Area\nLost", values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6", "black")) +
  scale_size_manual(name="Basal Area\nLost", values=rev(c(1,1,1.5,2,2.5,3)*1.5)) +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="gray75"))
dev.off()

# ------------------------------


# ------------------------------
# Convert to spatial object
# ------------------------------
# 
# ggplot(data=dat.gis) +
#   coord_equal() +
#   geom_point(aes(x=x.utm16, y=y.utm16, color=MgmtUnit))
# 
# # Subset plot data to just hte ones we want
# dat.plot <- dat.plot[dat.plot$PlotID %in% dat.gis[!is.na(dat.gis$wooded) & dat.gis$wooded=="East Woods","PlotID"],]
# summary(dat.plot)
# 
# grid.blank <- raster()
# ------------------------------
# -----------------------------------------------------------
