# Some generic, illustrative figures

# 1. Map of Sites -- Ecoregions (w/ forest cover?)
# 2. Example of Spatially implicit approach & management schemes using patch/cohort init file?

library(rgdal); library(ggplot2); library(maptools); library(raster)

# -------------
# Reading in and formatting (aggregating) the NBCD
# -------------
nbcd <- raster("~/Dropbox/PalEON_CR/PalEON_MIP_Site/Analyses/Change-and-Stability/raw_data/Benchmarks/NBCD_countrywide_biomass_240m_raster/NBCD_countrywide_biomass_mosaic.tif")
nbcd

nbcd2 <- aggregate(nbcd, fac=40) # This should make it approx 5 km res.
nbcd2
plot(nbcd2)

nbcd.df <- data.frame(coordinates(nbcd2))
nbcd.df$Biomass <- as.data.frame(nbcd2)[,1]
summary(nbcd.df)
dim(nbcd.df)
# -------------

# -------------
# Setting up and simplifying the ecoregions (plotting L1)
# -------------
ecoregions <- readOGR("/Volumes/Morton_SDM/GIS_Data/Global_Admin_Areas/Ecoregions/us_eco_l3/us_eco_l3.shp")
summary(ecoregions)
summary(ecoregions$NA_L1NAME)

ecoL1 <- unionSpatialPolygons(ecoregions, ecoregions$NA_L1NAME)
# ecoL1 <- raster::aggregate(ecoregions2, by="NA_L1NAME")
l1.names <- data.frame(NA_L1NAME=row.names(ecoL1))
row.names(l1.names) <- row.names(ecoL1)
ecoL1 <- SpatialPolygonsDataFrame(ecoL1, data=l1.names)
summary(ecoL1)

ecoL1b <- rgeos::gSimplify(ecoL1, tol=10000, topologyPreserve=T)
ecoL1b <- SpatialPolygonsDataFrame(ecoL1b, data=data.frame(ecoL1))
summary(ecoL1b)

ecoL1b <- spTransform(ecoL1b, CRS=projection(nbcd2))

plot(ecoL1b)
ecoL1b$id <- row.names(ecoL1b)
eco.fort.L1b <- fortify(ecoL1b, region="id")
eco.df.L1b <- merge(data.frame(ecoL1b), eco.fort.L1b, all.x=T)
summary(eco.df.L1b)
dim(eco.df.L1b)

# plot(ecoL1b)
# -------------

# -------------
# Reading in the site list
# -------------
sites <- read.csv("../0_setup/Sites_ExperimentalDesign_Test.csv")
summary(sites)

nacp <- read.csv("../0_setup/Fluxnet_DietzeInits.csv")
summary(nacp)

sites <- SpatialPointsDataFrame(sites[,c("lon", "lat")], data=sites, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
nacp <- SpatialPointsDataFrame(nacp[!is.na(nacp$lon),c("lon", "lat")], data=nacp[!is.na(nacp$lon),], proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

sites <- spTransform(sites, projection(nbcd2))
nacp <- spTransform(nacp, projection(nbcd2))

sites <- data.frame(sites)
nacp <- data.frame(nacp)

# -------------


# -------------
# The Figure!
# -------------
usa <- map_data("usa")
# usa <- map("usa", plot=F)
# summary(usa)
# 
usa2 <- SpatialPointsDataFrame(usa[,c("long", "lat")], data=usa, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
usa2 <- spTransform(usa2, projection(nbcd2))
# mask(nbcd2)

# Making it a data frame
# plot(usa2, pch=16)

usa3 <- data.frame(usa2)
names(usa3)[names(usa3) %in% c("long.1", "lat.1")] <- c("x", "y")
summary(usa3)


cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
eco.nf <- c("GREAT PLAINS", "NORTH AMERICAN DESERTS")

png("../0_setup/SiteLocations.png", height=15, width=22, units="in", res=220)
ggplot(data=eco.df.L1b[!eco.df.L1b$NA_L1NAME %in% eco.nf,]) +
  coord_equal() + 
  geom_point(data=nbcd.df[nbcd.df$Biomass>0,],aes(x=x, y=y, color=Biomass), shape=15, size=1) +
  scale_color_gradient2(low="white", high="black", mid="gray30", midpoint=(max(nbcd.df$Biomass)-min(nbcd.df$Biomass))/2) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=NA_L1NAME), alpha=0.5) +
  scale_fill_brewer(palette = "Set3", name="Ecoregion") + 
  geom_point(data=nacp[nacp$lat<48,], aes(x=lon.1, y=lat.1, shape="Future"), size=5, stroke=1.5) +
  geom_point(data=sites, aes(x=lon.1, y=lat.1, shape="This Poster"), size=10) +
  geom_path(data=usa3, aes(x=x, y=y, group=group)) +
  scale_shape_manual(values=c(8, 19), name="Status") +
  theme_bw() +
  theme(legend.box="vertical",
        legend.position="bottom",
        legend.key.size = unit(48, units="points"),
        legend.text = element_text(size=unit(18, "points")),
        legend.title=element_text(size=unit(24, "points"), face="bold"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank()
        )
dev.off()
 