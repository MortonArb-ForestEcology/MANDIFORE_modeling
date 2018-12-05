# Some generic, illustrative figures

# 1. Map of Sites -- Ecoregions (w/ forest cover?)
# 2. Example of management schemes
# 3. Example of Spatially implicit approach & turnover w/ patch creation?

library(rgdal); library(ggplot2); library(maptools); library(raster)

path.figs <- "/Volumes/GoogleDrive/My Drive/Conferences_Presentations/AGU 2018/"

# -----------------------------------------------------------
# 1. Map of Sites -- Ecoregions (w/ forest cover?)
# -----------------------------------------------------------
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

png(file.path(path.figs, "SiteLocations.png"), height=15, width=22, units="in", res=220)
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
# -------------
# -----------------------------------------------------------

# -----------------------------------------------------------
# 2. Example of management schemes
# -----------------------------------------------------------
# patch <- read.table("../init_files/harvard.NACP.lat42.5lon-72.5.pss", header=T)
# cohort <- read.table("../init_files/harvard.NACP.lat42.5lon-72.5.css", header=T)
cohort <- read.table("../init_files/US-WCr.Inv.lat45.5lon-90.5.css", header=T)
patch <- read.table("../init_files/US-WCr.Inv.lat45.5lon-90.5.pss", header=T)
summary(patch)
summary(cohort)


p.pot <- unique(cohort[cohort$dbh>50, "patch"])
p.pot <- patch[patch$patch %in% p.pot & patch$age>50, "patch"]
# p.use <- patch[patch$age==max(patch$age),"patch"]
# p.use <- patch[patch$age==60,"patch"]
c.pch <- cohort[cohort$patch==p.pot[1],]
dim(c.pch)
summary(c.pch)

# Making a simplified table
c.use <- c.pch[c.pch$dbh>35,]
c.use <- rbind(c.use, c.pch[sample(1:nrow(c.pch), 25),])
summary(c.use)
dim(c.use)

# set.seed(1204) 2014 2004
set.seed(2004)
c.use$x <- sample(1:100, nrow(c.use), replace=F)
c.use$y <- sample(1:100, nrow(c.use), replace=F)
summary(c.use)

ggplot(data=c.use) +
  coord_equal() +
  geom_point(aes(x=x, y=y, color=as.factor(pft), size=dbh)) +
  scale_size_continuous(range=c(5,20)) +
  guides(color=F, size=F) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


pass <- data.frame(treatment="Passive", c.use[,c("x", "y", "dbh", "pft")])
summary(as.factor(pass$pft))

pft.09a <- which(pass$pft==9 & pass$dbh>30) # 15% survival
pft.09b <- which(pass$pft==9 & pass$dbh<30) # 0% survival
pft.10a <- which(pass$pft==10 & pass$dbh>30) # 75% survival
pft.10b <- which(pass$pft==10 & pass$dbh<30) # 33% survival
pft.11a <- which(pass$pft==11 & pass$dbh>30) # 10% survival
pft.11b <- which(pass$pft==11 & pass$dbh<30) #  0% survival

pres.keep <- c(sample(pft.09a,length(pft.09a)*0.15), 
                sample(pft.10a, length(pft.10a)*0.75), sample(pft.10b, length(pft.10b)*0.33),
                sample(pft.10a, length(pft.11a)*0.1))
pres <- data.frame(treatment="Preservation", c.use[pres.keep,c("x", "y", "dbh", "pft")])

produ <- data.frame(treatment="Production", c.use[c.use$dbh<10,c("x", "y", "dbh", "pft")])

pft.09a2 <- which(pass$pft==9 & pass$dbh>50) # 15% survival
pft.10a2 <- which(pass$pft==10 & pass$dbh>50) # 15% survival
pft.11a2 <- which(pass$pft==11 & pass$dbh>50) # 15% survival

ecol.keep <- c(sample(pft.09a2,length(pft.09a2)/2), 
               sample(pft.10a2, length(pft.10a2)/2),
               sample(pft.10a2, length(pft.11a2)/2)
               )
ecol <- data.frame(treatment="Ecological", c.use[ecol.keep,c("x", "y", "dbh", "pft")])

dat.mgmt <- rbind(pass, pres, produ, ecol)

png(file.path(path.figs, "Treatment_Illustration.png"), height=7, width=7, units="in", res=220)
ggplot(data=dat.mgmt) +
  coord_equal() +
  facet_wrap(~treatment) +
  geom_point(aes(x=x, y=y, color=as.factor(pft), size=dbh)) +
  scale_size_continuous(range=c(3,15)) +
  scale_color_brewer(palette = "Dark2") + 
  guides(color=F, size=F) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text=element_text(size=unit(24, "points"), face="bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
dev.off()
# -----------------------------------------------------------


