#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script performs PCA on the Ed output to evalute trends
# Inputs: ED2 Morton Arb site data 
# Outputs: Multiple PCA biplots for different variables
# Notes: I'm unsure if the scaling done in the PCA functions is enough Christy seems to do more in her scripts

#----------------------------------------------------------------------------------------------------------------------#

library(vegan)
library(ape)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readbulk)

path.script <- "C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis"

path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

path.figs <- file.path("../figures/")

setwd(path.read)

runs.yr <- read_bulk(directory = "output", extension = ".csv", header = TRUE)

setwd(path.script)

#----------------------------------------------------------------------------#
# First ordination is ED output vs daily output
# Probably not the most informative but not sure
#----------------------------------------------------------------------------#

#Subsetting columns of interest (not weather variables or idenifiers)
col <- colnames(runs.yr)

col <- col[c(17:35, 37:44)]
#col <- col[c(8:35, 37:44)]
#Removing runoss.subsurf because it seems to break

runs.PCA <- subset(runs.yr, select = c(col))

runs.scale <- as.data.frame(scale(runs.PCA))

#Scale equals true because we have different units across variables so we need to standardize them with mean zero and no unit variance
PCA <- rda(runs.PCA, scale = TRUE)

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 41%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2

#--------------------------------------------------------------------------------------------#
#CHRISTY WHY IN YOUR CODE DO YOU NOT SCALE AND CENTER USING THE BUILT IN FUNCTIONS IN PRCOMP
#____________________________________________________________________________________________#
#Malus PCA 2
malus.pca2 <- prcomp(runs.PCA, center = TRUE, scale. = TRUE)
summary(malus.pca2)


#----------------------------------------------------------------------------------------#
# These PCA's are done using the difference from first decade to last decade for each variable
# This first section sets up the dataframe how we want. THey are then transformed for specific PCA
#----------------------------------------------------------------------------------------#
runs.all <- runs.yr

col <- colnames(runs.all[c(17:35, 37:44)])

dat.diff <- data.frame(matrix(NA, nrow = 116, ncol = 1))
colnames(dat.diff) <- c("RunID")
i <- 2
for(COL in col){
  
  runs.first <- runs.all[runs.all$year < 2036 & runs.all$year > 2025,]
  
  runs.last <- runs.all[runs.all$year > 2089,]
  
  #This is an abomination of aggregate functions that could be combined but I wrote this quick to get an abstract out.
  AGB.num <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.first,
                       FUN = mean)
  
  colnames(AGB.num) <- c("Management", "GCM", "rcp", "first.mean")
  
  
  AGB.num[,c("last.mean")] <- aggregate(eval(as.symbol(COL))~Management+GCM+rcp, data =runs.last,
                                        FUN = mean)["eval(as.symbol(COL))"]
  
  
  #Another aggreagate abomination creates differnet data frame structure for another visual. Can compare first and last
  AGB.num[,c(COL)] <-  AGB.num$last.mean - AGB.num$first.mean
  
  AGB.num <- AGB.num[,c("Management", "GCM", "rcp", COL)]
  
  AGB.num <- AGB.num %>% 
    unite("RunID", c(Management, GCM, rcp))
  
  dat.diff[,1] <- AGB.num$RunID
  dat.diff[, i] <- AGB.num[, c(COL)]
  colnames(dat.diff)[i] <- c(COL)
  
  i <- i+1
  
  #var.list[[paste(COL, sep="-")]]$RunID <- AGB.num$RunID
  #var.list[[paste(COL, sep="-")]]$diff <- AGB.num[,c(COL)]
}

#------------------------------------------------------------------------------------#
# Here is the first PCA that is done using the full RUNID vs the difference
#------------------------------------------------------------------------------------#
dat.RUN <- dat.diff
rownames(dat.RUN) <- dat.RUN[,"RunID"]
dat.RUN[,"RunID"] <- NULL

dat.RUN.TR <- data.table::transpose(dat.RUN)
colnames(dat.RUN.TR) <- rownames(dat.RUN)
rownames(dat.RUN.TR) <- colnames(dat.RUN)

#Scale equals true because we have different units across variables so we need to standardize them with mean zero and no unit variance
PCA.RUN <- rda(dat.RUN, scale = TRUE)
PCA.RUN.TR <- rda(dat.RUN.TR, scale = TRUE)

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA.RUN$CA$eig)/sum(PCA.RUN$CA$eig)) 
barplot(as.vector(PCA.RUN.TR$CA$eig)/sum(PCA.RUN.TR$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.RUN$CA$eig)/sum(PCA.RUN$CA$eig))[1:2]) # 62%, this is ok.
# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.RUN.TR$CA$eig)/sum(PCA.RUN.TR$CA$eig))[1:2]) # 84%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA.RUN)
plot(PCA.RUN.TR)

plot(PCA.RUN, display = "sites", type = "text")
plot(PCA.RUN.TR, display = "sites", type = "text")

plot(PCA.RUN, display = "species", type = "text")
plot(PCA.RUN.TR, display = "species", type = "text")


# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
png(file = "../figures/PCA_RUNID_vs_Vardiff.png", height=12, width=12, units="in", res=120)
biplot(PCA.RUN, choices = c(1,2), type = c("text", "text"))
dev.off()


png(file = "../figures/PCA_Vardiff_vs_RUNID.png", height=12, width=12, units="in", res=120)
biplot(PCA.RUN.TR, choices = c(1,2), type = c("text", "text"))
dev.off()

#-------------------------------------------------------------------------------#
# The PCA under here focus on GCM vs the difference in ED outputs first decade and alst decade
#-------------------------------------------------------------------------------#
dat.GCM <- dat.diff

dat.GCM$GCM <- sapply(strsplit(dat.GCM$RunID, "_"), "[", 2)
dat.GCM[, "RunID"] <- NULL

#Aggregating by GCM
dat.GCM <- aggregate(.~GCM, dat.GCM, mean)

rownames(dat.GCM) <- dat.GCM[,"GCM"]
dat.GCM[, "GCM"] <- NULL

dat.GCM.TR <- data.table::transpose(dat.GCM)
colnames(dat.GCM.TR) <- rownames(dat.GCM)
rownames(dat.GCM.TR) <- colnames(dat.GCM)

#Scale equals true because we have different units across variables so we need to standardize them with mean zero and no unit variance
PCA.GCM <- rda(dat.GCM, scale = TRUE)
PCA.GCM.TR <- rda(dat.GCM.TR, scale = TRUE)

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA.GCM$CA$eig)/sum(PCA.GCM$CA$eig)) 
barplot(as.vector(PCA.GCM.TR$CA$eig)/sum(PCA.GCM.TR$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.GCM$CA$eig)/sum(PCA.GCM$CA$eig))[1:2]) # 63%, this is ok.
# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.GCM.TR$CA$eig)/sum(PCA.GCM.TR$CA$eig))[1:2]) # 94%, this is good.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA.GCM)
plot(PCA.GCM.TR)

plot(PCA.GCM, display = "sites", type = "text")
plot(PCA.GCM.TR, display = "sites", type = "text")

plot(PCA.GCM, display = "species", type = "text")
plot(PCA.GCM.TR, display = "species", type = "text")


# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
png(file = "../figures/PCA_GCM_vs_Vardiff.png", height=12, width=12, units="in", res=120)
biplot(PCA.GCM, choices = c(1,2), type = c("text", "text"))
dev.off()


png(file = "../figures/PCA_Vardiff_vs_GCM.png", height=12, width=12, units="in", res=120)
biplot(PCA.GCM.TR, choices = c(1,2), type = c("text", "text"))
dev.off()

#-------------------------------------------------------------------------------#
# The PCA under here focus on Management vs the difference in ED outputs first decade and alst decade
#-------------------------------------------------------------------------------#
dat.MNG <- dat.diff

dat.MNG$MNG <- sapply(strsplit(dat.MNG$RunID, "_"), "[", 1)
dat.MNG[, "RunID"] <- NULL

#Aggregating by MNG
dat.MNG <- aggregate(.~MNG, dat.MNG, mean)

rownames(dat.MNG) <- dat.MNG[,"MNG"]
dat.MNG[, "MNG"] <- NULL

dat.MNG.TR <- data.table::transpose(dat.MNG)
colnames(dat.MNG.TR) <- rownames(dat.MNG)
rownames(dat.MNG.TR) <- colnames(dat.MNG)

#Scale equals true because we have different units across variables so we need to standardize them with mean zero and no unit variance
PCA.MNG <- rda(dat.MNG, scale = TRUE)
PCA.MNG.TR <- rda(dat.MNG.TR, scale = TRUE)

# Now plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA.MNG$CA$eig)/sum(PCA.MNG$CA$eig)) 
barplot(as.vector(PCA.MNG.TR$CA$eig)/sum(PCA.MNG.TR$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?

# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.MNG$CA$eig)/sum(PCA.MNG$CA$eig))[1:2]) # 90%, this is good.
# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA.MNG.TR$CA$eig)/sum(PCA.MNG.TR$CA$eig))[1:2]) # 99%, this is good.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the plot function
plot(PCA.MNG)
plot(PCA.MNG.TR)

plot(PCA.MNG, display = "sites", type = "text")
plot(PCA.MNG.TR, display = "sites", type = "text")

plot(PCA.MNG, display = "species", type = "text")
plot(PCA.MNG.TR, display = "species", type = "text")


# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
png(file = "../figures/PCA_MNG_vs_Vardiff.png", height=12, width=12, units="in", res=120)
biplot(PCA.MNG, choices = c(1,2), type = c("text", "text"))
dev.off()


png(file = "../figures/PCA_Vardiff_vs_MNG.png", height=12, width=12, units="in", res=120)
biplot(PCA.MNG.TR, choices = c(1,2), type = c("text", "text"))
dev.off()


