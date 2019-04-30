# Save a new .xml file for each run.  Can't use a global one because it turns on PFTs that need to be left off.
# Using or modeling this after the pecan function write.configs.ed.R

dat.site <- read.csv("Sites_ExperimentalDesign_Test.csv")

# -------------------------------
# Set up the lists of parameters to be written into XML
# -------------------------------
# ------------
# Disturbance settings
# ------------
list.disturb <- list()
list.disturb[["mature_harvest_age"]] <-  60
list.disturb[["plantation_rotation"]] <-  30
# ------------

# ------------
# PFT Settings
# ------------
list.pfts <- list()

list.pfts[["5"]] <- list()

list.pfts[["6"]] <- list()
list.pfts[["6"]][["fire_s_gtht"]] <- 0.15
list.pfts[["6"]][["fire_s_ltht"]] <- 0.00

list.pfts[["7"]] <- list()
list.pfts[["7"]][["include_pft_ag"]] <- 1
list.pfts[["9"]][["plant_min_temp"]] <- 273-15
list.pfts[["7"]][["fire_s_gtht"]] <- 0.75
list.pfts[["7"]][["fire_s_ltht"]] <- 0.33

list.pfts[["8"]] <- list()
list.pfts[["8"]][["fire_s_gtht"]] <- 0.10
list.pfts[["8"]][["fire_s_ltht"]] <- 0.00

list.pfts[["9"]] <- list()
list.pfts[["9"]][["plant_min_temp"]] <- 273-60
list.pfts[["9"]][["fire_s_gtht"]] <- 0.15
list.pfts[["9"]][["fire_s_ltht"]] <- 0.00

list.pfts[["10"]] <- list()
list.pfts[["9"]][["plant_min_temp"]] <- 273-45
list.pfts[["10"]][["fire_s_gtht"]] <- 0.75
list.pfts[["10"]][["fire_s_ltht"]] <- 0.33

list.pfts[["11"]] <- list()
list.pfts[["9"]][["plant_min_temp"]] <- 273-50
list.pfts[["11"]][["fire_s_gtht"]] <- 0.10
list.pfts[["11"]][["fire_s_ltht"]] <- 0.00
# ------------

# -------------------------------


# -------------------------------
# Save the xml file
# -------------------------------
for(SITE in 1:nrow(dat.site)){
  site.name <- dat.site$SiteName[SITE]
  pft.site <- as.numeric(unlist(strsplit(paste(dat.site$pft[SITE]), "-"))) 

  xml.site <- XML::newXMLNode("config")
  
  xml.disturb <- XML::newXMLNode("disturbance", parent=xml.site)
  # Set up disturbances
  for(i in 1:length(list.disturb)){
    child <- XML::newXMLNode(names(list.disturb)[i], parent=xml.disturb)
    XML::xmlValue(child) <- paste(list.disturb[[i]])
  }
  
  # Set up PFTs
  for(PFT in paste(pft.site)){
    xml.pft <- XML::newXMLNode("pft", parent=xml.site)
    pft.num <- XML::newXMLNode("num", parent=xml.pft)
    XML::xmlValue(pft.num) <- PFT
    
    if(length(list.pfts[[PFT]])<1) next
    for(i in 1:length(list.pfts[[PFT]])){
      child <- XML::newXMLNode(names(list.pfts[[PFT]])[i], parent=xml.pft)
      XML::xmlValue(child) <- paste(list.pfts[[PFT]][[i]])
      
    }
  }
  
  XML::saveXML(xml.site, file = paste0("PFTParams_MANDIFORE_", site.name, ".xml"), 
               indent = TRUE, 
               prefix=newXMLCommentNode('<?xml version="1.0"?>\n'),
               doctype='<!DOCTYPE config SYSTEM "ed.dtd">\n')
}
# -------------------------------
