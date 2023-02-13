library(ggplot2)


# Doing some quick analysis to 
# google.user <- dir("~/Library/CloudStorage/")
# out.google <- file.path("~/Library/CloudStorage/", google.user, "My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/output/")

fAll <- dir("extract.v3")
fAll[1:10]



runs.all <- readbulk::read_bulk(directory = "extract.v3", extension = "Site.csv", header = TRUE)
runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")
runs.all$RunID <- paste(runs.all$GCM, runs.all$rcp, runs.all$co2, runs.all$Management, sep="_" )
head(runs.all)

patch.test <- read.csv("extract.v3/MortArb_All_ACCESS1-0_rcp45_statCO2_MgmtGap_Patch.csv")
head(patch.test)

co.test <- read.csv("extract.v3/MortArb_All_ACCESS1-0_rcp45_statCO2_MgmtGap_Cohort.csv")
co.test <- co.test[co.test$pft %in% c(9,10,11) & co.test$dbh>12.7,] # 12.7cm = 5 inches
head(co.test)


co.avg <- aggregate(cbind(dbh, ba, carbs, stress, height, lai, agb, dens.pch, dens.pch.tree, patch.area, patch.age, dens.w))