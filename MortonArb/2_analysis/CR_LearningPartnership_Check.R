library(ggplot2)


# Doing some quick analysis to 
# google.user <- dir("~/Library/CloudStorage/")
# out.google <- file.path("~/Library/CloudStorage/", google.user, "My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/output/")

fAll <- dir("extract.v3")
fAll[1:10]


# Checking the output Learning Partnership got from Bailey
runs.lp <- readbulk::read_bulk(directory = "~/Downloads/Site_only Bailey Murphy/", extension = "Site.csv", header = TRUE)
runs.lp$Management <- car::recode(runs.lp$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")
runs.lp$RunID <- paste(runs.lp$GCM, runs.lp$rcp, runs.lp$co2, runs.lp$Management, sep="_" )
summary(runs.lp)

runs.yr <- aggregate( cbind(ba.tree.mean, agb, agb.tree.mean.PFT9, agb.tree.mean.PFT10, agb.tree.mean.PFT11, ba.tree.mean.PFT9, ba.tree.mean.PFT10, ba.tree.mean.PFT11) ~ year + GCM + rcp + Management + RunID, data=runs.lp, FUN=mean)
summary(runs.yr)

runs.mgmt.rcp <- aggregate( cbind(ba.tree.mean, agb, agb.tree.mean.PFT9, agb.tree.mean.PFT10, agb.tree.mean.PFT11, ba.tree.mean.PFT9, ba.tree.mean.PFT10, ba.tree.mean.PFT11) ~ year + rcp + Management, data=runs.yr, FUN=mean)
summary(runs.mgmt.rcp)


ggplot(data=runs.mgmt.rcp) +
  facet_grid(rcp~Management) +
  geom_line(aes(x=year, y=ba.tree.mean.PFT9, color="PFT9")) +
  geom_line(aes(x=year, y=ba.tree.mean.PFT10, color="PFT10")) +
  geom_line(aes(x=year, y=ba.tree.mean.PFT11, color="PFT11")) +
  labs(x="Year", y="ba.tree.mean")
  
  


# Checking our output
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