library(ggplot2)


# Doing some quick analysis to 
google.user <- dir("~/Library/CloudStorage/")
out.google <- file.path("~/Library/CloudStorage/", google.user, "My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/output/")
runs.all <- readbulk::read_bulk(directory = out.google, extension = "Site.csv", header = TRUE)
runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")
runs.all$RunID <- paste(runs.all$GCM, runs.all$rcp, runs.all$co2, runs.all$Management, sep="_" )
head(runs.all)

runs.yr <- aggregate( cbind(tair,precipf, basal.area.tree, density.tree, agb, soil.moist.surf, soil.moist.deep, lai,  tree.height.mean, tree.height.sd, tree.dbh.mean, tree.dbh.sd) ~ year + GCM + rcp + Management + RunID, data=runs.all, FUN=mean)
summary(runs.yr)

runs.mgmt.rcp <- aggregate( cbind(tair,precipf, basal.area.tree, density.tree, agb, soil.moist.surf, soil.moist.deep, lai, tree.height.mean, tree.height.sd, tree.dbh.mean, tree.dbh.sd) ~ year + rcp + Management, data=runs.yr, FUN=mean)
summary(runs.mgmt.rcp)

ggplot(data=runs.mgmt.rcp[runs.mgmt.rcp$year>=2016,]) +
  facet_wrap(~rcp) +
  geom_line(aes(x=year, y=basal.area.tree, color=Management))

ggplot(data=runs.mgmt.rcp[runs.mgmt.rcp$year>=2016,]) +
  facet_wrap(~rcp) +
  geom_line(aes(x=year, y=tree.dbh.mean, color=Management))


for(GCM in unique(runs.yr$GCM)){
  for(RCP in unique(runs.yr$rcp[runs.yr$GCM==GCM])){
    for(MGMT in unique(runs.yr$Management[runs.yr$GCM==GCM & runs.yr$rcp==RCP])){
      rows.now <- which(runs.yr$Management==MGMT & runs.yr$GCM==GCM & runs.yr$rcp==RCP)
      
      agb.ref <- mean(runs.yr$agb[runs.yr$Management==MGMT & runs.yr$GCM==GCM & runs.yr$rcp==RCP & runs.yr$year %in% 2015:2019], na.rm=T)
      
      runs.yr$agb.diff[rows.now] <- runs.yr$agb[rows.now]-agb.ref
      
    } # end MGMT
  } # end RCP
}# end GCM

ggplot(data=runs.yr[runs.yr$year>=2016 & runs.yr$rcp=="rcp45",]) +
  facet_wrap(~GCM) +
  geom_line(aes(x=year, y=agb, group=RunID, color=Management))


ggplot(data=runs.yr[runs.yr$year>=2016 & runs.yr$rcp=="rcp45",]) +
  facet_wrap(~GCM) +
  geom_line(aes(x=year, y=basal.area.tree, group=RunID, color=Management))


ggplot(data=runs.yr[runs.yr$year>=2016 & runs.yr$rcp=="rcp45",]) +
  # facet_wrap(~GCM) +
  geom_line(aes(x=year, y=agb, group=RunID, color=Management))

ggplot(data=runs.yr[runs.yr$year>=2016,]) +
  facet_wrap(~Management) +
  geom_line(aes(x=year, y=agb.diff, group=RunID, color=rcp))


ggplot(data=runs.yr[runs.yr$year>=2016 & runs.yr$Management=="None",]) +
  # facet_wrap(~Management) +
  geom_line(aes(x=year, y=tair, group=RunID, color=rcp))

runs.start <- aggregate(cbind(tair,precipf, basal.area.tree, density.tree, agb, soil.moist.surf, soil.moist.deep, lai, height.mean, height.sd, dbh.mean, dbh.sd) ~ GCM + rcp + Management + RunID, data=runs.yr[runs.yr$year %in% 2015:2019,], FUN=mean)
runs.end <- aggregate(cbind(tair,precipf, basal.area.tree, density.tree, agb, soil.moist.surf, soil.moist.deep, lai, height.mean, height.sd, dbh.mean, dbh.sd) ~ GCM + rcp + Management + RunID,, data=runs.yr[runs.yr$year %in% 2095:2099,], FUN=mean)

vars.agg <- c("tair", "precipf", "basal.area.tree", "density.tree", "agb", "soil.moist.surf", "soil.moist.deep", "lai", "height.mean", "height.sd", "dbh.mean", "dbh.sd")
runs.diff <- runs.end
runs.diff[,vars.agg ] <- runs.end[,vars.agg ] - runs.start[,vars.agg ]

ggplot(data=runs.diff) +
  geom_boxplot(aes(x=Management, y=agb))

ggplot(data=runs.diff) +
  geom_boxplot(aes(x=rcp, y=tair))
