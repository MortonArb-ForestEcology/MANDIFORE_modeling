library("nlme")

runs.start <- read.csv("../data/Summary_PFTs_Site_Year.csv")

runs.start <- runs.start[runs.start$GCM != "ACCESS1-0", ]

met.vars <- read.csv("../data/CMIP5_TDM_year_byModel.csv")

var.list <- list()
for(VAR in unique(met.vars$var)){
  met.temp <- met.vars[met.vars$var == VAR,]

  runs.all <- merge(runs.start, met.temp, by.x= c('GCM', 'RCP', 'year'), by.y= c('model', 'scenario', 'year'))
  
  runs.first <- runs.all[runs.all$year < 2036 & runs.all$year > 2025,]

  runs.last <- runs.all[runs.all$year > 2089,]


  #This is an abomination of aggregate functions that could be combined but I wrote this quick to get an abstract out.
  AGB.num <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.first,
                     FUN = mean)

  AGB.num[,c("first.mean.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.first,
                                            FUN = mean)[,c("mean")]

  colnames(AGB.num) <- c("Management", "GCM", "RCP", "first.mean.AGB", "first.mean.temp")


  AGB.num[,c("last.mean.AGB")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.last,
                                          FUN = mean)[,c("AGB.tot")]

  AGB.num[,c("last.mean.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.last,
                                           FUN = mean)[,c("mean")]


  #Another aggreagate abomination creates differnet data frame structure for another visual. Can compare first and last
  AGB.num$AGB.diff <-  AGB.num$last.mean.AGB - AGB.num$first.mean.AGB
  AGB.num$temp.diff <- AGB.num$last.mean.temp - AGB.num$first.mean.temp

  lm.test <- lme(AGB.diff ~ temp.diff*Management, random=list(RCP=~1, GCM=~1), data=AGB.num)
  hold <- anova(lm.test)
  var.list[[paste(VAR, sep="-")]]$value <- c(paste(VAR, "Intercept"), paste(VAR, "additive"), paste(VAR, "Mangement Additive"), paste(VAR, "Mangement Interactive"))
  var.list[[paste(VAR, sep="-")]]$'p-value' <- hold$`p-value`
}
dat.var <- dplyr::bind_rows(var.list)
write.csv(dat.var, "../data/Mandifore_Management_Regression_analysis.csv", row.names = F)

dat.worth <- dat.var[dat.var$`p-value` < .05,]
