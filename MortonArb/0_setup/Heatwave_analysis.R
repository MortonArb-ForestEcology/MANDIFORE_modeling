path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/0_setup/")

runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "15", sep = "-"))


dat.met <- read.csv("../Full_Weater_Daily.csv")
dat.met <- dat.met[dat.met$model %in% unique(runs.all$GCM),]
dat.heat <- dat.met[dat.met$var == "air_temperature_maximum",]


dat.avg <- aggregate(mean ~ yday,data=dat.heat, mean)
dat.avg$sd <- as.vector(aggregate(mean ~ yday,data=dat.heat, sd)[["mean"]])
for(YR in unique(dat.heat$year)){
  for(DAY in unique(dat.heat$yday)){
    dat.heat[dat.heat$year == YR & dat.heat$yday == DAY, "flag"] <- 
      ifelse(dat.heat[dat.heat$year == YR & dat.heat$yday == DAY, "mean"]>dat.avg[dat.avg$yday == DAY, "mean"]+3*dat.avg[dat.avg$yday == DAY, "sd"], T, F)
  }
}

dat.wave <- dat.heat[dat.heat$flag == T, ]
dat.wave <- dat.wave[!is.na(dat.wave$mean),]

for(i in 1:nrow(dat.wave)){
  dat.wave[i, "Date"] <- as.Date(dat.wave[i, "yday"], origin = paste(dat.wave[i, "year"], "-01-01", sep = ""))
  dat.wave[i, "month"] <- lubridate::month(dat.wave[i, "Date"])
}


#These are the forest variables we want to look at over time
var.interest <- c("agb")
#var.interest <- c("agb", "density.tree", "NEE", "dbh.sd", "soil.moist.deep")

library(ggplot2)
path.out = "../met.v3"
for(VAR in var.interest){
  pdf(file.path(path.out, paste("CMIP5_", VAR, "_Heat.pdf", sep = "")), height=11, width=8.5)
  for(MOD in unique(runs.all$GCM)){
    for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
      Dates <- as.Date(dat.wave[dat.wave$model == MOD & dat.wave$scenario == RCP, "Date"])
      if(length(Dates) > 0){
        print(
          ggplot() +
            ggtitle(paste(MOD, RCP)) +
            geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
            geom_vline(xintercept = Dates, linetype = 4)
        )
      }
    }
  }
}
dev.off()

pdf(file.path(path.out, "CMIP5_AGB_Heat.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Ddates <- as.Date(dat.wave[dat.wave$model == MOD & dat.wave$scenario == RCP, "Date"])
    if(length(Ddates) > 0){
      print(
        ggplot(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,]) +
          ggtitle(paste(MOD, RCP)) +
          geom_line(aes(x=Date, y=agb, color = Management))+
          geom_vline(xintercept = Ddates, linetype = 4)
      )
    }
  }
}
dev.off()
