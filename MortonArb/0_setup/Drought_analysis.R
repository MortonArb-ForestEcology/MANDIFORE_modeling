path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/0_setup/")
dat.met <- read.csv("../Full_Weater_Daily.csv")
dat.met <- dat.met[dat.met$model != "NLDAS"]

#Setting up droguht tracking

dat.precip <- dat.met[dat.met$var == "precipitation_flux",]
dat.precip$days_since_rain <- NA

dat.precip <- dat.precip[!is.na(dat.precip$mean),]


drought <- 0
for(i in 1:nrow(dat.precip)){
  if(dat.precip[i, "mean"] == 0){
    drought <- drought + 1
  } else {
    drought <- 0
  }
  dat.precip[i, "days_since_rain"] <- drought
}


#Adding a date and month object for matching
for(i in 1:nrow(dat.precip)){
  dat.precip[i, "Date"] <- as.Date(dat.precip[i, "yday"], origin = paste(dat.precip[i, "year"], "-01-01", sep = ""))
  dat.precip[i, "month"] <- lubridate::month(dat.precip[i, "Date"])
}

write.csv(dat.precip, "../Precip_Weather_Daily", row.names = F)

dat.precip <- read.csv("../Precip_Weather_Daily")

#Defining the start of a drought for flagging
#We need to pick how many days before a drought "begins" currently using 9 but this should be discussed
dat.start <- dat.precip[dat.precip$days_since_rain == 10,]
for(i in 1:nrow(dat.start)){
  dat.start[i, "Date"] <- as.Date(dat.start[i, "yday"], origin = paste(dat.start[i, "year"], "-01-01", sep = ""))
  dat.start[i, "month"] <- lubridate::month(dat.start[i, "Date"])
}


#Defining the end of a drought for flagging
dat.end <- data.frame()
for(i in 2:nrow(dat.precip)){
  if(dat.precip[i, "days_since_rain"] == 0 & nchar(dat.precip[i-1, "days_since_rain"]) > 1){
    dat.end <- rbind(dat.end, dat.precip[i-1,])
  }
}


write.csv(dat.end, "../Drought_Weather_Daily", row.names = F)

dat.end <- read.csv("../Drought_Weather_Daily")

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")


#dat.end$YM <- paste(dat.end$model, dat.end$scenario , dat.end$year, dat.end$month, sep = "-")
#runs.all$YM <- paste(runs.all$GCM, runs.all$rcp , runs.all$year, runs.all$month, sep = "-")

#runs.all$drought_flag <- ifelse(runs.all$YM %in% dat.end$YM, T, F)
#artifically adding the 15th as the day for the Date objects since you can't make a date object with just month and year
runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "15", sep = "-"))


library(ggplot2)
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Strdates <- as.Date(dat.start[dat.start$model == MOD & dat.start$scenario == RCP, "Date"])
    Enddates <- as.Date(dat.end[dat.end$model == MOD & dat.end$scenario == RCP, "Date"])
    if(length(Enddates) > 0){
    print(
      ggplot() +
        ggtitle(paste(MOD, RCP)) +
        geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
        geom_vline(xintercept = Enddates, linetype = 4) +
        geom_rect(aes(xmin = Strdates, xmax = Enddates, ymin = -Inf, ymax = Inf), alpha = 0.4)
    )
    }
  }
}
dev.off()

pdf(file.path(path.out, "CMIP5_NEE_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Ddates <- runs.all[runs.all$GCM == MOD & runs.all$rcp == RCP & runs.all$drought_flag == T, "Date"]
    if(length(Ddates) > 0){
      print(
        ggplot(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,]) +
          ggtitle(paste(MOD, RCP)) +
          geom_line(aes(x=Date, y=nee, color = Management))+
          geom_vline(xintercept = Ddates, linetype = 4)
      )
    }
  }
}
dev.off()


pdf(file.path(path.out, "CMIP5_density_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Ddates <- runs.all[runs.all$GCM == MOD & runs.all$rcp == RCP & runs.all$drought_flag == T, "Date"]
    if(length(Ddates) > 0){
      print(
        ggplot(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,]) +
          ggtitle(paste(MOD, RCP)) +
          geom_line(aes(x=Date, y=density.tree, color = Management))+
          geom_vline(xintercept = Ddates, linetype = 4)
      )
    }
  }
}
dev.off()


pdf(file.path(path.out, "CMIP5_dbh.sd_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Ddates <- runs.all[runs.all$GCM == MOD & runs.all$rcp == RCP & runs.all$drought_flag == T, "Date"]
    if(length(Ddates) > 0){
      print(
        ggplot(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,]) +
          ggtitle(paste(MOD, RCP)) +
          geom_line(aes(x=Date, y=dbh.sd, color = Management))+
          geom_vline(xintercept = Ddates, linetype = 4)
      )
    }
  }
}
dev.off()


pdf(file.path(path.out, "CMIP5_soil.moist.deep_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    Ddates <- runs.all[runs.all$GCM == MOD & runs.all$rcp == RCP & runs.all$drought_flag == T, "Date"]
    if(length(Ddates) > 0){
      print(
        ggplot(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,]) +
          ggtitle(paste(MOD, RCP)) +
          geom_line(aes(x=Date, y=soil.moist.deep, color = Management))+
          geom_vline(xintercept = Ddates, linetype = 4)
      )
    }
  }
}
dev.off()

