#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script crates plots and tables exploring drought periods impact on variables of interest
# Inputs: ED2 Morton Arb site data
#         Drought and preicpitaiton dataframes from 1a_Drought_Prep
# Outputs: Plots mmostly exploring agb, basal tree area, and density
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)
library(dplyr)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis/")
dat.met <- read.csv("../Full_Weather_Daily.csv")
dat.met <- dat.met[dat.met$model %in% unique(runs.all$GCM),]

dat.precip <- read.csv("../Precip_Weather_Daily")

dat.end <- read.csv("../Drought_Weather_Daily")

#dat.end$YM <- paste(dat.end$model, dat.end$scenario , dat.end$year, dat.end$month, sep = "-")
#runs.all$YM <- paste(runs.all$GCM, runs.all$rcp , runs.all$year, runs.all$month, sep = "-")

#runs.all$drought_flag <- ifelse(runs.all$YM %in% dat.end$YM, T, F)
#artifically adding the 15th as the day for the Date objects since you can't make a date object with just month and year
runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "15", sep = "-"))


start <- as.Date(dat.end$Date) - dat.end$days_since_rain

dat.start <- dat.precip[as.Date(dat.precip$Date) %in% start, ]


Good.models <- c("ACCESS1-0", "ACCESS1-3", "bcc-csm1-1", "bcc-csm1-1-m", "HadGEM2-CC", "HadGEM2-ES", "MIROC-ESM", "MIROC-ESM-CHEM")
runs.all <- runs.all[runs.all$GCM %in% Good.models, ]
dat.end <- dat.end[dat.end$model %in% Good.models, ]

#ED.interest <- c("dbh.mean", "dbh.sd", "height.mean", "height.sd", "nee", "agb", "nee", "soil.moist.surf", "soil.moist.deep")
Dates.list <- list()
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    if(nrow(temp.end) > 0){
      for(i in 1:nrow(temp.end)){
        END <- as.Date(temp.end[i, "Date"])
        STR <- END - temp.end[i, "days_since_rain"]
        if(length(STR) > 0){
          df <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                           runs.all$Date >= (STR-lubridate::years(1)) & runs.all$Date <= (END + lubridate::years(4)), ]
          
          temp <- aggregate(soil.moist.deep~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(1)) ,],
                            FUN = mean)
          
          colnames(temp) <- c("Management", "first.mean")
          
          temp$last.mean <- aggregate(soil.moist.deep~Management, data = df[df$Date >= (max(df$Date) %m-% years(1)) & df$Date <= max(df$Date)  ,],
                                      FUN = mean)[,c("soil.moist.deep")]
          
          temp$diff <-  temp$last.mean - temp$first.mean
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]] <- temp
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$GCM <- as.factor(MOD)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$rcp <- as.factor(RCP)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$Start <- as.factor((STR-lubridate::years(1)))
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$End <- as.factor((END + lubridate::years(4)))
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$Drought.period <- paste0(STR," to ", END, sep="")
        }
      }
    }
  }
}
drought.df <- dplyr::bind_rows(Dates.list)

drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))
lm.test <- lme(diff ~ Management-1, random=list(rcp=~1, GCM=~1), data=drought.df)
hold <- anova(lm.test)
hold

sum <- summary(lm.test)
df.eff <- as.data.frame(sum$tTable)

#Creating a visual that looks at what is happeninguring the drought periods

#Dates.list <- list()
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought_window.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    if(nrow(temp.end) > 0){
      for(i in 1:nrow(temp.end)){
        END <- as.Date(temp.end[i, "Date"])
        STR <- END - temp.end[i, "days_since_rain"]
        if(length(STR) > 0){
          df.var <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                               runs.all$Date >= (STR-lubridate::years(1)) & runs.all$Date <= (END + lubridate::years(5)), ]
          
          print(
            ggplot() +
              ggtitle(paste(MOD, RCP, (STR-lubridate::years(1)), (END + lubridate::years(5)))) +
              geom_line(data=df.var, aes(x=Date, y=agb, color = Management))+
              #geom_vline(xintercept = Enddates, linetype = 4) +
              geom_rect(aes(xmin = STR, xmax = END, ymin = -Inf, ymax = Inf), alpha = 0.4)
          )
        }
      }
    }
  }
}
dev.off()


#--------------------------------------------------------#
#Experimenting with identifying drought based on avaerage
#--------------------------------------------------------#
dat.sum <- aggregate(mean~model+scenario+month+year, dat.precip, sum)

avg.month <- aggregate(mean~month, dat.sum, mean)
avg.month$sd <- aggregate(mean~month,dat.sum,sd)[,c("mean")]

#by.month <- aggregate(mean~model+scenario+month+year, dat.precip, mean)


dat.sum$flag <- ifelse(dat.sum$mean < 9.198171e-06, T, F)

dat.end <- dat.sum[dat.sum$flag == T,]

dat.end$Date <- lubridate::ymd(paste(dat.end$year, dat.end$month, "15", sep = "-"))


precip.chi <- data.frame(c(1:12), c(1.75, 1.63, 2.65, 3.68, 3.38, 3.63, 3.51, 4.62, 3.27, 2.71, 3.01, 2.43))
colnames(precip.chi) <- c("month", "precip")
precip.chi$precip <- precip.chi$precip * 25.4

precip.chi$sd <- aggregate(precipf~month, runs.all, sd)[, c("precipf")]


for(Month in unique(runs.all$month)){
  avg <- precip.chi[precip.chi$month == Month, "precip"]
  runs.all[runs.all$month == Month, "d_flag"] <- 
    ifelse(runs.all[runs.all$month == Month, "precipf"] < 
             avg/8, T, F)
}

dat.end <- runs.all[runs.all$d_flag == T,]

dat.end$Date <- lubridate::ymd(paste(dat.end$year, dat.end$month, "15", sep = "-"))

library(ggplot2)
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought_monthtest.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$GCM == MOD & dat.end$rcp == RCP, ]
    Enddates <- as.Date(temp.end[, "Date"])
    if(length(Enddates) > 0){
      print(
        ggplot() +
          ggtitle(paste(MOD, RCP)) +
          geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
          geom_vline(xintercept = Enddates, linetype = 4) 
        #geom_rect(aes(xmin = Strdates, xmax = Enddates, ymin = -Inf, ymax = Inf, fill = letters[1:4]), alpha = 0.4)
      )
    }
  }
}
dev.off()





library(ggplot2)
path.out = "../met.v3"
pdf(file.path(path.out, "CMIP5_AGB_Drought.pdf"), height=11, width=8.5)
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    Enddates <- as.Date(temp.end[, "Date"])
    Strdates <- as.Date(temp.end$Date) - temp.end$days_since_rain
    if(length(Strdates) > 0){
    print(
      ggplot() +
        ggtitle(paste(MOD, RCP)) +
        geom_line(data=runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP,], aes(x=Date, y=agb, color = Management))+
        #geom_vline(xintercept = Enddates, linetype = 4) +
        geom_rect(aes(xmin = Strdates, xmax = Enddates, ymin = -Inf, ymax = Inf, fill = letters[1:4]), alpha = 0.4)
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

