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
library(lubridate)

runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)


runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis/")
dat.precip <- read.csv("../Precip_Weather_Daily")

dat.end <- read.csv("../Drought_Weather_Daily")
dat.end <- dat.end[dat.end$days_since_rain > 40,]
dat.end <- dat.end[6,]

#artifically adding the 15th as the day for the Date objects since you can't make a date object with just month and year
#This is used for plotting not for direct dat comparision
runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "15", sep = "-"))


start <- as.Date(dat.end$Date) - dat.end$days_since_rain

dat.start <- dat.precip[as.Date(dat.precip$Date) %in% start, ]


Good.models <- c("ACCESS1-0")#, "ACCESS1-3", "bcc-csm1-1", "bcc-csm1-1-m", "HadGEM2-CC", "HadGEM2-ES", "MIROC-ESM", "MIROC-ESM-CHEM")
runs.all <- runs.all[runs.all$GCM %in% Good.models, ]
dat.end <- dat.end[dat.end$model %in% Good.models, ]



#Calculating the recovery period and the resilience over that period.
#ED.interest <- c("dbh.mean", "dbh.sd", "height.mean", "height.sd", "nee", "agb", "nee", "soil.moist.surf", "soil.moist.deep")
Dates.list <- list()
for(MOD in unique(runs.all$GCM)){
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    
    #Subsetting to identify the end dates for this model and scenario 
    temp.end <- dat.end[dat.end$model == MOD & dat.end$scenario == RCP, ]
    if(nrow(temp.end) > 0){
      for(i in 1:nrow(temp.end)){
        
        #Identifying the beginning and end of drought dates
        END <- as.Date(temp.end[i, "Date"])
        STR <- END - temp.end[i, "days_since_rain"]
        if(length(STR) > 0){
          
          #Subsetting the ED output starting a year before drought
          df <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                           runs.all$Date >= (STR-lubridate::years(1)), ]
          
          #Finding the mean for the year leading up to drought
          recov.df <- aggregate(agb~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(1)) ,],
                                FUN = mean)
          
          colnames(recov.df) <- c("Management", "first.mean")
          
          recov.df$D.start <- STR
          recov.df$D.end <- END
          
          #D.end is when the drought ends. D.window.end is one year after the end of the drought because I'm giving time for drought's impacts
          recov.df$D.window.end <- recov.df$D.end %m+% years(1)
          
          #1 year after drought end when is the first time agb equals the mean of the year leading up to drought
          for(i in 1:nrow(recov.df)){
            recov.df[i, "recov.Date"] <- df[df$Management == recov.df[i, "Management"] & #Matching management
                                              df$Date > recov.df[i, "D.window.end"] & #Making sure we check after drought has occured
                                              df$agb >= recov.df[i, "first.mean"], "Date"][1]  #Choosing the first date above the original
            
            recov.df[i, "resil.min"] <- min(df[df$Management == recov.df[i, "Management"] & #Matching management
                                                 df$Date >= recov.df$D.end & df$Date <= recov.df$recov.Date, "agb"], na.rm=T) #Pulling the lowest over the window
          }
          
          #Flagging situation where it never recovers to not have a resilience. Not sure how to handle going forward
          recov.df$resil.min <- ifelse(is.na(recov.df$recov.Date), NA, recov.df$resil.min)
          
          #Counting how many days it takes for recovery after the drought window (so 1 year after drought ends)
          recov.df$days_of_recovery <- recov.df$recov.Date - recov.df$D.window.end
          
          #Counting 
          recov.df$resil.diff <- recov.df$first.mean - recov.df$resil.min
          
          
          #In between the end of the drought (rain begins again) and the recovery date what is the local minimum
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]] <- recov.df
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$GCM <- as.factor(MOD)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$rcp <- as.factor(RCP)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$Drought.period <- paste0(STR," to ", END, sep="")
        }
      }
    }
  }
}
drought.df <- dplyr::bind_rows(Dates.list)


#This section creates a graphic that highlights drought windows
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


#This is an older section that is used for anova's on the difference between management groups.
#OLD for now
#ED.interest <- c("dbh.mean", "dbh.sd", "height.mean", "height.sd", "nee", "agb", "nee", "soil.moist.surf", "soil.moist.deep")
Dates.lme <- list()
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
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]] <- temp
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]]$GCM <- as.factor(MOD)
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]]$rcp <- as.factor(RCP)
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]]$Start <- as.factor((STR-lubridate::years(1)))
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]]$End <- as.factor((END + lubridate::years(4)))
          Dates.lme[[paste(MOD, RCP, STR, END, sep="-")]]$Drought.period <- paste0(STR," to ", END, sep="")
        }
      }
    }
  }
}
drought.lme <- dplyr::bind_rows(Dates.lme)

drought.lme$Management <- factor(drought.lme$Management, levels = c("None", "Gap", "Shelter", "Under"))
lm.test <- lme(diff ~ Management-1, random=list(rcp=~1, GCM=~1), data=drought.lme)
hold <- anova(lm.test)
hold

summary <- summary(lm.test)
df.eff <- as.data.frame(summary$tTable)

#Creating a visual that looks at what is happeninguring the drought periods





#--------------------------------------------------------#
#Experimenting with identifying drought based on avaerage
#--------------------------------------------------------#
dat.sum <- aggregate(mean~month+year+model+scenario, dat.precip, FUN = sum)

dat.merge <- runs.all[, c("month", "year", "GCM", "rcp", "precipf", "Management")]

dat.compare <- merge(dat.sum, dat.merge, by.x = c("month", "year", "model", "scenario"), by.y= c("month", "year", "GCM", "rcp"))

dat.sum$days_no_rain <- aggregate(Drought_flag~month+year+model+scenario, dat.precip, FUN = sum)[,c("Drought_flag")]

#dat.sum$Date <- lubridate::ymd(paste(dat.sum$year, dat.sum$month, "15", sep = "-"))

avg.month <- aggregate(mean~month, dat.sum, mean)
avg.month$sd <- aggregate(mean~month,dat.sum,sd)[,c("mean")]

dat.sum$flag <- ifelse(dat.sum$mean < 9.198171e-06, T, F)

dat.end <- dat.sum[dat.sum$flag == T,]

dat.end$Date <- lubridate::ymd(paste(dat.end$year, dat.end$month, "15", sep = "-"))

#Chicago average per month
precip.chi <- data.frame(c(1:12), c(1.75, 1.63, 2.65, 3.68, 3.38, 3.63, 3.51, 4.62, 3.27, 2.71, 3.01, 2.43))
colnames(precip.chi) <- c("month", "precip")
#inches to mm
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


#Where the Exploratory figures begin

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
