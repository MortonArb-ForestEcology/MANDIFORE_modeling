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
dat.precip <- read.csv("../Drought_Weather_Daily.csv")

dat.end <- read.csv("../Drought_Periods_End.csv")

#Semi-arbitrary cut off. Will discuss in the future.
dat.end <- dat.end[dat.end$days_since_rain >= 20,]
#artifically adding the 1st as the day for the Date objects since you can't make a date object with just month and year
#This is used for plotting not for direct date comparision
runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "01", sep = "-"))

#Good.models <- c("ACCESS1-0", "ACCESS1-3", "bcc-csm1-1", "bcc-csm1-1-m", "HadGEM2-CC", "HadGEM2-ES", "MIROC-ESM", "MIROC-ESM-CHEM")
#runs.all <- runs.all[runs.all$GCM %in% Good.models, ]
#dat.end <- dat.end[dat.end$model %in% Good.models, ]


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
        Season <- temp.end[i, "season"]
        if(length(STR) > 0){
          
          #Subsetting the ED output starting a year before drought
          df <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                           runs.all$Date >= (STR-lubridate::years(10)), ]
          
          #Finding the mean for the year leading up to drought
          recov.df <- aggregate(agb~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                FUN = mean)
          
          colnames(recov.df) <- c("Management", "agb.mean")
          
          recov.df$agb.sd <- aggregate(agb~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                FUN = sd)[,c("agb")]
          
          recov.df$D.start <- STR
          recov.df$D.end <- END

          
          #1 year after drought end when is the first time agb equals the mean of the year leading up to drought
          for(j in 1:nrow(recov.df)){
            dip <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                df$Date >= recov.df$D.end & df$Date <= recov.df$D.end %m+% period("15 months"), "agb"], na.rm=T) #Pulling the lowet point in 15 months

            
            resil.month <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                df$Date >= recov.df$D.end & df$Date <= recov.df$D.end %m+% period("15 months") & df$agb == dip,"Date"]
            
            recov.df[j, "flag.2sig"] <- ifelse(dip < recov.df[j, "agb.mean"] -2*recov.df[j, "agb.sd"] ,T,F)
            
            recov.df[j, "flag.3sig"] <- ifelse(dip < recov.df[j, "agb.mean"] -3*recov.df[j, "agb.sd"] ,T,F)
            
            recov.df[j, "flag.4sig"] <- ifelse(dip < recov.df[j, "agb.mean"] -4*recov.df[j, "agb.sd"],T,F)
            
              if(recov.df[j, "flag.2sig"] == T){
                recov.df[j, "recov.Date"] <- as.Date(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                  df$Date > resil.month & #Making sure we check after drought has occured
                                                  df$agb >= recov.df[j, "agb.mean"] -2*recov.df[j, "agb.sd"], "Date"][1])  #Choosing the first date above the original
                
                #If it never recovers then we look at the lowest point from the drought to the end of the model
                if(is.na(recov.df[j, "recov.Date"])){
                  recov.df[j, "resil.min.ultimate"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                       df$Date >= resil.month & df$Date <= max(df$Date), "agb"])
                  
                  recov.df[j, "resil.month.ultimate"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                     df$Date >= resil.month & df$Date <= max(df$Date) & df$agb == recov.df[j, "resil.min.ultimate"],"Date"]
                  
                  
                  recov.df[j, "resil.min.local"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                               df$Date >= resil.month & df$Date <= recov.df[j, "D.end"] %m+% period("15 months"), "agb"])
                  
                  recov.df[j, "resil.month.local"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                             df$Date >= resil.month & df$Date <= recov.df[j, "D.end"] %m+% period("15 months") & df$agb == recov.df[j, "resil.min.local"],"Date"]
                  
                  
                  recov.df[j, "recov.soil.moist.mean"] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                               df$Date >= resil.month & df$Date <= max(df$Date), "soil.moist.deep"])

                  recov.df[j, "recov.soil.moist.max"] <- max(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                   df$Date >= resil.month & df$Date <= max(df$Date), "soil.moist.deep"])
                  
                  recov.df[j, "recov.soil.moist.min"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                   df$Date >= resil.month & df$Date <= max(df$Date), "soil.moist.deep"])
                  
                  recov.df[j, "recov.nee.mean"] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                             df$Date >= resil.month & df$Date <= max(df$Date), "nee"])
                  
                  
                  recov.df[j, "recov.nee.max"] <- max(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                             df$Date >= resil.month & df$Date <= max(df$Date), "nee"])
                  
                  recov.df[j, "recov.nee.min"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                             df$Date >= resil.month & df$Date <= max(df$Date), "nee"])
                  
                } else if(is.na(recov.df[j, "recov.Date"])==F){ #If it does recoer we look at the lowest point before recovery
                  
                  recov.df[j, "resil.min.ultimate"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                df$Date >= resil.month & df$Date <= max(df$Date), "agb"])
                  
                  recov.df[j, "resil.month.ultimate"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                              df$Date >= resil.month & df$Date <= max(df$Date) & df$agb == recov.df[j, "resil.min.norecov"],"Date"]
                  
                  recov.df[j, "resil.min.local"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                          df$Date >= resil.month & df$Date < recov.df[j, "recov.Date"], "agb"])
                  
                  recov.df[j, "resil.month.local"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                    df$Date >= resil.month & df$Date < recov.df[j, "recov.Date"] & df$agb == recov.df[j, "resil.min"],"Date"]
                  
                  recov.df[j, "recov.soil.moist.mean"] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                    df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "soil.moist.deep"])
                  
                  recov.df[j, "recov.soil.moist.max"] <- max(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                  df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "soil.moist.deep"])
                  
                  recov.df[j, "recov.soil.moist.min"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                  df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "soil.moist.deep"])
                  
                  recov.df[j, "recov.nee.mean"] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                             df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "nee"])
                  
                  
                  recov.df[j, "recov.nee.max"] <- max(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                           df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "nee"])
                  
                  recov.df[j, "recov.nee.min"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                           df$Date >= resil.month & df$Date <= recov.df[j, "recov.Date"], "nee"])
                }
              } else { #Flagging situations were there isn't a signifgant effect of drought
                recov.df[j, "recov.Date"] <- as.Date(NA)
                recov.df[j, "resil.month.local"] <- resil.month
                recov.df[j, "resil.min.local"] <- dip

            }
          }
          #Flagging situation where it never recovers to not have a resilience. Not sure how to handle going forward
          #recov.df$resil.min.local <- ifelse(is.na(recov.df$recov.Date), NA, recov.df$resil.min.local)
          
          #Counting how many days it takes for recovery after the drought window (so 1 year after drought ends)
          recov.df$days_of_recovery <- difftime(recov.df$recov.Date, recov.df$resil.month.local, units = "days")
          
          #Counting 
          recov.df$resil.diff <-  recov.df$resil.min.local - recov.df$agb.mean
          
          recov.df$resil.pcent.diff <- (recov.df$resil.min.local/recov.df$agb.mean - 1) * 100
          
          recov.df$past.soil.moist.mean <- aggregate(soil.moist.deep~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                     FUN = mean)[,c("soil.moist.deep")]
          
          recov.df$past.soil.moist.max <- aggregate(soil.moist.deep~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                    FUN = max)[,c("soil.moist.deep")]
          
          recov.df$past.soil.moist.min <- aggregate(soil.moist.deep~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                    FUN = min)[,c("soil.moist.deep")]
          
          recov.df$past.nee.mean <- aggregate(nee~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                              FUN = mean)[,c("nee")]
          
          recov.df$past.nee.max <- aggregate(nee~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                             FUN = max)[,c("nee")]
          
          recov.df$past.nee.min <- aggregate(nee~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                             FUN = min)[,c("nee")]
          
          
          #In between the end of the drought (rain begins again) and the recovery date what is the local minimum
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]] <- recov.df
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$GCM <- as.factor(MOD)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$rcp <- as.factor(RCP)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$Drought.period <- paste0(STR," to ", END, sep="")
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$season <- Season
          }
      }
    }
  }
}
drought.df <- dplyr::bind_rows(Dates.list)
drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))



sig <- drought.df[drought.df$flag.2sig == T,]

table(sig$Management)

#number of unique drought with a 4xsignificant drought dip
length(unique(sig$Drought.period))

#--------------------------------------#
#Linear regression of resilience for dry conditions
#--------------------------------------#
library(nlme)
drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))

drought.df$year <- year(drought.df$D.start)
#This is what lucien is checking if Management has a signifigant effect on the drop in agb
lm.test <- lme(resil.pcent.diff ~ Management-1, random=list(rcp=~1, GCM=~1, year=~1), data=drought.df[drought.df$D.start >= as.Date("2025-01-01") & drought.df$flag.2sig == T & is.na(drought.df$recov.Date) == F,])
summary(lm.test)
anova(lm.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
lm.2test <- lme(resil.pcent.diff ~ Management, random=list(rcp=~1, GCM=~1, year =~1), data=drought.df[drought.df$D.start >= as.Date("2025-01-01") & drought.df$flag.2sig == T& is.na(drought.df$recov.Date) == F,])
summary(lm.2test)
anova(lm.2test)


#--------------------------------------------#
#Here we only look at signifignt results for drought
#--------------------------------------------#
#This is what lucien is checking if Management has a signifigant effect on the drop in agb
sig.test <- lme(resil.diff ~ Management-1, random=list(rcp=~1, GCM=~1, year=~1), data=sig)
summary(sig.test)
anova(sig.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
sig.2test <- lme(resil.diff ~ Management, random=list(rcp=~1, GCM=~1, year=~1), data=sig)
summary(sig.2test)
anova(sig.2test)

#--------------------------------------------#
#Here we do linear regression for time to recovery. Only including sucessful recoveries. 
#Not sure how to account for failed recovery?
#-------------------------------------------#
recov.sig <- sig[!is.na(sig$days_of_recovery),]

#This is what lucien is checking if Management has a signifigant effect on the drop in agb
sig.test <- lme(days_of_recovery ~ Management-1, random=list(rcp=~1, GCM=~1), data=recov.sig)
summary(sig.test)
anova(sig.test)

#This is what lucien is checking if Management has a signifigant effect compared to the None condition
sig.2test <- lme(days_of_recovery ~ Management, random=list(rcp=~1, GCM=~1), data=recov.sig)
summary(sig.2test)
anova(sig.2test)

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
                               runs.all$Date >= (STR-lubridate::years(1)) & runs.all$Date <= (END + lubridate::years(2)), ]
          
          print(
            ggplot() +
              ggtitle(paste(MOD, RCP, (STR-lubridate::years(1)), (END + lubridate::years(5)))) +
              geom_line(data=df.var, aes(x=Date, y=agb, color = Management))+
              geom_vline(xintercept = as.Date("2070-07-01"), linetype = 4) +
              geom_rect(aes(xmin = STR, xmax = END, ymin = -Inf, ymax = Inf), alpha = 0.4)
          )
        }
      }
    }
  }
}
dev.off()

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
