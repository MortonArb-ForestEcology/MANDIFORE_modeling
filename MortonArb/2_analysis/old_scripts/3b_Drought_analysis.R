#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script crates the dataframe containing forests responses to dry periods
# Inputs: ED2 Morton Arb site data
#         Drought and preicpitaiton dataframes from 1a_Drought_Prep
# Outputs: Dataframe containing resistance and recovery information
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
path.read <- "../data/"

library(dplyr)
library(lubridate)
library(ggplot2)

runs.all <- read.csv(paste0(path.read, "All_runs_mod.csv"))



#drought <- 0
#for(i in 1:nrow(runs.all)){
  #LEss than 50% compared to the standard and only during growing months
#  if(!is.na(runs.all[i, "roll.pdiff.prcp"]) & runs.all[i, "roll.pdiff.prcp"] <= .50 & runs.all[i, "month"] > 4 & runs.all[i, "month"] < 11){
#    drought <- drought + 1
#  } else {
#    drought <- 0
#  }
#  runs.all[i, "months_of_drought"] <- drought
#} 


dat.end <- runs.all[runs.all$months_of_drought > 1, ]

#dat.end <- read.csv("../Drought_Periods_End.csv")

#Semi-arbitrary cut off. Will discuss in the future.
#dat.end <- dat.end[dat.end$days_since_rain >= 14,]

#Removing any droughts that occur before management separates the scenarios
dat.end <- dat.end[dat.end$Date >= as.Date("2025-01-01"),]

#Calculating the recovery period and the resilience over that period.
#Calculating the temp at the start of the model run
#Structural variables of interest
ED.interest <- c("dbh.mean", "dbh.sd", "height.mean", "height.sd", "density.tree", "nee", "soil.moist.surf", "soil.moist.deep")
leap <- data.frame()
Dates.list <- list()

# put this before start of loop
total <-  length(unique(runs.all$GCM))

# put this before closing braces of loop
pb <- winProgressBar(title = "progress bar", min = 0, max = total , width = 300)
num <- 1
Sys.sleep(0.1)
for(MOD in unique(runs.all$GCM)){
  setWinProgressBar(pb, num, title=paste( round(num/total*100, 0),"% done"))
  num <- 1+num
  for(RCP in unique(runs.all[runs.all$GCM == MOD, "rcp"])){
    
    #Subsetting to identify the end dates for this model and scenario 
    temp.end <- dat.end[dat.end$GCM == MOD & dat.end$rcp == RCP, ]
    if(nrow(temp.end) > 0){
      for(i in 1:nrow(temp.end)){
        
        #Identifying the beginning and end of drought dates
        END <- as.Date(temp.end[i, "Date"])
        STR <- END %m-% months(temp.end[i, "months_of_drought"])
        #Season <- temp.end[i, "season"]
        if(length(STR) > 0){
          
        #Subsetting the ED output starting 10 years before drought with an exception for leap years
        if(month(STR) == 2 & day(STR) == 29){
          #This math can't find leap years so if the start date is a leap year I need it to go back an extra day 2-28
        }else{
          df <- runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & 
                           runs.all$Date >= (STR-years(10)), ]
        }
          
          #Finding the mean for the year leading up to drought
          recov.df <- aggregate(agb~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                FUN = mean)
          
          colnames(recov.df) <- c("Management", "past.agb.mean")
          
          recov.df$past.agb.sd <- aggregate(agb~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                FUN = sd)[,c("agb")]
          
          recov.df$D.start <- STR
          recov.df$D.end <- END

          
          #5 years after drought end when is the first time agb equals the mean of the year leading up to drought
          for(j in 1:nrow(recov.df)){
            dip <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                df$Date >= recov.df$D.start & df$Date <= recov.df$D.start %m+% period("60 months"), "roll.agb"], na.rm=T) #Pulling the lowet point in 60 months
            
            lai <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                            df$Date >= recov.df$D.start & df$Date <= recov.df$D.start %m+% period("60 months"), "roll.lai"], na.rm=T) #Pulling the lowet point in 60 months
            
            month.agb.min <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                  df$Date >= recov.df$D.start & df$Date <= recov.df$D.start %m+% period("60 months") & df$roll.agb == dip,"Date"]
            
            month.lai.min <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                  df$Date >= recov.df$D.start & df$Date <= recov.df$D.start %m+% period("60 months") & df$roll.lai == lai,"Date"][1]
            
            OG.temp <- mean(runs.all[runs.all$GCM==MOD & runs.all$rcp == RCP & runs.all$Management == recov.df[j, "Management"] &
                                       runs.all$Date > min(runs.all$Date) & runs.all$Date < (min(runs.all$Date)%m+% years(10)), "tair"])
            
            
            New.temp <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                  df$Date >= recov.df$D.start %m-% years(10) & df$Date <= recov.df$D.start, "tair"], na.rm=T) #Pulling the mean temp for past 10 years
            
            End.temp <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                  df$Date >= max(df$Date) %m-% years(10) & df$Date <= max(df$Date), "tair"], na.rm=T) #Pulling the mean temp for last 10 years
            
            
            recov.df[j, "flag.2sig"] <- ifelse(dip < recov.df[j, "past.agb.mean"] -2*recov.df[j, "past.agb.sd"] ,T,F)
            
            recov.df[j, "flag.3sig"] <- ifelse(dip < recov.df[j, "past.agb.mean"] -3*recov.df[j, "past.agb.sd"] ,T,F)
            
            recov.df[j, "flag.4sig"] <- ifelse(dip < recov.df[j, "past.agb.mean"] -4*recov.df[j, "past.agb.sd"],T,F)
            
            recov.df[j, "agb.min.local"] <- dip
            
            recov.df[j, "lai.min.local"] <- lai
            
            recov.df[j, "month.agb.min.local"] <- month.agb.min
            
            recov.df[j, "month.agb.lai.local"] <- month.lai.min
            
            recov.df[j, "delta.temp"] <- New.temp - OG.temp
            
            recov.df[j, "delta.temp.end"] <- End.temp - OG.temp
            
            
              if(recov.df[j, "flag.2sig"] == T){
                recov.df[j, "recov.Date"] <- as.Date(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                  df$Date > month.agb.min & #Making sure we check after drought has occured
                                                  df$roll.agb >= recov.df[j, "past.agb.mean"] -2*recov.df[j, "past.agb.sd"], "Date"][1])  #Choosing the first date above the original
                
                #If it never recovers then we look at the lowest point from the drought to the end of the model
                if(is.na(recov.df[j, "recov.Date"])){
                  recov.df[j, "agb.min.ultimate"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                       df$Date >= recov.df$D.start & df$Date <= max(df$Date), "roll.agb"])
                  
                  recov.df[j, "month.agb.min.ultimate"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                     df$Date >= recov.df$D.start & df$Date <= max(df$Date) & df$roll.agb == recov.df[j, "agb.min.ultimate"],"Date"]
                  
                  recov.df[j, "agb.min.recov"] <- NA
                  
                  recov.df[j, "month.agb.min.recov"] <- as.Date(NA)
                  
                  #I've removed these for now becuase they don't have a good point of comparision with the scenarios where recovery does happen
                  #Ive kept one as a reference point in case I bring this back
                  
                  #recov.df[j, "recov.soil.moist.mean"] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                  #                                             df$Date >= month.agb.min & df$Date <= max(df$Date), "soil.moist.deep"])

                  
                } else if(is.na(recov.df[j, "recov.Date"])==F){ #If it does recoer we look at the lowest point before recovery
                  
                  #This is to make looper over other parts easier
                  recov.df[j, "month.agb.min.recov"] <- as.Date(NA)
                  
                  
                  #Ultimate means the lowest point from the end of this drought to the end of the model run
                  recov.df[j, "agb.min.ultimate"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                df$Date >= recov.df$D.start & df$Date <= max(df$Date), "roll.agb"])
                  
                  recov.df[j, "month.agb.min.ultimate"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                              df$Date >= recov.df$D.start & df$Date <= max(df$Date) & df$roll.agb == recov.df[j, "agb.min.ultimate"],"Date"]
                  
                  #Recov means the lowest point before recovery begins
                  recov.df[j, "agb.min.recov"] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                          df$Date >= recov.df$D.start & df$Date < recov.df[j, "recov.Date"], "roll.agb"])
                  
                  recov.df[j, "month.agb.min.recov"] <- df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                df$Date >= recov.df$D.start & df$Date <= recov.df[j, "recov.Date"] & df$roll.agb == recov.df[j, "agb.min.recov"],"Date"]
                  
                  
                  #Structural metrics to compare to the past. Can see how much they hae changed since the drought ended
                  
                  for(VAR in ED.interest){
                    recov.df[j, paste("recov", VAR ,"mean", sep = ".")] <- mean(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                           df$Date >= recov.df$D.start & df$Date <= recov.df[j, "recov.Date"], VAR])
                    
                    recov.df[j, paste("recov", VAR ,"max", sep = ".")] <- max(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                         df$Date >= recov.df$D.start & df$Date <= recov.df[j, "recov.Date"], VAR])
                    
                    recov.df[j, paste("recov", VAR ,"min", sep = ".")] <- min(df[df$Management == recov.df[j, "Management"] & #Matching management
                                                                         df$Date >= recov.df$D.start & df$Date <= recov.df[j, "recov.Date"], VAR])
                  }

                }
              } else { #Flagging situations were there isn't a signifgant effect of drought
                recov.df[j, "recov.Date"] <- as.Date(NA)
                recov.df[j, "month.agb.min.recov"] <- as.Date(NA)
                recov.df[j, "agb.min.ultimate"] <- NA
                recov.df[j, "agb.min.recov"] <- NA
            }
          }
          #Counting how many days it takes for recovery after the drought ends
          recov.df$days_of_descent <- difftime(recov.df$month.agb.min.recov, recov.df$D.end, units = "days")
          recov.df$days_of_ascent <- difftime(recov.df$recov.Date, recov.df$month.agb.min.recov, units = "days")
          recov.df$days_of_recovery_str <- difftime(recov.df$recov.Date, recov.df$D.start, units = "days")
          recov.df$days_of_recovery_end <- difftime(recov.df$recov.Date, recov.df$D.end, units = "days")

          #Calculating the difference and change over the first 60 months
          recov.df$agb.local.diff <-  recov.df$agb.min.local - recov.df$past.agb.mean
          
          recov.df$agb.pcent.local.diff <- (recov.df$agb.min.local/recov.df$past.agb.mean - 1) * 100
          
          #Calculating the difference and change over the entire drought months
          recov.df$agb.ultimate.diff <-  recov.df$agb.min.ultimate - recov.df$past.agb.mean
          
          recov.df$agb.pcent.ultimate.diff <- (recov.df$agb.min.ultimate/recov.df$past.agb.mean - 1) * 100
          
          #Calculating the difference and change over the recovery period
          recov.df$agb.recov.diff <-  recov.df$agb.min.recov - recov.df$past.agb.mean
          
          recov.df$agb.pcent.recov.diff <- (recov.df$agb.min.recov/recov.df$past.agb.mean - 1) * 100
        
          
          #Calculating 10 year averages of structural response variables
          for(VAR in ED.interest){
          recov.df[, paste("past", VAR ,"mean", sep = ".")] <- aggregate(eval(as.symbol(VAR))~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                                FUN = mean)[,c("eval(as.symbol(VAR))")]
          
          recov.df[, paste("past", VAR ,"max", sep = ".")] <- aggregate(eval(as.symbol(VAR))~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                                FUN = max)[,c("eval(as.symbol(VAR))")]
          
          recov.df[, paste("past", VAR ,"min", sep = ".")] <- aggregate(eval(as.symbol(VAR))~Management, data = df[df$Date >= min(df$Date) & df$Date <= (min(df$Date) %m+% years(10)) ,],
                                                                FUN = min)[,c("eval(as.symbol(VAR))")]
          
          }

          #In between the end of the drought (rain begins again) and the recovery date what is the local minimum
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]] <- recov.df
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$days_since_rain <- temp.end[i, "months_of_drought"]
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$GCM <- as.factor(MOD)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$rcp <- as.factor(RCP)
          Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$Dry.period <- paste0(STR," to ", END, sep="")
          #Dates.list[[paste(MOD, RCP, STR, END, sep="-")]]$season <- Season
          }
      }
    }
  }
}
# put this after closing braces of loop
close(pb)

#Despite it's name this dataframe contains all the dry periods. It is an ecological drought if theres a signfigant drop
drought.temp <- dplyr::bind_rows(Dates.list)
drought.temp$Management <- factor(drought.temp$Management, levels = c("None", "Gap", "Shelter", "Under"))
drought.temp$year <- year(drought.temp$D.end)
drought.temp$check.recov <- ifelse(is.na(drought.temp$recov.Date), F, T)

drought.df <- data.frame()
#Making a loop to count previous droughts and dry periods. I'm not checking for overlap of periods just total previous occurences
for(MOD in unique(drought.temp$GCM)){
  for(RCP in unique(drought.temp[drought.temp$GCM == MOD, "rcp"])){
    for(MNG in unique(drought.temp[drought.temp$GCM == MOD & drought.temp$rcp == RCP, "Management"])){
      temp <- drought.temp[drought.temp$GCM == MOD & drought.temp$rcp == RCP & drought.temp$Management == MNG, ]
      temp$prev.dry.period <- (seq.int(nrow(temp))-1)
      drought <- -1
      for(i in 1:nrow(temp)){
        #Using 2 sigs but flagging in case we change in the future
        if(temp[i, "flag.2sig"]){
          drought <- drought + 1
        }
          temp[i, "prev.drought"] <- drought
      }
      drought.df <- rbind(drought.df, temp)  
    }
  }
}
#Removing the negative ones that occur if the first instance isn't a signifigant drought
drought.df$prev.drought <- ifelse(drought.df$prev.drought == -1, 0, drought.df$prev.drought)

drought.df$Growing <- ifelse(month(drought.df$D.start) < 5 | month(drought.df$D.start) > 11, F, T)

write.csv(drought.df, "../Resilience_dataframe_test.csv", row.names = F)
