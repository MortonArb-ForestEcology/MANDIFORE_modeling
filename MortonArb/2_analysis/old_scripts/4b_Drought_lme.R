#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore Morton arb casee study
# Purpose: This script crates plots and tables exploring drought periods impact on variables of interest
# Inputs: Drought and preicpitaiton dataframes from 1a_Drought_Analysis
# Outputs:
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(nlme)
library(ggplot2)
library(lubridate)

drought.df <- read.csv("../Resilience_dataframe_test.csv")

drought.df$Management <- factor(drought.df$Management, levels = c("None", "Gap", "Shelter", "Under"))

#drought.df$Growing <- ifelse(month(drought.df$D.start) < 4 | month(drought.df$D.start) > 11, "Dormant", "Growing")

#Checking if the days since rain plays a role in whether a response to drought occurs
response.df <- data.frame()
for(SEA in unique(drought.df$Growing)){
  rain <- as.data.frame(table(drought.df[drought.df$flag.2sig & drought.df$Growing == SEA , "days_since_rain"]))
  check <- as.data.frame(table(drought.df[drought.df$Growing == SEA , "days_since_rain"]))
  check$Growing <- SEA
  check$sig <- rain$Freq[match(check$Var1, rain$Var1)]
  check$res.pcent <- (check$sig/check$Freq) *100
  check$res.pcent <- ifelse(is.na(check$res.pcent), 0, check$res.pcent)
  response.df <- rbind(check, response.df)
}
colnames(response.df) <- c("days", "Freq", "Growing", "sig", "res.pcent")

png(file.path('../figures', 'Growing_season_vs_response.png'))
ggplot(response.df)+
  geom_point(aes(x=days, y = res.pcent))+
  xlab("months low rain")+
  ylab("Percentage of ecological response")+
  ggtitle("Months of low rain vs % drought occurence")+
  scale_x_discrete(guide = guide_axis(check.overlap = T))+
  geom_vline(xintercept = 14)
dev.off()


#Now that we have checked with use our cutoff of 14 days and only growing season
drought.temp <- drought.df
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

#Creating figure looking at percentage of response by previous droughts experienced
rain <- as.data.frame(table(drought.df[drought.df$flag.2sig , "prev.drought"]))
check <- as.data.frame(table(drought.df$prev.drought))
check$sig <- rain$Freq[match(check$Var1, rain$Var1)]
check$res.pcent <- (check$sig/check$Freq) * 100
check$res.pcent <- ifelse(is.na(check$res.pcent), 0, check$res.pcent)

png(file.path('../figures', 'Previous_drought_vs_response.png'))
ggplot(check)+
  geom_point(aes(x=Var1, y = res.pcent))+
  xlab("Number of previous signifigant droughts")+
  ylab("Percentage of ecological response")+
  ggtitle("Number of previous droughts vs % drought occurence")+
  scale_x_discrete(guide = guide_axis(check.overlap = T))
dev.off()

#Creating figure looking at percentage of response by previous droughts experienced
rain <- as.data.frame(table(drought.df[drought.df$flag.2sig , "prev.dry.period"]))
check <- as.data.frame(table(drought.df$prev.dry.period))
check$sig <- rain$Freq[match(check$Var1, rain$Var1)]
check$res.pcent <- (check$sig/check$Freq) * 100
check$res.pcent <- ifelse(is.na(check$res.pcent), 0, check$res.pcent)

png(file.path('../figures', 'Previous_dry_period_vs_response.png'))
ggplot(check)+
  geom_point(aes(x=Var1, y = res.pcent))+
  xlab("Number of previous dry years")+
  ylab("Percentage of ecological response")+
  ggtitle("Number of previous dry years vs % drought occurence")+
  scale_x_discrete(guide = guide_axis(check.overlap = T))
dev.off()


#Seeing if Management has an effect on the change in agb
Dry.variables <- c("agb.local.diff", #The difference in the lowest dip in agb over 15 months after drought
                   "agb.pcent.local.diff") #The percentage difference in the lowest dip in agb over 15 months after drought

for(COL in Dry.variables){
  dry.list <- list()
  lm.test <- lme(eval(substitute(j ~ Management-1, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1, year=~1), data = drought.df)
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(COL, "~", "Management - 1")
  dry.list[[paste(COL, " -1")]]$Var <- COL
  dry.list[[paste(COL, " -1")]]$Equation <- df.eff$Equation
  dry.list[[paste(COL, " -1")]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste(COL, " -1")]]$Value <- df.eff$Value
  dry.list[[paste(COL, " -1")]]$pvalue <- df.eff$`p-value`
  #One set is for the general signifigance and this set is for sigfignace relative to no management
  lm2.test <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1, year=~1), data = drought.df)
  sum <- summary(lm2.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(COL, "~", "Management")
  dry.list[[paste(COL)]]$Var <- COL
  dry.list[[paste(COL)]]$Equation <- df.eff$Equation
  dry.list[[paste(COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste(COL)]]$Value <- df.eff$Value
  dry.list[[paste(COL)]]$pvalue <- df.eff$`p-value`
  dat.dry <- dplyr::bind_rows(dry.list)
  write.csv(dat.dry, file.path(paste("../Tables/",COL ,"_vs_Mangement.csv", sep = "")), row.names = F)
  
  png(file.path(paste("../figures/", COL, "_vs_Management.png", sep = "")))
  ggplot(drought.df)+
    geom_boxplot(aes(x=Management, y =eval(as.symbol(COL)) , fill = Management)) +
    #stat_smooth(aes(x=eval(as.symbol(COL)), y= Management, color = Management), method="lm", alpha=0.2) +
    ggtitle(paste(COL, "_vs_Management", sep=""))+
    ylab(COL)
  dev.off()
}


#Creating a table checking if management has an effect on ecological response to drought
dry.list <- list()
#lm.test <- lme4::glmer(flag.2sig ~ Management-1 + (1|GCM/rcp), family = binomial, data = drought.df[drought.df$days_since_rain >= 14 & drought.df$Growing == "Growing",])
lm.test <- lme4::glmer(flag.2sig ~ Management-1 + (1|GCM/rcp), family = binomial, data = drought.df)
#lm.test <- lme4::glmer(flag.2sig ~ GCM*Management-1 + (1|year) , family = binomial, data = drought.df)
sum <- summary(lm.test)
df.eff <-  as.data.frame(sum$coefficients)
colnames(df.eff) <- c("Value", "Std.Error", "z value", "p-value")
df.eff$Fixedeff <- rownames(df.eff)
df.eff$Equation <- paste("flag.2sig ~ Management - 1 + (1|Dry.period)")
dry.list[[paste("flag.2sig", " -1")]]$Var <- "flag.2sig"
dry.list[[paste("flag.2sig", " -1")]]$Equation <- df.eff$Equation
dry.list[[paste("flag.2sig", " -1")]]$Fixedeff <- df.eff$Fixedeff
dry.list[[paste("flag.2sig", " -1")]]$Value <- plogis(df.eff$Value)
dry.list[[paste("flag.2sig", " -1")]]$pvalue <- df.eff$`p-value`

df.eff$Fixedeff <- factor(df.eff$Fixedeff, levels = c("ManagementNone", "ManagementGap", "ManagementShelter", "ManagementUnder"))
png(file.path('../figures', 'Management_vs_response.png'))
ggplot(df.eff)+
  geom_point(aes(x=Fixedeff, y = plogis(Value)))+
  geom_errorbar(aes(x= Fixedeff, y = plogis(Value), ymin =plogis(Value) - plogis(Std.Error), ymax =plogis(Value) + plogis(Std.Error)))+
  xlab("Management")+
  ylab("Percentage of ecological response")+
  ggtitle("Management_vs_%_drought_occurence")+
  scale_x_discrete(guide = guide_axis(check.overlap = T))
dev.off()

#lm2.test <- lme4::glmer(flag.2sig ~ Management + (1|GCM/rcp), family = binomial, data = drought.df[drought.df$days_since_rain >= 14 & drought.df$Growing == "Growing",])
lm2.test <- lme4::glmer(flag.2sig ~ Management + (1|GCM/rcp), family = binomial, data = drought.df)
#lm2.test <- lme4::glmer(flag.2sig ~ GCM*Management + (1|Dry.period), family = binomial, data = drought.df[drought.df$days_since_rain >= 14 & drought.df$Growing == "Growing",])
sum <- summary(lm2.test)
df.eff <-  as.data.frame(sum$coefficients)
colnames(df.eff) <- c("Value", "Std.Error", "z value", "p-value")
df.eff$Fixedeff <- rownames(df.eff)
df.eff$Equation <- paste("flag.2sig", "~", "Management + (1|Dry.period)")
dry.list[[paste("flag.2sig")]]$Var <- "flag.2sig"
dry.list[[paste("flag.2sig")]]$Equation <- df.eff$Equation
dry.list[[paste("flag.2sig")]]$Fixedeff <- df.eff$Fixedeff
dry.list[[paste("flag.2sig")]]$Value <- plogis(df.eff$Value)
dry.list[[paste("flag.2sig")]]$pvalue <- df.eff$`p-value`
dat.dry <- dplyr::bind_rows(dry.list)
dat.dry
write.csv(dat.dry, file.path("../Tables/Response_vs_Mangement.csv"), row.names = F)

#Creating figure looking at percentage of response by Management
rain <- as.data.frame(table(drought.df[drought.df$flag.2sig , "Management"]))
check <- as.data.frame(table(drought.df$Management))
check$sig <- rain$Freq[match(check$Var1, rain$Var1)]
check$res.pcent <- (check$sig/check$Freq) *100
check$res.pcent <- ifelse(is.na(check$res.pcent), 0, check$res.pcent)

#This is for the creation of a demonstrative figure showcasing how we define the response period and historic variability
#This takes a clean example case and creates a figure to demonstrate

path.read <- "C:/Users/lucie/Documents/MANDIFORE/"

setwd(path.read)
library(readbulk)
runs.all <- read_bulk(directory = "output", extension = "Site.csv", header = TRUE)

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

#artifically adding the 1st as the day for the Date objects since you can't make a date object with just month and year
#This is used for plotting not for direct date comparision
runs.all$Date <- lubridate::ymd(paste(runs.all$year, runs.all$month, "01", sep = "-"))

sig <- drought.df[drought.df$flag.2sig,]
sig2 <- sig[sig$D.start == as.Date("2044-12-10"),]
runs.new <- runs.all[runs.all$Date >= as.Date(sig2$D.start) %m-% years(15) & runs.all$Date <= as.Date(sig2$recov.Date) %m+% years(5) 
                     & runs.all$GCM == sig2$GCM & runs.all$rcp == sig2$rcp, ]


sig2 <- drought.df[drought.df$D.start == as.Date("2044-12-10"),]
runs.merge <- merge(sig2, runs.new, by= "Management")

setwd("C:/Users/lucie/Documents/GitHub/MANDIFORE_modeling/MortonArb/2_analysis/")

#This figure is just to show one example case. This is only the no management scenario
png(file.path('../figures', 'Sframe_Simple_definitions_explained.png'),width = 720, height = 720)
ggplot(runs.merge[runs.merge$Date <= "2048-01-01" & runs.merge$Management == "None",])+
  geom_line(aes(x=Date, y = agb))+
  xlab("Time")+
  ylab("agb")+
  #geom_hline(aes(yintercept = (past.agb.mean), linetype = 1)) +
  geom_rect(aes(xmin = as.Date(min(Date)), xmax = as.Date(max(Date)), ymin = (past.agb.mean - (2*past.agb.sd)), ymax = (past.agb.mean - (2*past.agb.sd) + .005)), fill = "#D55E00", alpha = 0.1)+
  #Rectangle of the drought
  geom_rect(aes(xmin = as.Date(D.start), xmax = as.Date(D.end), ymin = -Inf, ymax = Inf), fill = "#CC79A7", alpha = 0.4)+
  #Rectangle of the agb checking period
  geom_rect(aes(xmin = as.Date(D.start) %m-% years(10), xmax = as.Date(D.start), ymin = (past.agb.mean - (2*past.agb.sd)) , ymax = (past.agb.mean + (2*past.agb.sd))), fill = "#56B4E9", alpha = 0.01)+
  #Rectanlge of the 15 month response period
  geom_rect(aes(xmin = as.Date(D.end), xmax = as.Date(D.end) %m+% months(15), ymin = -Inf , ymax = Inf), fill = "#009E73", alpha = 0.01)+
  theme(legend.position="none")
dev.off()


#This uses every Management to show the difference when one scenario reacts
png(file.path('../figures', 'Simple_definitions_explained.png'),width = 720, height = 720)
ggplot(runs.merge[runs.merge$Date <= "2048-01-01",])+
  facet_wrap(~Management)+
  geom_line(aes(x=Date, y = agb))+
  xlab("Time")+
  ylab("agb")+
  #geom_hline(aes(yintercept = (past.agb.mean), linetype = 1)) +
  geom_rect(aes(xmin = as.Date(min(Date)), xmax = as.Date(max(Date)), ymin = (past.agb.mean - (2*past.agb.sd)), ymax = (past.agb.mean - (2*past.agb.sd) + .005)), fill = "#D55E00", alpha = 0.1)+
  #Rectangle of the drought
  geom_rect(aes(xmin = as.Date(D.start), xmax = as.Date(D.end), ymin = -Inf, ymax = Inf), fill = "#CC79A7", alpha = 0.4)+
  #Rectangle of the agb checking period
  geom_rect(aes(xmin = as.Date(D.start) %m-% years(10), xmax = as.Date(D.start), ymin = (past.agb.mean - (2*past.agb.sd)) , ymax = (past.agb.mean + (2*past.agb.sd))), fill = "#56B4E9", alpha = 0.01)+
  #Rectanlge of the 15 month response period
  geom_rect(aes(xmin = as.Date(D.end), xmax = as.Date(D.end) %m+% months(15), ymin = -Inf , ymax = Inf), fill = "#009E73", alpha = 0.01)+
  theme(legend.position="none")
dev.off()


#This version includes the recovery period incase we want to show and talk about that
png(file.path('../figures', 'Definitions_explained.png'),width = 720, height = 720)
ggplot(runs.merge[runs.merge$Management == "None",])+
  geom_line(aes(x=Date, y = agb))+
  xlab("Time")+
  ylab("agb")+
  geom_vline(xintercept = as.Date("2055-05-01"), linetype = 4) +
  #geom_hline(aes(yintercept = (past.agb.mean), linetype = 1)) +
  geom_rect(aes(xmin = as.Date(min(Date)), xmax = as.Date(max(Date)), ymin = (past.agb.mean - (2*past.agb.sd)), ymax = (past.agb.mean - (2*past.agb.sd) + .05)), fill = "#D55E00", alpha = 0.1)+
  #Rectangle of the drought
  geom_rect(aes(xmin = as.Date(D.start), xmax = as.Date(D.end), ymin = -Inf, ymax = Inf), fill = "#CC79A7", alpha = 0.4)+
  #Rectangle of the agb checking period
  geom_rect(aes(xmin = as.Date(D.start) %m-% years(10), xmax = as.Date(D.start), ymin = (past.agb.mean - (2*past.agb.sd)) , ymax = (past.agb.mean + (2*past.agb.sd))), fill = "#56B4E9", alpha = 0.01)+
  #Rectanlge of the 15 month response period
  geom_rect(aes(xmin = as.Date(D.end), xmax = as.Date(D.end) %m+% months(15), ymin = -Inf , ymax = Inf), fill = "#009E73", alpha = 0.01)+
  #Rectanlge of the 15 month recovery period
  geom_rect(aes(xmin = as.Date(D.end) %m+% months(15), xmax = as.Date("2055-05-01"), ymin = -Inf , ymax = (past.agb.mean - (2*past.agb.sd))), fill = "#F0E442", alpha = 0.01)+
  theme(legend.position="none")
dev.off()

library(dplyr)
check <- as.data.frame(table(drought.df$days_since_rain))

bad.ss <- as.vector(check[check$Freq <= 4, "Var1"])

good.ss <- drought.df[!drought.df$days_since_rain %in% as.numeric(bad.ss), ]

response.df <- data.frame()
for(SEA in unique(good.ss$Growing)){
  rain <- as.data.frame(table(good.ss[good.ss$flag.2sig & good.ss$Growing == SEA , "days_since_rain"]))
  check <- as.data.frame(table(good.ss[good.ss$Growing == SEA , "days_since_rain"]))
  check$Growing <- SEA
  check$sig <- rain$Freq[match(check$Var1, rain$Var1)]
  check$res.pcent <- (check$sig/check$Freq) *100
  check$res.pcent <- ifelse(is.na(check$res.pcent), 0, check$res.pcent)
  response.df <- rbind(check, response.df)
}
colnames(response.df) <- c("days", "Freq", "Growing", "sig", "res.pcent")

#png(file.path('../figures', 'Days_since_rain_vs_response.png'))
ggplot(response.df[response.df$Growing == "Growing",])+
  geom_point(aes(x=days, y = res.pcent))+
  xlab("days since rain")+
  ylab("Percentage of ecological response")+
  ggtitle("Days since rain vs % drought response")+
  scale_x_discrete(guide = guide_axis(check.overlap = T))+
  geom_vline(xintercept = 14)
#dev.off()
