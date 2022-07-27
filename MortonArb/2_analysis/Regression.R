#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(ggplot2)
library(nlme)

path.reg <- "../figures/regression/"
if(!dir.exists(path.reg)) dir.create(path.reg, recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "agb", sep=""))) dir.create(paste(path.reg, "agb", sep=""), recursive=T, showWarnings = F)
if(!dir.exists(paste(path.reg, "lai", sep=""))) dir.create(paste(path.reg, "lai", sep=""), recursive=T, showWarnings = F)

dat.precip <- read.csv("../Precip_Weather_Daily.csv")

dat.precip$year <- lubridate::year(dat.precip$Date)

dat.precip <- dat.precip[!is.na(dat.precip$mean),]

dat.precip$no.rain <- ifelse(dat.precip$sum == 0, 1 ,0)

dat.year <- aggregate(sum~year+model+scenario, dat.precip, FUN = sum)

dat.year$rainless.days <- aggregate(no.rain~year+model+scenario, dat.precip, FUN = sum)[, "no.rain"]

path.read <- "../data/"

runs.all <- read.csv(paste0(path.read, "All_runs.csv"))

runs.all$Management <- car::recode(runs.all$Management, "'MgmtNone'='None'; 'MgmtGap'='Gap'; 'MgmtShelter'='Shelter'; 'MgmtUnder'='Under'")

runs.year <- aggregate(cbind(tair, VPD, agb, lai, npp,soil.moist.deep, soil.moist.surf ,
                             density.tree, height.sd, height.mean, dbh.mean, dbh.sd)~year+Management+GCM+rcp, runs.all, FUN = mean)

runs.comb <- merge(runs.year, dat.year, by.x = c("year", "GCM", "rcp"), by.y = c("year", "model", "scenario"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))


for(i in 2:nrow(runs.comb)){
  #Difference between current year and previous year
  GCM <- runs.comb[i, "GCM"]
  rcp <- runs.comb[i, "rcp"]
  MNG <- runs.comb[i, "Management"]
  Year <- runs.comb[i, "year"]
  
  mean.precip <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "sum"])
  mean.VPD <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "VPD"])
  mean.tair <- mean(runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year <2018, "tair"])
  
  
  runs.comb[i, "rel.precip"] <- (runs.comb[i, "sum"] - mean.precip)/mean.precip
  runs.comb[i, "rel.VPD"] <- (runs.comb[i, "VPD"] - mean.VPD)/mean.VPD
  runs.comb[i, "rel.tair"] <- (runs.comb[i, "tair"] - mean.tair)/mean.tair
  
  if(Year > 2006 & Year < 2099){
  
  runs.comb[i, "diff.precip"] <- (runs.comb[i, "sum"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "sum"]
    
  runs.comb[i, "agb.diff"] <- runs.comb[i, "agb"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"]  
  
  runs.comb[i, "agb.rel.diff"] <- (runs.comb[i, "agb"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "agb"] 
  
  runs.comb[i, "agb.rel.lag1"] <- (runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"] - runs.comb[i, "agb"])/runs.comb[i, "agb"]  
  
  #Shifting agb up a year for easier regression
  runs.comb[i, "future.agb"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "agb"]
  
  runs.comb[i, "lai.diff"] <- runs.comb[i, "lai"] - runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
  
  runs.comb[i, "lai.rel.diff"] <- (runs.comb[i, "lai"]- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"])/runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year-1, "lai"] 
  
  runs.comb[i, "lai.lag"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "lai"] - runs.comb[i, "lai"] 
  
  #Shifting agb up a year for easier regression
  runs.comb[i, "future.lai"] <- runs.comb[runs.comb$GCM==GCM & runs.comb$rcp == rcp & runs.comb$Management==MNG & runs.comb$year == Year+1, "lai"]
  }
  
}

runs.late <- runs.comb[runs.comb$year >= 2025, ]

write.csv(runs.late, paste0(path.read, "All_runs_yearly.csv"))

ggplot(data=runs.late)+
  geom_point(aes(x=rel.precip, y = agb.rel.lag1, color = Management))


#---------------------------------------------------------------#
#Summary figures for soil moisture at the deep and surface level
#---------------------------------------------------------------#

#deep soil moisture by Management
png(file.path(paste(path.reg, "deepsoil_by_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=Management, y = soil.moist.deep, color = Management))+
  geom_boxplot(aes(x=Management, y = soil.moist.deep))+
  ggtitle("Deep soil moisture by Management")
dev.off()

#surface soil moisture by Management
png(file.path(paste(path.reg, "surfacesoil_by_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=Management, y = soil.moist.surf, color = Management))+
  geom_boxplot(aes(x=Management, y = soil.moist.surf))+
  ggtitle("Surface soil moisture by Management")
dev.off()

#---------------------------------------------------------------#
#Looking directly at rain's impact
#---------------------------------------------------------------#

#AGB vs total precipitation in year
png(file.path(paste(path.reg, "agb/agb_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = agb, color = Management))+
  geom_smooth(aes(x=sum, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ total precip * Management")
dev.off()

#AGB vs number of rainless days in year
png(file.path(paste(path.reg, "agb/agb_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = agb, color = Management))+
  geom_smooth(aes(x=rainless.days, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ total precip * Management")
dev.off()

#Delta AGB vs total precipitation in year
png(file.path(paste(path.reg, "agb/delta_agb_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = agb.diff, color = Management))+
  geom_smooth(aes(x=sum, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ total precip * Management")
dev.off()

#Delta AGB vs number of rainless days in year
png(file.path(paste(path.reg, "agb/delta_agb_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = agb.diff, color = Management))+
  geom_smooth(aes(x=rainless.days, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ rainless days * Management")
dev.off()

#Future AGB vs total precipitation in year
png(file.path(paste(path.reg, "agb/future_agb_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = future.agb, color = Management))+
  geom_smooth(aes(x=sum, y = future.agb, color = Management), method='lm', formula= y~x)+
  ggtitle("future agb ~ total precip * Management")
dev.off()

#Future AGB vs number of rainless days in year
png(file.path(paste(path.reg, "agb/future_agb_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = future.agb, color = Management))+
  geom_smooth(aes(x=rainless.days, y = future.agb, color = Management), method='lm', formula= y~x)+
  ggtitle("future agb ~ rainless days * Management")
dev.off()

#---------------------------------------------------------------------------#
# AGB as the response
#---------------------------------------------------------------------------#
#Seeing if Management has an effect on the change in agb
pred.Vars <- c("tair", "VPD", "soil.moist.deep", "soil.moist.surf")

dry.list <- list()
for(COL in pred.Vars){
    
  lm.test <- lme(eval(substitute(agb ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late)
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("agb", "~", COL,"*Management")
  dry.list[[paste("agb ~ ", COL)]]$Var <- "agb"
  dry.list[[paste("agb ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("agb ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("agb ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("agb ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
  
  lm.test <- lme(eval(substitute(agb.diff ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late)
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("agb.diff", "~", COL,"*Management")
  dry.list[[paste("agb.diff ~ ", COL)]]$Var <- "agb.diff"
  dry.list[[paste("agb.diff ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("agb.diff ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("agb.diff ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("agb.diff ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
  
  lm.test <- lme(eval(substitute(future.agb ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late[!is.na(runs.late$future.agb),])
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("future.agb", "~", COL,"- 1")
  dry.list[[paste("future.agb ~ ", COL)]]$Var <- "future.agb"
  dry.list[[paste("future.agb ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("future.agb ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("future.agb ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("future.agb ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
}

dat.agb <- dplyr::bind_rows(dry.list)

dat.agbsig <- dat.agb[dat.agb$pvalue <= .05,]

#----------------------------------------------------------#
# Here we make figures using agb
#----------------------------------------------------------#

#AGB vs deep soil moisture
png(file.path(paste(path.reg, "agb/agb_vs_deepsoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.deep, y = agb, color = Management))+
  geom_smooth(aes(x=soil.moist.deep, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ deep soil moisture * Management")
dev.off()

#AGB vs surface soil moisture
png(file.path(paste(path.reg, "agb/agb_vs_surfacesoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.surf, y = agb, color = Management))+
  geom_smooth(aes(x=soil.moist.surf, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ surface soil moisture * Management")
dev.off()

#AGB vs VPD
png(file.path(paste(path.reg, "agb/agb_vs_VPD_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=VPD, y = agb, color = Management))+
  geom_smooth(aes(x=VPD, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ VPD * Management")
dev.off()

#AGB vs air temperature
png(file.path(paste(path.reg, "agb/agb_vs_airtemp_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=tair, y = agb, color = Management))+
  geom_smooth(aes(x=tair, y = agb, color = Management), method='lm', formula= y~x)+
  ggtitle("agb ~ Mean air temp * Management")
dev.off()

#--------------------------------------------------------#
#Figures working with delta agb
#--------------------------------------------------------#

#Delta AGB vs deep soil moisture
png(file.path(paste(path.reg, "agb/delta_agb_vs_deepsoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.deep, y = agb.diff))+
  geom_smooth(aes(x=soil.moist.deep, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ deep soil moisture * Management")
dev.off()

#Delta AGB vs surface soil moisture
png(file.path(paste(path.reg, "agb/delta_agb_vs_surfacesoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.surf, y = agb.diff))+
  geom_smooth(aes(x=soil.moist.surf, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ surface soil moisture * Management")
dev.off()

#Delta AGB vs VPD
png(file.path(paste(path.reg, "agb/delta_agb_vs_VPD_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=VPD, y = agb.diff))+
  geom_smooth(aes(x=VPD, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ VPD * Management")
dev.off()

#Delta AGB vs air temperature
png(file.path(paste(path.reg, "agb/delta_agb_vs_airtemp_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=tair, y = agb.diff))+
  geom_smooth(aes(x=tair, y = agb.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta agb ~ deep soil moisture * Management")
dev.off()


#---------------------------------------------------------------------------#
# LAI now as the response
#---------------------------------------------------------------------------#

dry.list <- list()
for(COL in pred.Vars){
  
  lm.test <- lme(eval(substitute(lai ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late)
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("lai", "~", COL,"*Management")
  dry.list[[paste("lai ~ ", COL)]]$Var <- "lai"
  dry.list[[paste("lai ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("lai ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("lai ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("lai ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
  
  lm.test <- lme(eval(substitute(lai.diff ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late)
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("lai.diff", "~", COL,"*Management")
  dry.list[[paste("lai.diff ~ ", COL)]]$Var <- "lai.diff"
  dry.list[[paste("lai.diff ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("lai.diff ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("lai.diff ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("lai.diff ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
  
  lm.test <- lme(eval(substitute(future.lai ~ j*Management, list(j = as.name(COL)))), random=list(GCM=~1, rcp=~1), data = runs.late[!is.na(runs.late$future.lai),])
  summary(lm.test)
  anova(lm.test)
  
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste("future.lai", "~", COL,"*Management")
  dry.list[[paste("future.lai ~ ", COL)]]$Var <- "future.lai"
  dry.list[[paste("future.lai ~ ", COL)]]$Equation <- df.eff$Equation
  dry.list[[paste("future.lai ~ ", COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste("future.lai ~ ", COL)]]$Value <- df.eff$Value
  dry.list[[paste("future.lai ~ ", COL)]]$pvalue <- df.eff$`p-value`
  
}

dat.leaf <- dplyr::bind_rows(dry.list)

dat.lai <- dat.leaf[dat.leaf$pvalue <= .05,]

#---------------------------------------------------------------#
#Looking directly at rain's impact
#---------------------------------------------------------------#

#lai vs total precipitation in year
png(file.path(paste(path.reg, "lai/lai_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = lai, color = Management))+
  geom_smooth(aes(x=sum, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ total precip * Management")
dev.off()

#lai vs number of rainless days in year
png(file.path(paste(path.reg, "lai/lai_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = lai, color = Management))+
  geom_smooth(aes(x=rainless.days, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ total precip * Management")
dev.off()

#Delta lai vs total precipitation in year
png(file.path(paste(path.reg, "lai/delta_lai_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = lai.diff, color = Management))+
  geom_smooth(aes(x=sum, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ total precip * Management")
dev.off()

#Delta lai vs number of rainless days in year
png(file.path(paste(path.reg, "lai/delta_lai_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = lai.diff, color = Management))+
  geom_smooth(aes(x=rainless.days, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ rainless days * Management")
dev.off()

#Future lai vs total precipitation in year
png(file.path(paste(path.reg, "lai/future_lai_vs_totalprecip_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=sum, y = future.lai, color = Management))+
  geom_smooth(aes(x=sum, y = future.lai, color = Management), method='lm', formula= y~x)+
  ggtitle("future lai ~ total precip * Management")
dev.off()

#Future lai vs number of rainless days in year
png(file.path(paste(path.reg, "lai/future_lai_vs_rainlessdays_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=rainless.days, y = future.lai, color = Management))+
  geom_smooth(aes(x=rainless.days, y = future.lai, color = Management), method='lm', formula= y~x)+
  ggtitle("future lai ~ rainless days * Management")
dev.off()

#----------------------------------------------------------#
# Here we make figures using lai
#----------------------------------------------------------#

#lai vs deep soil moisture
png(file.path(paste(path.reg, "lai/lai_vs_deepsoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.deep, y = lai, color = Management))+
  geom_smooth(aes(x=soil.moist.deep, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ deep soil moisture * Management")
dev.off()

#lai vs surface soil moisture
png(file.path(paste(path.reg, "lai/lai_vs_surfacesoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.surf, y = lai, color = Management))+
  geom_smooth(aes(x=soil.moist.surf, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ surface soil moisture * Management")
dev.off()

#lai vs VPD
png(file.path(paste(path.reg, "lai/lai_vs_VPD_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=VPD, y = lai, color = Management))+
  geom_smooth(aes(x=VPD, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ VPD * Management")
dev.off()

#lai vs air temperature
png(file.path(paste(path.reg, "lai/lai_vs_airtemp_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=tair, y = lai, color = Management))+
  geom_smooth(aes(x=tair, y = lai, color = Management), method='lm', formula= y~x)+
  ggtitle("lai ~ Mean air temp * Management")
dev.off()

#--------------------------------------------------------#
#Figures working with delta lai
#--------------------------------------------------------#

#Delta lai vs deep soil moisture
png(file.path(paste(path.reg, "lai/delta_lai_vs_deepsoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.deep, y = lai.diff))+
  geom_smooth(aes(x=soil.moist.deep, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ deep soil moisture * Management")
dev.off()

#Delta lai vs surface soil moisture
png(file.path(paste(path.reg, "lai/delta_lai_vs_surfacesoil_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=soil.moist.surf, y = lai.diff))+
  geom_smooth(aes(x=soil.moist.surf, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ surface soil moisture * Management")
dev.off()

#Delta lai vs VPD
png(file.path(paste(path.reg, "lai/delta_lai_vs_VPD_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=VPD, y = lai.diff))+
  geom_smooth(aes(x=VPD, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ VPD * Management")
dev.off()

#Delta lai vs air temperature
png(file.path(paste(path.reg, "lai/delta_lai_vs_airtemp_Management.png", sep = "")))
ggplot(runs.late)+
  geom_point(aes(x=tair, y = lai.diff))+
  geom_smooth(aes(x=tair, y = lai.diff, color = Management), method='lm', formula= y~x)+
  ggtitle("delta lai ~ deep soil moisture * Management")
dev.off()

#---------------------------------------------------------------------#
# Attempting to check multiple sturctucal variables against our weather metrics

pred.Vars <- c("tair", "VPD", "soil.moist.deep", "soil.moist.surf")
struc.Vars <- c("density.tree", "height.sd", "height.mean", "dbh.mean", "dbh.sd")

dry.list <- list()
for(COL in pred.Vars){
  for(STRUC in struc.Vars){
    lm.test <- lme(eval(substitute(i ~ j*Management, list(j = as.name(COL), i = as.name(STRUC)))), random=list(GCM=~1, rcp=~1), data = runs.late)
    summary(lm.test)
    anova(lm.test)
    
    sum <- summary(lm.test)
    df.eff <- as.data.frame(sum$tTable)
    df.eff$Fixedeff <- rownames(df.eff)
    df.eff$Equation <- paste(STRUC, "~", COL,"*Management")
    dry.list[[paste(STRUC, "~", COL)]]$Var <- STRUC
    dry.list[[paste(STRUC, "~", COL)]]$Equation <- df.eff$Equation
    dry.list[[paste(STRUC, "~", COL)]]$Fixedeff <- df.eff$Fixedeff
    dry.list[[paste(STRUC, "~", COL)]]$Value <- df.eff$Value
    dry.list[[paste(STRUC, "~", COL)]]$pvalue <- df.eff$`p-value`
  }
}

dat.struc <- dplyr::bind_rows(dry.list)

dat.strucsig <- dat.struc[dat.struc$pvalue <= .05,]


write.csv(dat.struc, "../tables/Structural_variables_vs_Management.csv")
write.csv(dat.agb, "../tables/Weather_variables_vs_AGB.csv")
write.csv(dat.struc, "../tables/Weather_variables_vs_LAI.csv")
