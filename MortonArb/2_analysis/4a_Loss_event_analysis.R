#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection, does linear regression, and creates figures
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)
library(ggplot2)
library(ggpubr)
library(multcomp)

path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

runs.comb$RCP.name <- car::recode(runs.comb$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.comb$RCP.name <- factor(runs.comb$RCP.name, levels=c("Low Emmissions", "High Emissions"))

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff.future),]

#-------------------------------------------------------------#
#Counting the number of major agb loss events
#-------------------------------------------------------------#
#agb.extreme <- as.numeric(quantile(runs.late$agb.rel.diff.future, probs = c(.025)))

runs.comb$loss.event.20 <- ifelse(runs.comb$agb.rel.diff <= -.20, T, F)

#Counting individual instances of a crash beginning
for(i in 5:nrow(runs.comb)){
  DRIVE <- runs.comb[i, "Driver.set"]
  MNG <- runs.comb[i, "Management"]
  YR <- runs.comb[i, "year"]
  if(YR != 2007){
    prev.20 <- runs.comb[runs.comb$Driver.set == DRIVE & runs.comb$Management == MNG & runs.comb$year == YR-1 , "loss.event.20"]
    runs.comb[i, "nonseq.loss.event.20"] <- ifelse((runs.comb[i, "loss.event.20"] == T & prev.20 ==F), T, F)
  }
}

#Modified so that red dots are now looking at unique occurences of a crash instead of the total number of years crashing
png("AGB_static_crashes.png", width=16, height=8, units="in", res=220)
ggplot(data=runs.comb)+
  facet_grid(Management ~ RCP.name) +
  geom_rect(xmin=2020, xmax=2024, ymin=-Inf, ymax=Inf, fill="orange3", alpha=0.9) +
  geom_line(aes(x=year, y=agb, group=GCM)) +
  geom_point(data=runs.comb[runs.comb$loss.event.20==T & runs.comb$year>2024 & !is.na(runs.comb$agb.rel.diff),], aes(x=year, y=agb, group=GCM), color="blue2", size=3) +
  geom_point(data=runs.comb[runs.comb$nonseq.loss.event.20==T & runs.comb$year>2024 & !is.na(runs.comb$agb.rel.diff),], aes(x=year, y=agb, group=GCM), color="red2", size=3) +
  labs(x="Year", y="Aboveground Biomass (kgC/m2)") +
  geom_text(x=2025, y=25, label="Harvest Period: 2020-2024", color="orange3", hjust=0) +
  theme(axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))
dev.off()

# Getting summaries of # crashes by scenario by mid & end of century
runs.comb$crash <- ifelse(runs.comb$nonseq.loss.event.20==T, 1, 0)

crash.summary <- aggregate(crash~ GCM + rcp + RCP.name + Management, data=runs.comb[runs.comb$year>=2025 ,], FUN=sum)
names(crash.summary)[names(crash.summary)=="crash"] <- "crash.end"
crash.summary$crash.mid <- aggregate(crash~ GCM + rcp + RCP.name + Management, data=runs.comb[runs.comb$year>=2025 & runs.comb$year<=2050,], FUN=sum)$crash
summary(crash.summary)

library(MASS)
library(lme4)
crash.end <- glm.nb(crash.end~Management*rcp, data = crash.summary)
summary(crash.end)
anova(crash.end)

t.test(crash.summary$crash.mid[crash.summary$Management=="None"], crash.summary$crash.mid[crash.summary$Management=="Shelter"], paired=T)
t.test(crash.summary$crash.mid[crash.summary$Management=="Gap"], crash.summary$crash.mid[crash.summary$Management=="Shelter"], paired=T)
t.test(crash.summary$crash.mid[crash.summary$Management=="Under"], crash.summary$crash.mid[crash.summary$Management=="Shelter"], paired=T)

# t.test(crash.summary$crash.mid[crash.summary$Management=="None"], crash.summary$crash.mid[crash.summary$Management=="Gap"], paired=T)

# TukeyHSD(crash.mid)
# chisq.test(x=crash.summary$crash.mid[crash.summary$Management=="None"], y=crash.summary$crash.mid[crash.summary$Management=="Shelter"])

# Doesn't converge
# crash.end.mm <- glmer.nb(crash.end~Management*rcp + (1|GCM), data = crash.summary)
# summary(crash.end.mm)
crash.mid <- glm.nb(crash.mid~Management*rcp, data = crash.summary)
summary(crash.mid)
anova(crash.mid)

crash.stack <- stack(crash.summary[,c("crash.mid", "crash.end")])
crash.stack[,c("Management", "rcp", "GCM", "RCP.name")] <- crash.summary[,c("Management", "rcp", "GCM", "RCP.name")]
crash.stack$TimePeriod <- car::recode(crash.stack$ind, "'crash.mid'='mid-century'; 'crash.end'='end-century'")
crash.stack$TimePeriod <- factor(crash.stack$TimePeriod, levels=c("mid-century", "end-century"))

# ggplot(data=crash.stack) +
# facet_grid(.~rcp) +
# geom_boxplot(aes(x=ind, y=values, fill=Management))
theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text = element_text(size=rel(2), face="bold"),
                       strip.background = element_rect(fill=NA),
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
                       legend.key = element_rect(fill=NA))

png("../figures/Crashes_Summary.png", width=10, height=8, units="in", res=220)
ggplot(data=crash.stack) +
  facet_grid(TimePeriod~.) +
  geom_bar(aes(x=rcp, y=values, fill=Management), position="dodge", stat="summary", fun.y="mean") +
  geom_errorbar(aes(x=rcp, y=values, fill=Management), position="dodge", stat="summary", fun.y="sd") +
  scale_y_continuous(name="# Unique Crashes", expand=c(0,0)) +
  scale_x_discrete(labels=c("Low\nEmissions", "High\nEmmisions")) +
  coord_cartesian(ylim=c(0,3.99)) +
  scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean + theme(axis.title.x=element_blank(), panel.spacing.y = unit(2, "lines"))
dev.off()


#Counting the duration of the crash events
runs.count <- data.frame()
for(MOD in unique(runs.late$GCM)){
  for(RCP in unique(runs.late$rcp)){
    for(MNG in unique(runs.late$Management)){
      temp <- runs.late[runs.late$GCM == MOD & runs.late$rcp == RCP & runs.late$Management == MNG,]
      count <-0
      f.crash <- F
      for(i in 1:nrow(temp)){
        #Teasing out the duration of the crashes
        if(temp[i, "loss.event.20"] == T & f.crash == F){
          temp[i, "crash.status"] <- "first.crash"
          count <- count + 1
          if(i < 92 & temp[i+1, "loss.event.20"] == F){ #Caveat, if first crash is year 2099 we don't flag. Doesn't present a problem with our data
            f.crash <- T #Where we label that first crash happened
          }
        } else if(temp[i, "loss.event.20"] == F & f.crash == F){
          temp[i, "crash.status"] <- "pre-crash"
          count <- 0
        } else if(temp[i, "loss.event.20"] == F & f.crash == T){
          temp[i, "crash.status"] <- "recovery"
          count <- 0
        } else if(temp[i, "loss.event.20"] == T & f.crash == T){
          temp[i, "crash.status"] <- "subsequent.crash"
          count <- count + 1
        }
        
        temp[i, "years.crash"] <- count
        
      }
      runs.count <- rbind(runs.count, temp)
    }
  }
}

#Calculating the full length of the crashes
for(i in 5:nrow(runs.count)){
  DRIVE <- runs.count[i, "Driver.set"]
  MNG <- runs.count[i, "Management"]
  YR <- runs.count[i, "year"]
  if(YR != 2098){
    post <- runs.count[runs.count$Driver.set == DRIVE & runs.count$Management == MNG & runs.count$year == YR+1 , "loss.event.20"]
    runs.count[i, "full.duration"] <- ifelse((runs.count[i, "loss.event.20"] == T & post ==F), runs.count[i, "years.crash"], 0)
  }else if(YR == 2098){
    runs.count[i, "full.duration"] <- runs.count[i, "years.crash"]
    
  }
}

# -----------------------------------------------------------

#Organizing data into long form for easier graphing.
agg.status <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~GCM+rcp+Driver.set+Management+crash.status, data = runs.comb, FUN = mean, na.action = NULL)

plot.status <- stack(agg.status[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.status) <- c("values", "var")
plot.status[,c("GCM", "rcp", "Driver.set", "Management", "crash.status")] <- agg.status[,c("GCM", "rcp", "Driver.set", "Management", "crash.status")]

plot.status$crash.status <- factor(plot.status$crash.status, levels = c("pre-crash", "first.crash", "recovery", "subsequent.crash"))

#Making a box plot of the variables by crash status
png(width= 750, filename= file.path(path.figures, paste0('Structure_by_crash_status_boxplot.png')))
ggplot(plot.status)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=crash.status, y=values, color = crash.status))+
  ggtitle("Structural variables by crash status")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Making a histogram of the variables by crash status
png(width= 750, filename= file.path(path.figures, paste0('Structure_by_crash_status_histogram.png')))
ggplot(plot.status)+
  facet_wrap(~var, scales = "free")+
  geom_histogram(aes(x=values, color = crash.status, fill = crash.status))+
  ggtitle("Structural variables by crash status")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#----------------------------------------------------------#
#Getting stats for the duration of major crashes
#----------------------------------------------------------#
#Creating a mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

runs.count$full.duration <- as.numeric(runs.count$full.duration)

#General summary stats of duration
#mean median and mode of duration of event
Mean.years <- mean(as.numeric(runs.count[runs.count$full.duration != 0 ,"full.duration"]), na.rm = T)
Median.years <- median(as.numeric(runs.count[runs.count$full.duration != 0 ,"full.duration"]), na.rm = T)
Mode.years <- getmode(as.numeric(runs.count[runs.count$full.duration != 0 ,"full.duration"]))

duration.df <- data.frame(Mean.years, Median.years, Mode.years)
#Seems like 1 year is the median and mode but the mean is just under 2


#Aggregating our stats by Management and rcp
agg.duration <- aggregate(full.duration~rcp+Management, data = runs.count[runs.count$full.duration >0,], FUN = mean, na.rm = T)
agg.duration[, "sd"] <- aggregate(full.duration~rcp+Management, data = runs.count[runs.count$full.duration >0,], FUN = sd, na.rm = T)[, "full.duration"]
agg.duration[, "median"] <- aggregate(full.duration~rcp+Management, data = runs.count[runs.count$full.duration >0,], FUN = median, na.rm = T)[, "full.duration"]
agg.duration[, "mode"] <- aggregate(full.duration~rcp+Management, data = runs.count[runs.count$full.duration >0,], FUN = getmode)[, "full.duration"]

stat.shape <- reshape(agg.duration, idvar ="Management", timevar = "rcp",direction = "wide")
colnames(stat.shape) <- c("Management", "Mean # crashes (rcp45)", "SD # of crashes (rcp45)", "Median (rcp45)", "Mode (rcp45)",
                          "Mean # crashes (rcp85)", "SD # of crashes (rcp85)", "Median (rcp45)", "Mode (rcp85)")

#Filling in the first year
runs.late$nonseq.loss.event.20 <- ifelse(is.na(runs.late$nonseq.loss.event.20), F ,runs.late$nonseq.loss.event.20)

crash.df <- data.frame()
for(MOD in unique(runs.late$GCM)){
  for(RCP in unique(runs.late$rcp)){
    for(MNG in unique(runs.late$Management)){
      count <- nrow((runs.late[runs.late$GCM == MOD & runs.late$rcp == RCP & runs.late$Management == MNG & runs.late$nonseq.loss.event.20,]))
      temp <- data.frame(MOD, RCP, MNG)
      temp$crash.count <- count
      crash.df <- rbind(crash.df, temp)
    }
  }
}


stat.df <- aggregate(crash.count~RCP+MNG, data = crash.df, FUN = mean)
stat.df[,"sd"] <- aggregate(crash.count~RCP+MNG, data = crash.df, FUN = sd)[, "crash.count"]

stat.shape <- reshape(stat.df, idvar ="MNG", timevar = "RCP",direction = "wide")
colnames(stat.shape) <- c("Management", "Mean # crashes (rcp45)", "SD # of crashes (rcp45)",
                          "Mean # crashes (rcp85)", "SD # of crashes (rcp85)")

loss.freq.20 <- as.data.frame(table(runs.late[runs.late$nonseq.loss.event.20, "Management"]))
colnames(loss.freq.20) <- c("Management", "Number of Nonsequential Major Crashes (20%)")
write.csv(stat.shape, "../data/Frequency_of_nonsequential_major_loss.csv", row.names=F)

hist(crash.df$crash.count)

colnames(crash.df) <- c("GCM", "rcp", "Management", "crash.count")
crash.df$Management <- factor(crash.df$Management, levels = c("None", "Gap", "Shelter", "Under"))
crash.lm <- glm.nb(crash.count~Management*rcp, data = crash.df)
summary(crash.lm)

#---------------------------------------------------------#
#Immediate post-harvest structure
#---------------------------------------------------------#
#Organizing data into long form for easier graphing. Only using the year immediately post harvest
agg.harv <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~GCM+rcp+Driver.set+Management, data = runs.late[runs.late$year == 2025,], FUN = mean, na.action = NULL)

plot.harv <- stack(agg.harv[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.harv) <- c("values", "var")
plot.harv[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.harv[,c("GCM", "rcp", "Driver.set", "Management")]

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/Loss_Event_Figures"

#Making a box plot of the variables immediately post-harvest
png(width= 750, filename= file.path(path.figures, paste0('Immediate_post-harvest_Structure_by_Management.png')))
ggplot(plot.harv)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables immediately post-harvest (2025) by Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Organizing data into long form for easier graphing. Only using the year immediately post harvest
agg.end <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~GCM+rcp+Driver.set+Management, data = runs.comb[runs.comb$year == 2099,], FUN = mean, na.action = NULL)

plot.end <- stack(agg.end[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.end) <- c("values", "var")
plot.end[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.end[,c("GCM", "rcp", "Driver.set", "Management")]

#Making a box plot of the variables immediately post-harvest
png(width= 750, filename= file.path(path.figures, paste0('End_of_Run_Structure_by_Management.png')))
ggplot(plot.end)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables at run end (2099) by Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Organizing data into long form for easier graphing. Only using the year immediately post harvest
agg.mid <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~GCM+rcp+Driver.set+Management, data = runs.comb[runs.comb$year == 2050,], FUN = mean, na.action = NULL)

plot.mid <- stack(agg.mid[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.mid) <- c("values", "var")
plot.mid[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.mid[,c("GCM", "rcp", "Driver.set", "Management")]

#Making a box plot of the variables immediately post-harvest
png(width= 750, filename= file.path(path.figures, paste0('Midcentury_Structure_by_Management.png')))
ggplot(plot.mid)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables at mid century (2050) by Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Test attempt at including p-values in the boxplots
ggboxplot(plot.harv, x = "Management", y = "values",
          color = "Management", palette = "jco")+
  facet_wrap(~var, scales = "free")+
  ggtitle("Structural variables immediately post-harvest (2025) by Management")+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "None") 

#lme anova analysis of our structural variables and Tukey multiple comparison analysis
mult.df.25 <- data.frame()
mult.df.50 <- data.frame()
mult.df.99 <- data.frame()
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")
for(RCP in unique(runs.late$rcp)){
  for(COL in struc.var){
    lm.test.25 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(Driver.set =~1), data = runs.late[runs.late$year == 2025 & runs.late$rcp == RCP,], method = "ML")
    
    #Doing a multiple comparison across the different management types
    mult.list.25 <- list()
    post.hoc <- glht(lm.test.25, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.25[[paste(COL)]]$Var <- COL
    mult.list.25[[paste(COL)]]$rcp <- RCP
    mult.list.25[[paste(COL)]]$Comp <- c("Gap-None", "Shelter-None", "Under-None", "Shelter-Gap", "Under-Gap", "Under-Shelter")
    mult.list.25[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.25 <- dplyr::bind_rows(mult.list.25)
    mult.df.25 <- rbind(mult.df.25, dat.mult.25)
    
    
    lm.test.50 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2050 & runs.late$rcp == RCP,], method = "ML")
    
    mult.list.50 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.50, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.50[[paste(COL)]]$Var <- COL
    mult.list.50[[paste(COL)]]$rcp <- RCP
    mult.list.50[[paste(COL)]]$Comp <- c("Gap-None", "Shelter-None", "Under-None", "Shelter-Gap", "Under-Gap", "Under-Shelter")
    mult.list.50[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.50 <- dplyr::bind_rows(mult.list.50)
    mult.df.50 <- rbind(mult.df.50, dat.mult.50)
    
    
    lm.test.99 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.comb[runs.comb$year == 2099 & runs.comb$rcp == RCP,], method = "ML")
    
    mult.list.99 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.99, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.99[[paste(COL)]]$Var <- COL
    mult.list.99[[paste(COL)]]$rcp <- RCP
    mult.list.99[[paste(COL)]]$Comp <- c("Gap-None", "Shelter-None", "Under-None", "Shelter-Gap", "Under-Gap", "Under-Shelter")
    mult.list.99[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.99 <- dplyr::bind_rows(mult.list.99)
    mult.df.99 <- rbind(mult.df.99, dat.mult.99)
  }
}
#Creating different dataframes to look at specific windows. This information should evtually end up captured in a figure
rcp45.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp45",], Comp ~ Var)
rcp45.25.df$Scenario <- "Low Emmissions"
rcp45.25.df <- rcp45.25.df[,c(8,1,2,3,4,5,6,7)]
rcp85.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp85",], Comp ~ Var)
rcp85.25.df$Scenario <- "High Emmissions"
rcp85.25.df <- rcp85.25.df[,c(8,1,2,3,4,5,6,7)]

rcp45.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp45",], Comp ~ Var)
rcp45.50.df$Scenario <- "Low Emmissions"
rcp45.50.df <- rcp45.50.df[,c(8,1,2,3,4,5,6,7)]
rcp85.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp85",], Comp ~ Var)
rcp85.50.df$Scenario <- "High Emmissions"
rcp85.50.df <- rcp85.50.df[,c(8,1,2,3,4,5,6,7)]

rcp45.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp45",], Comp ~ Var)
rcp45.99.df$Scenario <- "Low Emmissions"
rcp45.99.df <- rcp45.99.df[,c(8,1,2,3,4,5,6,7)]
rcp85.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp85",], Comp ~ Var)
rcp85.99.df$Scenario <- "High Emmissions"
rcp85.99.df <- rcp85.99.df[,c(8,1,2,3,4,5,6,7)]

#--------------------------------------------------#
# Looking at Structure after crash
#--------------------------------------------------#
runs.MNG.comp <- data.frame()
for(i in 1:nrow(runs.late)){
  if(runs.late[i, "nonseq.loss.event.20"] == T){
    temp.lead <- runs.late[runs.late$Driver.set == runs.late[i, "Driver.set"] & runs.late$year == runs.late[i, "year"], ]
    #Now pulling the year before the crash
    temp.lag <- runs.late[runs.late$Driver.set == runs.late[i, "Driver.set"] & runs.late$year == (runs.late[i, "year"]-1), ]
    #Flagging the Management that ha the loss event
    temp.lag$nonseq.loss.event.20 <- temp.lead$nonseq.loss.event.20
    runs.MNG.comp <- rbind(runs.MNG.comp, temp.lag)
  }
}

#Creating a figure that shows the structure of all the management styles before at least one of them crashed. 
agg.crash <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~rcp+Driver.set+Management+nonseq.loss.event.20, data = runs.MNG.comp, FUN = mean, na.action = NULL)

plot.crash <- stack(agg.crash[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.crash) <- c("values", "var")
plot.crash[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")] <- agg.crash[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")]

#We can now compare the year before a crash across management styles experiencing the same weather conditions
png(width= 750, filename= file.path(path.figures, paste0('Pre-crash_Structure_by_whether_crash_occured.png')))
ggplot(plot.crash)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=nonseq.loss.event.20, y=values, color = rcp))+
  ggtitle("Structural variables immediately pre-crash by whether they crashed")+
  xlab("Was there a major loss event")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()



library(data.table)
#Creating a figure that shows the structure of all the management styles before at least one of them crashed. 
runs.before <- data.frame()
for(i in 1:nrow(runs.late)){
  if(runs.late[i, "nonseq.loss.event.20"] == T){
    temp.lead <- runs.late[runs.late$Driver.set == runs.late[i, "Driver.set"] & runs.late$year == runs.late[i, "year"], ]
    #Now pulling the year before the crash
    temp.lag <- runs.late[runs.late$Driver.set == runs.late[i, "Driver.set"] & runs.late$year == runs.late[i-1, "year"], ]
    #Flagging the Management that ha the loss event
    temp.lag$nonseq.loss.event.20 <- temp.lead$nonseq.loss.event.20
    runs.before <- rbind(runs.before, temp.lag)
  }
}


agg.stack <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.height.mean, tree.dbh.sd, tree.height.sd)~GCM+rcp+Driver.set+Management, data = runs.before, FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.stack[,c("GCM", "rcp", "Driver.set", "Management")]

#We can now compare the year before a crash across management styles experiencing the same weather conditions
png(width= 750, filename= file.path(path.figures, paste0('Pre-crash_Structure_by_Management.png')))
ggplot(plot.stack)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables immediately pre-crash by Management")+
  xlab("Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#T test on the structural metrics
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "agb"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "agb"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "density.tree"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "density.tree"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "tree.dbh.mean"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "tree.dbh.mean"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "tree.height.mean"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "tree.height.mean"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "tree.dbh.sd"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "tree.dbh.sd"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "tree.height.sd"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "tree.height.sd"])