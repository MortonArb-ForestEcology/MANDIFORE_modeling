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

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]


#---------------------------------------------------------#
#Immediate post-harvest structure
#---------------------------------------------------------#
#Organizing data into long form for easier graphing. Only using the year immediately post harvest
agg.stack <- aggregate(cbind(agb, density.tree, dbh.mean, height.mean, dbh.sd, height.sd)~GCM+rcp+Driver.set+Management, data = runs.late[runs.late$year == 2025,], FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.stack[,c("GCM", "rcp", "Driver.set", "Management")]

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/Loss_Event_Figures"

#Making a box plot of the variables immediately post-harvest
png(width= 750, filename= file.path(path.figures, paste0('Immediate_post-harvest_Structure_by_Management.png')))
ggplot(plot.stack)+
  #facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables immediately post-harvest (2025) by Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Test attempt at including p-values in the boxplots
ggboxplot(plot.stack, x = "Management", y = "values",
          color = "Management", palette = "jco")+
  facet_wrap(~var, scales = "free")+
  ggtitle("Structural variables immediately post-harvest (2025) by Management")+
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "None") 

struc.df <- data.frame()
mult.df <- data.frame()
struc.var <- c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")
for(COL in struc.var){
  dry.list <- list()
  lm.test <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(Driver.set =~1), data = runs.late[runs.late$year == 2025,], method = "ML")
  sum <- summary(lm.test)
  df.eff <- as.data.frame(sum$tTable)
  df.eff$Fixedeff <- rownames(df.eff)
  df.eff$Equation <- paste(COL, "~", "Management")
  dry.list[[paste(COL)]]$Var <- COL
  dry.list[[paste(COL)]]$Equation <- df.eff$Equation
  dry.list[[paste(COL)]]$Fixedeff <- df.eff$Fixedeff
  dry.list[[paste(COL)]]$Value <- df.eff$Value
  dry.list[[paste(COL)]]$pvalue <- df.eff$`p-value`
  dat.dry <- dplyr::bind_rows(dry.list)
  struc.df <- rbind(struc.df, dat.dry)
  
  #Doing a multiple comparison across the different management types
  mult.list <- list()
  post.hoc <- glht(lm.test, linfct = mcp(Management = 'Tukey'))
  output <- summary(post.hoc)
  mult.list[[paste(COL)]]$Var <- COL
  #mult.list[[paste(COL)]]$Equation <- paste0(COL, " ~ Management")
  mult.list[[paste(COL)]]$Comp <- c("Gap-None", "Shelter-None", "Under-None", "Shelter-Gap", "Under-Gap", "Under-Shelter")
  mult.list[[paste(COL)]]$pvalue <- output$test$pvalues
  dat.mult <- dplyr::bind_rows(mult.list)
  mult.df <- rbind(mult.df, dat.mult)
}

sigstruc.df <- reshape2::dcast(struc.df, Fixedeff ~ Var)
sigmult.df <- reshape2::dcast(mult.df, Comp ~ Var)

write.csv(sigstruc.df, "../data/Post-harvest_structural_lme.csv", row.names = F)
write.csv(sigmult.df, "../data/Post-harvest_structural_multcomp.csv", row.names = F)

#-------------------------------------------------------------#
#Counting the number of massive agb loss events
#-------------------------------------------------------------#
#agb.extreme <- as.numeric(quantile(runs.late$agb.rel.diff, probs = c(.025)))

runs.late$loss.event.20 <- ifelse(runs.late$agb.rel.diff <= -.20, T, F)

#Counting individual instances of a crash beginning
for(i in 5:nrow(runs.late)){
  DRIVE <- runs.late[i, "Driver.set"]
  MNG <- runs.late[i, "Management"]
  YR <- runs.late[i, "year"]
  if(YR != 2025){
  prev.20 <- runs.late[runs.late$Driver.set == DRIVE & runs.late$Management == MNG & runs.late$year == YR-1 , "loss.event.20"]
  runs.late[i, "nonseq.loss.event.20"] <- ifelse((runs.late[i, "loss.event.20"] == T & prev.20 ==F), T, F)
  }
}

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
            if(i < 75 & temp[i+1, "loss.event.20"] == F){ #Caveat, if first crash is year 2099 we don't flag. Doesn't present a problem with our data
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
  if(YR != 2099){
    post <- runs.count[runs.count$Driver.set == DRIVE & runs.count$Management == MNG & runs.count$year == YR+1 , "loss.event.20"]
    runs.count[i, "full.duration"] <- ifelse((runs.count[i, "loss.event.20"] == T & post ==F), runs.count[i, "years.crash"], 0)
  }else if(YR == 2099){
    runs.count[i, "full.duration"] <- runs.count[i, "years.crash"]
    
  }
}

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
lm <- glm.nb(crash.count~Management*rcp, data = crash.df)
summary(lm)

#--------------------------------------------------#
# Structure after crash
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
agg.stack <- aggregate(cbind(agb, density.tree, dbh.mean, height.mean, dbh.sd, height.sd)~rcp+Driver.set+Management+nonseq.loss.event.20, data = runs.MNG.comp, FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")] <- agg.stack[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")]

#We can now compare the year before a crash across management styles experiencing the same weather conditions
png(width= 750, filename= file.path(path.figures, paste0('Pre-crash_Structure_by_whether_crash_occured.png')))
ggplot(plot.stack)+
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


agg.stack <- aggregate(cbind(agb, density.tree, dbh.mean, height.mean, dbh.sd, height.sd)~GCM+rcp+Driver.set+Management, data = runs.before, FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")])
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
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "dbh.mean"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "dbh.mean"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "height.mean"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "height.mean"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "dbh.sd"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "dbh.sd"])
t.test(runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == T, "height.sd"], runs.MNG.comp[runs.MNG.comp$nonseq.loss.event.20 == F, "height.sd"])


#------------------------------------------------------#
#Lead anaysis of 2 years
#------------------------------------------------------#

runs.late$loss.event.20 <- ifelse(runs.late$agb.rel.lead2 <= -.40, T, F)#50%
loss.freq.20 <- as.data.frame(table(runs.late[runs.late$loss.event.20, "Management"]))
colnames(loss.freq.20) <- c("Management", "Number of Major Crashes (20%)")
write.csv(loss.freq, "../data/Frequency_of_major_loss.csv", row.names=F)

for(i in 5:nrow(runs.late)){
  DRIVE <- runs.late[i, "Driver.set"]
  MNG <- runs.late[i, "Management"]
  YR <- runs.late[i, "year"]
  if(YR >= 2030){
    prev.20 <- runs.late[runs.late$Driver.set == DRIVE & runs.late$Management == MNG & runs.late$year == YR-2 , "loss.event.20"]
    runs.late[i, "nonseq.loss.event.20"] <- ifelse((runs.late[i, "loss.event.20"] == T & prev.20 ==F), T, F)
    
  }
}

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

hist(crash.df$crash.count)

colnames(crash.df) <- c("GCM", "rcp", "Management", "crash.count")
crash.df$Management <- factor(crash.df$Management, levels = c("None", "Gap", "Shelter", "Under"))
lm <- pscl::zeroinfl(crash.count~Management*rcp, data = crash.df, dist = "negbin")
summary(lm)

colnames(loss.freq.20) <- c("Management", "Number of Nonsequential Major Crashes (20%)")
write.csv(loss.freq.20, "../data/Frequency_of_nonsequential_major_loss.csv", row.names=F)


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
agg.stack <- aggregate(cbind(agb, density.tree, dbh.mean, height.mean, dbh.sd, height.sd)~rcp+Driver.set+Management+nonseq.loss.event.20, data = runs.MNG.comp, FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")] <- agg.stack[,c("rcp", "Driver.set", "Management", "nonseq.loss.event.20")]

#We can now compare the year before a crash across management styles experiencing the same weather conditions
png(width= 750, filename= file.path(path.figures, paste0('Pre-crash_Structure_by_whether_crash_occured.png')))
ggplot(plot.stack)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=nonseq.loss.event.20, y=values, color = rcp))+
  ggtitle("Structural variables immediately pre-crash by whether they crashed")+
  xlab("Was there a major loss event")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()
