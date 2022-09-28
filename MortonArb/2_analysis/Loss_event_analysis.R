library(lubridate)
library(dplyr)
library(lubridate)
library(nlme)
library(AICcmodavg)
library(ggplot2)

path.read <- "../data/"

runs.comb <- read.csv(paste0(path.read, "All_runs_yearly.csv"))

runs.comb$Management <- factor(runs.comb$Management, levels = c("None", "Gap", "Shelter", "Under"))

runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

runs.late <- runs.comb[runs.comb$year >= 2025,]

runs.late <- runs.late[!is.na(runs.late$agb.rel.diff),]

#Counting the number of massive agb loss events
#agb.extreme <- as.numeric(quantile(runs.late$agb.rel.diff, probs = c(.025)))

runs.late$loss.event.20 <- ifelse(runs.late$agb.rel.diff <= -.20, T, F)
runs.late$loss.event.10 <- ifelse(runs.late$agb.rel.diff <= -.10, T, F)

write.csv(loss.freq, "../data/Frequency_of_major_loss.csv", row.names=F)

for(i in 5:nrow(runs.late)){
  DRIVE <- runs.late[i, "Driver.set"]
  MNG <- runs.late[i, "Management"]
  YR <- runs.late[i, "year"]
  if(YR != 2025){
  prev.20 <- runs.late[runs.late$Driver.set == DRIVE & runs.late$Management == MNG & runs.late$year == YR-1 , "loss.event.20"]
  runs.late[i, "nonseq.loss.event.20"] <- ifelse((runs.late[i, "loss.event.20"] == T & prev.20 ==F), T, F)
  prev.10 <- runs.late[runs.late$Driver.set == DRIVE & runs.late$Management == MNG & runs.late$year == YR-1 , "loss.event.10"]
  runs.late[i, "nonseq.loss.event.10"] <- ifelse((runs.late[i, "loss.event.10"] == T & prev.10 ==F), T, F)
  }
}

#Filling in the first year
runs.late$nonseq.loss.event.20 <- ifelse(is.na(runs.late$nonseq.loss.event.20), F ,runs.late$nonseq.loss.event.20)
runs.late$nonseq.loss.event.10 <- ifelse(is.na(runs.late$nonseq.loss.event.10), F ,runs.late$nonseq.loss.event.10)


crash.count <- runs.late %>% count(nonseq.loss.event.20, Management, GCM, rcp)

loss.freq.20 <- as.data.frame(table(runs.late[runs.late$nonseq.loss.event.20, "Management"]))
loss.freq.10 <- as.data.frame(table(runs.late[runs.late$nonseq.loss.event.10, "Management"]))

colnames(loss.freq.20) <- c("Management", "Number of Nonsequential Major Crashes (20%)")
write.csv(loss.freq.20, "../data/Frequency_of_nonsequential_major_loss.csv", row.names=F)

#Organizing data into long form for easier graphing. Only using the year immediately post harvest
agg.stack <- aggregate(cbind(agb, density.tree, dbh.mean, height.mean, dbh.sd, height.sd)~GCM+rcp+Driver.set+Management, data = runs.late[runs.late$year == 2025,], FUN = mean, na.action = NULL)

plot.stack <- stack(agg.stack[,c("agb", "density.tree", "dbh.mean", "height.mean", "dbh.sd", "height.sd")])
names(plot.stack) <- c("values", "var")
plot.stack[,c("GCM", "rcp", "Driver.set", "Management")] <- agg.stack[,c("GCM", "rcp", "Driver.set", "Management")]

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/Loss_Event_Figures"

#Making a box plot of the variables immediately post-harvest
png(width= 750, filename= file.path(path.figures, paste0('Immediate_post-harvest_Structure_by_Management.png')))
ggplot(plot.stack)+
  facet_wrap(~var, scales = "free")+
  geom_boxplot(aes(x=Management, y=values, color = Management), show.legend = FALSE)+
  ggtitle("Structural variables immediately post-harvest (2025) by Management")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()
  

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
#Lead anaysis of 5 years
#------------------------------------------------------#

runs.late$loss.event.20 <- ifelse(runs.late$agb.rel.lead5 <= -.50, T, F)#50%
loss.freq.20 <- as.data.frame(table(runs.late[runs.late$loss.event.20, "Management"]))
colnames(loss.freq.20) <- c("Management", "Number of Major Crashes (20%)")
write.csv(loss.freq, "../data/Frequency_of_major_loss.csv", row.names=F)

for(i in 5:nrow(runs.late)){
  DRIVE <- runs.late[i, "Driver.set"]
  MNG <- runs.late[i, "Management"]
  YR <- runs.late[i, "year"]
  if(YR >= 2030){
    prev.20 <- runs.late[runs.late$Driver.set == DRIVE & runs.late$Management == MNG & runs.late$year == YR-5 , "loss.event.20"]
    runs.late[i, "nonseq.loss.event.20"] <- ifelse((runs.late[i, "loss.event.20"] == T & prev.20 ==F), T, F)
    
  }
}

#Filling in the first year
runs.late$nonseq.loss.event.20 <- ifelse(is.na(runs.late$nonseq.loss.event.20), F ,runs.late$nonseq.loss.event.20)
runs.late$nonseq.loss.event.10 <- ifelse(is.na(runs.late$nonseq.loss.event.10), F ,runs.late$nonseq.loss.event.10)


crash.count <- runs.late %>% count(nonseq.loss.event.20, Management, GCM, rcp)


loss.freq.20 <- as.data.frame(table(runs.late[runs.late$nonseq.loss.event.20, "Management"]))
colnames(loss.freq.20) <- c("Management", "Number of Nonsequential Major Crashes (20%)")

colnames(loss.freq.20) <- c("Management", "Number of Nonsequential Major Crashes (20%)")
write.csv(loss.freq.20, "../data/Frequency_of_nonsequential_major_loss.csv", row.names=F)

