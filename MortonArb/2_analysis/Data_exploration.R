library(ggplot2)

runs.start <- read.csv("../data/Summary_PFTs_Site_Year.csv")

runs.start <- runs.start[runs.start$GCM != "ACCESS1-0", ]

met.temp <- read.csv("../data/CMIP5_TDM_year_byModel.csv")

met.temp <- met.temp[met.temp$var == "precipitation_flux",]

runs.all <- merge(runs.start, met.temp, by.x= c('GCM', 'RCP', 'year'), by.y= c('model', 'scenario', 'year'))

runs.first <- runs.all[runs.all$year < 2036 & runs.all$year > 2025,]

runs.last <- runs.all[runs.all$year > 2089,]


#This is an abomination of aggregate functions that could be combined but I wrote this quick to get an abstract out.
AGB.num <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.first,
                        FUN = mean)

AGB.num[,c("first.mean.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.first,
                                         FUN = mean)[,c("mean")]

colnames(AGB.num) <- c("Management", "GCM", "RCP", "first.mean.AGB", "first.mean.temp")

AGB.num[,c("first.sd.AGB")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.first,
                     FUN = sd)[,c("AGB.tot")]

AGB.num[,c("first.sd.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.first,
                                         FUN = sd)[,c("mean")]

AGB.num[,c("last.mean.AGB")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.last,
                                          FUN = mean)[,c("AGB.tot")]

AGB.num[,c("last.mean.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.last,
                                          FUN = mean)[,c("mean")]

AGB.num[,c("last.sd.AGB")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.last,
                                         FUN = sd)[,c("AGB.tot")]

AGB.num[,c("last.sd.temp")] <- aggregate(mean~Management+GCM+RCP, data =runs.last,
                                        FUN = sd)[,c("mean")]

#Another aggreagate abomination creates differnet data frame structure for another visual. Can compare first and last

first.num <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.first,
                       FUN = mean)

first.num$type <- "first"

first.num[,c("AGB.sd")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.first,
                                           FUN = sd)[,c("AGB.tot")]

first.num[,c("temp.mean")] <- aggregate(mean~Management+GCM+RCP, data =runs.first,
                                 FUN = mean)[,c("mean")]

first.num[,c("temp.sd")] <- aggregate(mean~Management+GCM+RCP, data =runs.first,
                                 FUN = sd)[,c("mean")]


last.num <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.last,
                         FUN = mean)

last.num$type <- "last"

last.num[,c("AGB.sd")] <- aggregate(AGB.tot~Management+GCM+RCP, data =runs.last,
                                   FUN = sd)[,c("AGB.tot")]

last.num[,c("temp.mean")] <- aggregate(mean~Management+GCM+RCP, data =runs.last,
                                        FUN = mean)[,c("mean")]

last.num[,c("temp.sd")] <- aggregate(mean~Management+GCM+RCP, data =runs.last,
                                      FUN = sd)[,c("mean")]

both.num <- rbind(first.num, last.num)

AGB.num$AGB.diff <-  AGB.num$last.mean.AGB - AGB.num$first.mean.AGB
AGB.num$temp.diff <- AGB.num$last.mean.temp - AGB.num$first.mean.temp


Style <- c("Gap", "Shelter", "Under")

diff.list <- list()
for(GCM in unique(AGB.num$GCM)){
  for(RCP in unique(AGB.num$RCP)){
    for(Management in Style){
        control <- AGB.num[AGB.num$GCM == GCM & AGB.num$RCP == RCP & AGB.num$Management == "None" , "AGB.diff"]
        new.diff <- AGB.num[AGB.num$GCM == GCM & AGB.num$RCP == RCP & AGB.num$Management == Management, "AGB.diff"] - control
        diff.list[[paste(GCM, RCP, Management, sep="-")]]$GCM <- as.factor(GCM)
        diff.list[[paste(GCM, RCP, Management, sep="-")]]$RCP <- as.factor(RCP)
        diff.list[[paste(GCM, RCP, Management, sep="-")]]$Management <- as.factor(Management)
        diff.list[[paste(GCM, RCP, Management, sep="-")]]$AGB.diff <- new.diff
        diff.list[[paste(GCM, RCP, Management, sep="-")]]$temp.diff <- AGB.num[AGB.num$GCM == GCM & AGB.num$RCP == RCP & AGB.num$Management == Management, "temp.diff"]
    }
  }
}  
diff.AGB <- dplyr::bind_rows(diff.list)


AGB.num$Management <- factor(AGB.num$Management, levels =c("None", "Gap", "Shelter", "Under"))
both.num$Management <- factor(both.num$Management, levels =c("None", "Gap", "Shelter", "Under"))


both <- c("first", "last")


pred.list <- list()
for(GCM in unique(both.num$GCM)){
  for(RCP in unique(both.num$RCP)){
    for(Management in Style){
      for(DEC in both){
        control <- both.num[both.num$GCM == GCM & both.num$RCP == RCP &  both.num$type == DEC & both.num$Management == "None" , "AGB.tot"]
        new.agb <- both.num[both.num$GCM == GCM & both.num$RCP == RCP &  both.num$type == DEC & both.num$Management == Management, "AGB.tot"] - control
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$GCM <- as.factor(GCM)
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$RCP <- as.factor(RCP)
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$Management <- as.factor(Management)
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$DEC <- DEC
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$AGB.tot <- new.agb
        pred.list[[paste(GCM, RCP, Management, DEC, sep="-")]]$temp.mean <- both.num[both.num$GCM == GCM & both.num$RCP == RCP &  both.num$type == DEC & both.num$Management == Management, "temp.mean"]
      }
    }
  }
}  
Rel.AGB <- dplyr::bind_rows(pred.list)


#DF key before I clean this better
#AGB.num = has first and last decade in one row
#both.num = has first and last decade as sepearte rows with identifier type 
#diff.AGB = Is a relative version of AGB.num that sets everything relative to the control
#Rel.AGB = Is a relative version of both.num that sets everythign relative to the control



png(file.path("Total_AGB_bplot.png"), height=10, width=8, units="in", res=120)
  ggplot(data=Rel.AGB)+
    facet_grid(rows = vars(RCP), cols =vars(Management))+
    geom_boxplot(aes(x=DEC, y=AGB.tot))+
    ggtitle("Mean Total AGB for first and last decade after harvest")
dev.off()
  
#png(file.path("Total_AGB_bplot_RCP85.png"), height=10, width=8, units="in", res=120)
  ggplot(data=Rel.AGB[Rel.AGB$RCP == "rcp85",])+
    facet_grid(rows = vars(GCM), cols =vars(Management))+
    geom_boxplot(aes(x=DEC, y=AGB.tot))+
    ggtitle("Mean Total AGB for first and last decade after harvest RCP85")
#dev.off()
  
png(file.path("Rel_Total_ABG_Mean_Diff.png"), height=10, width=8, units="in", res=120)
 ggplot(data=diff.AGB)+
  facet_wrap(~RCP)+
  geom_boxplot(aes(x=Management, y=AGB.diff, color = Management))+
  geom_boxplot(aes(x= Management, y=temp.diff))+
  ggtitle("Difference from control in post harvest Total AGB between first decade and last")+
  ylab("Difference to difference of control")
dev.off()

png(file.path("Rel_AGB_v_Temp.png"), height=10, width=8, units="in", res=120)
ggplot(data=diff.AGB)+
  geom_smooth(aes(x=temp.diff, y=AGB.diff, color = Management), method='lm')+
  geom_point(aes(x=temp.diff, y=AGB.diff, color = Management))+
  geom_line(aes(x=temp.diff, y=AGB.diff, color = Management))+
  ggtitle("Difference from control in post harvest Total AGB between first decade and last")+
  ylab("Difference to difference of control")
dev.off()

png(file.path("AGB_v_Temp.png"), height=10, width=8, units="in", res=120)
ggplot(data=AGB.num)+
  facet_wrap(~RCP, scales = "free")+
  #geom_smooth(aes(x=temp.diff, y=AGB.diff, color = Management), method='lm')+
  geom_point(aes(x=temp.diff, y=AGB.diff, color = Management))+
  geom_line(aes(x=temp.diff, y=AGB.diff, color = Management))+
  ggtitle("Difference in post harvest Total AGB between first decade and last")+
  ylab("Difference")
dev.off()


M.test <- aov(AGB.diff ~ Management, AGB.num)
G.test <- aov(AGB.diff ~ GCM, AGB.num)
T.test <- aov(AGB.diff ~ temp.diff, AGB.num)
doub.test <- lm(AGB.diff ~ temp.diff * Management, AGB.num)
library("nlme")
lm.test <- lme(AGB.diff ~ temp.diff*Management, random=list(RCP=~1, GCM=~1), data=AGB.num)

anova(lm.test)

summary(M.test)
anova(doub.test)
M.reg <- lm(AGB.diff ~ Management, AGB.num)
T.reg <- lm(AGB.diff ~ temp.diff, AGB.num)
G.reg <- lm(AGB.diff ~ GCM, AGB.num)
summary(T.reg)


  
