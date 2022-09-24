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
agb.extreme <- as.numeric(quantile(runs.late$agb.rel.diff, probs = c(.025)))

runs.late$loss.event <- ifelse(runs.late$agb.rel.diff <= agb.extreme, T, F)

loss.freq <- as.data.frame(table(runs.late[runs.late$loss.event, "Management"]))
colnames(loss.freq) <- c("Management", "Number_of_loss_events")
write.csv(loss.freq, "../data/Frequency_of_major_loss.csv", row.names=F)



for(DRIVE in unique(runs.late$Driver.set)){
  temp <- runs.late[runs.late$Driver.set == DRIVE  & runs.late$year == 2025,]
  
}

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
  

