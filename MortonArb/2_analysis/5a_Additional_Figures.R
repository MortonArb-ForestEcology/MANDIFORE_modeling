#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection, does linear regression, and creates figures
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#

#------------------------------------------------------------------------#
# FIGURES SECTION
#------------------------------------------------------------------------#
path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/Outline Figures"

library(ggplot2)
cbPalette  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Proportional agb change vs VPD by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_to_VPD_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) to VPD increases by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = agb.rel.diff, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Proportional agb change vs agb by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_to_AGB_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) to current AGB increases by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=agb, y = agb.rel.diff, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Proportional agb change vs time by management
png(width= 750, filename= file.path(path.figures, paste0('Proportional_Agb_Change_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional Change in above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb.rel.diff, group = Driver.set, color = rcp))+
  ylab("Proportional change in AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#AGB over time by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_Over_Time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb, group = Driver.set, color = rcp))+
  ylab("AGB")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Example using a single GCM
png(width= 750, filename= file.path(path.figures, paste0('Sinlge_GCM_AGB_Over_Time_by_Management.png')))
ggplot(data=runs.late[runs.late$Driver.set == "BNU-ESM.rcp45",])+
  ggtitle("Example: Above ground biomass (AGB) over time by Management for BNU-ESM.rcp45")+
  #facet_wrap(~Management)+
  geom_line(aes(x=year, y = agb, color = Management))+
  ylab("AGB")+
  theme(plot.title = element_text(size = 10, face = "bold"))
dev.off()


pdf(file= file.path(path.figures, paste0('AGB_Over_Time_Individual_GCM.pdf')))
for(DRIVE in unique(runs.late$Driver.set)){
  temp.fig <- ggplot(data=runs.late[runs.late$Driver.set == DRIVE,])+
    ggtitle(paste0(DRIVE,": Above ground biomass (AGB) over time by Management"))+
    #facet_wrap(~Management)+
    geom_line(aes(x=year, y = agb, color = Management))+
    ylab("AGB")+
    theme(plot.title = element_text(size = 10, face = "bold"))
  print(temp.fig)
}
dev.off()

runs.late$total.precip <- runs.late$sum
runs.late$air.temp <- runs.late$tair
runs.long <- tidyr::gather(runs.late, var, values, air.temp, total.precip, VPD, factor_key=TRUE)

#VPD increasing over time
png(width= 750, filename= file.path(path.figures, paste0('Weather_Changing.png')))
ggplot(data=runs.long)+
  ggtitle("CMIP5 models weather metrics across two different emisisons scenarios")+
  facet_grid(var~rcp, scales = "free")+
  geom_point(aes(x = year, y= values, color = GCM))+
  geom_smooth(aes(x = year, y= values, color = GCM))+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#AGB over height sd by management
png(width= 750, filename= file.path(path.figures, paste0('AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=height.sd, y = agb, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=height.sd, y=agb, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Proportional AGB over height sd by management
png(width= 800, filename= file.path(path.figures, paste0('Proportional_change_in_AGB_vs_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in above ground biomass (AGB) vs. standard deviaiton of tree height by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=height.sd, y = agb.rel.diff, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=height.sd, y=agb.rel.diff, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("Proportional change in AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 14, face = "bold"))
dev.off()


#Proportional AGB over AGB by management
png(width= 800, filename= file.path(path.figures, paste0('Proportional_change_in_AGB_vs_AGB_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in above ground biomass (AGB) vs. AGB by management")+
  facet_wrap(~Management)+
  geom_point(aes(x=agb, y = agb.rel.diff, color = harvest, shape = rcp))+
  geom_point(data = runs.comb[runs.comb$harvest == "Pre-harvest" | runs.comb$harvest == "Harvest",], aes(x=agb, y=agb.rel.diff, color = harvest))+
  scale_colour_manual(values=cbPalette)+
  ylab("Proportional change in AGB")+
  xlab("SD of tree height")+
  theme(plot.title = element_text(size = 14, face = "bold"))
dev.off()



#height sd vs VPD by Management
png(width= 750, filename= file.path(path.figures, paste0('Height_sd_vs_VPD_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Standard Deviation of Height vs. Vapor Pressure Deficit by Management")+
  facet_wrap(~Management)+
  geom_point(aes(x=VPD, y = height.sd, color = rcp))+
  ylab("SD of tree height")+
  xlab("VPD")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#height sd over time by Management
png(width= 750, filename= file.path(path.figures, paste0('Height_sd_vs_time_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Standard Deviation of Height over time by Management")+
  facet_wrap(~Management)+
  geom_line(aes(x=year, y = height.sd, group= Driver.set, color = rcp))+
  ylab("SD of tree height")+
  xlab("year")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()


#Distribution of height.sd
png(width= 750, filename= file.path(path.figures, paste0('Dist_of_Height_sd_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Distribution of Standard Deviation of Height by Management")+
  facet_wrap(~Management)+
  geom_histogram(aes(x =height.sd))+
  ylab("Frequency")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Distribution of proportional change in agb
png(width= 750, filename= file.path(path.figures, paste0('Proportional_change_in_Agb_by_Management.png')))
ggplot(data=runs.late)+
  ggtitle("Proportional change in agb by Management")+
  facet_wrap(~Management)+
  geom_histogram(aes(x =agb.rel.diff))+
  xlab("Proportional change in agb")+
  ylab("Frequency")+
  theme(plot.title = element_text(size = 16, face = "bold"))
dev.off()

#Boxplot section
runs.comb$Driver.set <- paste0(runs.comb$GCM,"." ,runs.comb$rcp)

box.df <- data.frame()
for(DRIVE in unique(runs.comb$Driver.set)){
  for(MNG in unique(runs.comb$Management)){
    temp <- runs.comb[runs.comb$Driver.set == DRIVE & runs.comb$Management == MNG, ]
    temp$height.sd.diff <- mean(temp[temp$year >= 2089, "height.sd"]) - mean(temp[temp$year <= 2017, "height.sd"])
    temp$dbh.sd.diff <-  mean(temp[temp$year >= 2089, "dbh.sd"]) - mean(temp[temp$year <= 2017, "dbh.sd"])
    temp$agb.change <-  mean(temp[temp$year >= 2089, "agb"]) - mean(temp[temp$year <= 2017, "agb"])
    out.df <- data.frame(unique(temp$Driver.set),unique(temp$Management), unique(temp$height.sd.diff), unique(temp$dbh.sd.diff), unique(temp$agb.change))
    colnames(out.df) <- c("Driver.set","Management" ,"height.sd.diff", "dbh.sd.diff", "agb.change")
    box.df <- rbind(box.df, out.df)
  }
}

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_height_sd_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in SD of tree height from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=height.sd.diff))+
  ylab("SD of tree height")+
  xlab("Management")
dev.off()

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_dbh_sd_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in SD of DBH from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=dbh.sd.diff))+
  ylab("SD of DBH")+
  xlab("Management")
dev.off()

png(width= 750, filename= file.path(path.figures, paste0('Boxplot_of_AGB_change.png')))
ggplot(data = box.df)+
  ggtitle("Change in AGB from first 10 years to last 10 years")+
  geom_boxplot(aes(x=Management, y=agb.change))+
  ylab("Aboveground biomass")+
  xlab("Management")
dev.off()