#----------------------------------------------------------------------------------------------------------------------#
# Script by : Lucien Fitzpatrick
# Project: Mandifore/AGU 2022
# Purpose: This script runs AIC model selection, does linear regression, and creates figures
# Inputs: Yearly ED2 output csv from script 2a_Yearly_ED2_sum.R
# Outputs: Figures and Tables
# Notes: 
#----------------------------------------------------------------------------------------------------------------------#
library(ggplot2)
library(nlme)
library(multcomp)
#------------------------------------------------------------------------#
# FIGURES SECTION
#------------------------------------------------------------------------#
path.google <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/"

runs.yr <- read.csv(file.path(path.google, "processed_data/All_runs_yearly.csv"))
runs.yr$Management <- factor(runs.yr$Management, levels=c("None", "Under", "Shelter", "Gap"))
runs.yr$RCP.name <- car::recode(runs.yr$rcp, "'rcp45'='Low Emmissions'; 'rcp85'='High Emissions'")
runs.yr$RCP.name <- factor(runs.yr$RCP.name, levels=c("Low Emmissions", "High Emissions"))
summary(runs.yr)

# Getting summaries of # crashes by scenario by mid & end of century
runs.yr$crash <- ifelse(runs.yr$agb.rel.diff.future<=-0.2, 1, 0)

crash.summary <- aggregate(crash~ GCM + rcp + RCP.name + Management + year, data=runs.yr[runs.yr$year>=2025 ,], FUN=sum)
names(crash.summary)[names(crash.summary)=="crash"] <- "crash.end"
crash.summary$crash.mid <- aggregate(crash~ GCM + rcp + RCP.name + Management, data=runs.yr[runs.yr$year>=2025 & runs.yr$year<=2050,], FUN=sum)$crash
summary(crash.summary)

library(MASS)
library(lme4)
crash.end <- glm.nb(crash.end~Management*rcp, data = crash.summary)
summary(crash.end)
anova(crash.end)


crash.mid <- glm.nb(crash.mid~Management*rcp, data = crash.summary)
summary(crash.mid)
anova(crash.mid)

crash.first <- aggregate(crash~ GCM + rcp + RCP.name + Management + year, data=runs.yr[runs.yr$year>=2025 ,], FUN=sum)
names(crash.first)[names(crash.first)=="crash"] <- "crash.end"
crash.first <- crash.first[crash.first$crash.end == 1,]

dat.first <- data.frame()
for(MOD in unique(crash.first$GCM)){
  for(RCP in unique(crash.first$rcp)){
    for(MNG in unique(crash.first$Management)){
      temp <- crash.first[crash.first$GCM == MOD & crash.first$rcp == RCP & crash.first$Management == MNG,] 
      temp.1 <- temp[1,]
      dat.first <- rbind(dat.first,temp.1)
    }
  }
}
dat.first <- dat.first[!is.na(dat.first$crash.end),]

mng.first.agg <- aggregate(cbind(year)~Management, data = dat.first, FUN = mean, na.action = NULL)
mng.first.agg[, "sd"] <- aggregate(cbind(year)~Management, data = dat.first, FUN = sd, na.action = NULL)[, "year"]


#We see that at the mid-century point, Shelter () management was the most resistant to AGB loss events compared to other prescriptions...
mng.crash.agg <- aggregate(cbind(crash.mid)~Management, data = crash.summary, FUN = mean, na.action = NULL)
mng.crash.agg[, "sd"] <- aggregate(cbind(crash.mid)~Management, data = crash.summary, FUN = sd, na.action = NULL)[, "crash.mid"]

#RCP 8.5 experienced more loss events than RCP 4.5 demonstrating the increasing importance of climate and...
rcp.crash.agg <- aggregate(cbind(crash.end)~rcp, data = crash.summary, FUN = mean, na.action = NULL)
rcp.crash.agg[, "sd"] <- aggregate(cbind(crash.end)~rcp, data = crash.summary, FUN = sd, na.action = NULL)[, "crash.end"]

#But by the end of the century air temperature and VPD were both higher in rcp 8.5 than in rcp 4.5 
rcp.temp.agg <- aggregate(cbind(tair)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = mean, na.action = NULL)
rcp.temp.agg[, "sd"] <- aggregate(cbind(tair)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = sd, na.action = NULL)[, "tair"]
rcp.VPD.agg <- aggregate(cbind(VPD)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = mean, na.action = NULL)
rcp.VPD.agg[, "sd"] <- aggregate(cbind(VPD)~rcp, data = runs.yr[runs.yr$year==2099,], FUN = sd, na.action = NULL)[, "VPD"]

# Distinct structures were caused by management (p<0.05, df=) for all four different styles immediately post harvest (S4).
mng.struc.agg.postharv <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2025,], FUN = mean, na.action = NULL)
mng.struc.agg.postharv[, c("agb.sd", "density.tree.sd", "dbh.mean.sd", "dbh.sd.sd")] <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2025,], FUN = sd, na.action = NULL)[, c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")]

# Mid-century AGB was similar across all prescriptions while the structural changes of tree density, mean tree dbh, and tree dbh sd persisted.
mng.struc.agg.mid <- aggregate(cbind(density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2050,], FUN = mean, na.action = NULL)
mng.struc.agg.mid[, c("density.tree.sd", "dbh.mean.sd", "dbh.sd.sd")] <- aggregate(cbind(agb, density.tree, tree.dbh.mean, tree.dbh.sd)~Management, data = runs.yr[runs.yr$year==2050,], FUN = sd, na.action = NULL)[, c("density.tree", "tree.dbh.mean", "tree.dbh.sd")]

# -----------------------------------------------------------
# Looking at the effect of harvest on structural variables
# -----------------------------------------------------------
vars.plot <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")

dat.harvest.pre <- stack(runs.yr[runs.yr$year==2019, vars.plot])
dat.harvest.pre$time <- "pre-harvest"
dat.harvest.pre[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2019, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.post <- stack(runs.yr[runs.yr$year==2025, vars.plot])
dat.harvest.post$time <- "post-harvest"
dat.harvest.post[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2025, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.mid <- stack(runs.yr[runs.yr$year==2050, vars.plot])
dat.harvest.mid$time <- "mid-century"
dat.harvest.mid[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2050, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest.end <- stack(runs.yr[runs.yr$year==2099, vars.plot])
dat.harvest.end$time <- "end-century"
dat.harvest.end[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[runs.yr$year==2099, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.harvest <- rbind(dat.harvest.pre, dat.harvest.post, dat.harvest.mid, dat.harvest.end)

theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text.x = element_text(size=rel(2), face="bold"),
                       strip.text.y = element_text(size=rel(1),angle=0, face="bold"),
                       strip.background = element_rect(fill=NA),
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
                       legend.key = element_rect(fill=NA))
var.labs <- c("AGB (kgC/m2)", "Tree density (trees/m2)", "Mean DBH (cm)", "SD of DBH (cm)")
names(var.labs) <- c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")


runs.late <- runs.yr[runs.yr$year >= 2025, ]
#lme anova analysis of our structural variables and Tukey multiple comparison analysis
mult.df.25 <- data.frame()
mult.df.50 <- data.frame()
mult.df.99 <- data.frame()
struc.var <- c("agb", "density.tree", "tree.dbh.mean", "tree.height.mean", "tree.dbh.sd", "tree.height.sd")
for(RCP in unique(runs.late$rcp)){
  for(COL in struc.var){
    lm.test.25 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2025 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "25"))
    print(anova(lm.test.25))
    #Doing a multiple comparison across the different management types
    mult.list.25 <- list()
    post.hoc <- glht(lm.test.25, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.25[[paste(COL)]]$Var <- COL
    mult.list.25[[paste(COL)]]$rcp <- RCP
    mult.list.25[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.25[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.25 <- dplyr::bind_rows(mult.list.25)
    mult.df.25 <- rbind(mult.df.25, dat.mult.25)
    
    
    lm.test.50 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2050 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "50"))
    print(anova(lm.test.50))
    mult.list.50 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.50, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.50[[paste(COL)]]$Var <- COL
    mult.list.50[[paste(COL)]]$rcp <- RCP
    mult.list.50[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.50[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.50 <- dplyr::bind_rows(mult.list.50)
    mult.df.50 <- rbind(mult.df.50, dat.mult.50)
    
    
    lm.test.99 <- lme(eval(substitute(j ~ Management, list(j = as.name(COL)))), random=list(GCM =~1), data = runs.late[runs.late$year == 2099 & runs.late$rcp == RCP,], method = "ML")
    print(paste(COL, "99"))
    print(anova(lm.test.99))
    mult.list.99 <- list()
    #Doing a multiple comparison across the different management types
    post.hoc <- glht(lm.test.99, linfct = mcp(Management = 'Tukey'))
    output <- summary(post.hoc)
    output$test$pvalues <- ifelse(output$test$pvalues<=.05, paste0(round(output$test$pvalues,5), "*"), round(output$test$pvalues,5))
    mult.list.99[[paste(COL)]]$Var <- COL
    mult.list.99[[paste(COL)]]$rcp <- RCP
    mult.list.99[[paste(COL)]]$Comp <- names(output$test$coefficients)
    mult.list.99[[paste(COL)]]$pvalue <- output$test$pvalues
    dat.mult.99 <- dplyr::bind_rows(mult.list.99)
    mult.df.99 <- rbind(mult.df.99, dat.mult.99)
  }
}
#Creating different dataframes to look at specific windows. This information should evtually end up captured in a figure
rcp45.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp45",], Comp ~ Var)
rcp45.25.df$Scenario <- "Low Emmissions"
rcp45.25.df$year <- "2025"
rcp45.25.df <- rcp45.25.df[,c(8,9,1,2,3,4,5,6,7)]
rcp85.25.df <- reshape2::dcast(mult.df.25[mult.df.25$rcp=="rcp85",], Comp ~ Var)
rcp85.25.df$Scenario <- "High Emmissions"
rcp85.25.df$year <- "2025"
rcp85.25.df <- rcp85.25.df[,c(8,9,1,2,3,4,5,6,7)]

rcp45.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp45",], Comp ~ Var)
rcp45.50.df$Scenario <- "Low Emmissions"
rcp45.50.df$year <- "2050"
rcp45.50.df <- rcp45.50.df[,c(8,9,1,2,3,4,5,6,7)]
rcp85.50.df <- reshape2::dcast(mult.df.50[mult.df.50$rcp=="rcp85",], Comp ~ Var)
rcp85.50.df$Scenario <- "High Emmissions"
rcp85.50.df$year <- "2050"
rcp85.50.df <- rcp85.50.df[,c(8,9,1,2,3,4,5,6,7)]

rcp45.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp45",], Comp ~ Var)
rcp45.99.df$Scenario <- "Low Emmissions"
rcp45.99.df$year <- "2099"
rcp45.99.df <- rcp45.99.df[,c(8,9,1,2,3,4,5,6,7)]
rcp85.99.df <- reshape2::dcast(mult.df.99[mult.df.99$rcp=="rcp85",], Comp ~ Var)
rcp85.99.df$Scenario <- "High Emmissions"
rcp85.99.df$year <- "2099"
rcp85.99.df <- rcp85.99.df[,c(8,9,1,2,3,4,5,6,7)]

struc.comp <- rbind(rcp45.25.df,rcp45.50.df, rcp45.99.df, rcp85.25.df, rcp85.50.df, rcp85.99.df)


plot.struc <- stack(struc.comp[,c("agb", "density.tree", "tree.dbh.mean", "tree.dbh.sd")])
names(plot.struc) <- c("values", "var")
plot.struc[,c("Scenario", "Comp", "year")] <- struc.comp[,c("Scenario", "Comp", "year")]
plot.struc$Comparison <- plot.struc$Comp
plot.struc <- plot.struc %>% tidyr::separate(Comp, c('MNG.1', 'MNG.2'))


MANAGE <- c("None", "Gap", "Shelter", "Under")
Letter <- c("a", "b", "c", "d")
full <- data.frame()
for(SCEN in unique(plot.struc$Scenario)){
  for(YR in unique(plot.struc$year)){
    for(VAR in unique(plot.struc$var)){
      start.df <- plot.struc[plot.struc$Scenario == SCEN & plot.struc$year == YR & plot.struc$var == VAR,]
      mid <- data.frame()
      for(MNG in unique(start.df$MNG.1)){
        temp <- start.df[start.df$MNG.1 == MNG | start.df$MNG.2 == MNG,]
        temp$pair <- ifelse(temp$values >.05, paste0(temp$MNG.1,"-",temp$MNG.2), "sig")
        mid <- rbind(mid,temp)
      }
      
      mid <- mid[!duplicated(mid$Comparison),]
      mng.df <- data.frame(MANAGE, Letter)
      mng.df$Scenario <- SCEN
      mng.df$year <- YR
      mng.df$var <- VAR
      count <- 1
      
      for(mng in unique(MANAGE)){
        for(i in 1:nrow(mid)){
          if(mid[i, "values"] >.05 & mid[i, "MNG.1"] !=mng){
            mid[i, "letters"] <- paste0(mid[i, "letters"], mng.df[mng.df$MANAGE == mid[i, "MNG.1"], "Letter"])
          } else if(mid[i, "values"] >.05 & mid[i, "MNG.2"] !=mng){ #else if to check which column the pair is with
            mid[i, "letters"] <- paste0(mid[i, "letters"], mng.df[mng.df$MANAGE == mid[i, "MNG.2"], "Letter"])
          } else{
            mid[i, "letters"] <- paste0("")
          }
        }
      }
      #for(i in 1:nrow(mid)){
        #if(mid[i , "pair"] != "sig"){
         # mng.df[mng.df$MANAGE == mid[i,"MNG.1"], "letter"] <- paste0( mng.df[mng.df$MANAGE == mid[i,"MNG.1"], "letter"],letters[count])
         # mng.df[mng.df$MANAGE == mid[i,"MNG.2"], "letter"] <- paste0( mng.df[mng.df$MANAGE == mid[i,"MNG.2"], "letter"],letters[count])
         # count <- count + 1
        #} else{
          
        #}
      #}
      
      full <- rbind(full, mng.df)
    }
  }
}

path.figures <- "G:/.shortcut-targets-by-id/0B_Fbr697pd36c1dvYXJ0VjNPVms/MANDIFORE/MANDIFORE_CaseStudy_MortonArb/Drought and heat analysis/Figures/"

theme.clean <-   theme(axis.text = element_text(size=rel(1), color="black"),
                       axis.title = element_text(size=rel(2), face="bold"),
                       panel.background = element_rect(fill=NA, color="black"),
                       panel.grid=element_blank(),
                       # panel.spacing.x = unit(1, "lines"),
                       strip.text.x = element_text(size=rel(1), face="bold"),
                       strip.text.y = element_text(size=rel(1),angle=0, face="bold"),
                       strip.background = element_rect(fill=NA),
                       strip.placement = "outside",
                       plot.margin = unit(c(1, 1, 1, 1), "lines"),
                       plot.title = element_text(size=rel(2), face="bold", hjust=0.5),
                       axis.title.y = element_blank())


#Supplemental figure of pre and post harvest
png(paste0(path.figures, "HarvestStructure_Pre-Post.png"), width=12, height=8, units="in", res=220)
ggplot(data=dat.harvest[dat.harvest$time == "pre-harvest" | dat.harvest$time == "post-harvest",]) +
  facet_grid(ind~rcp, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
  geom_boxplot(aes(x=as.factor(year), y=values, fill=Management)) +
  scale_x_discrete(name="Time", labels=c("pre-harvest", "post-harvest")) +
  scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean+
  ggtitle("Pre and Post harvest structure by rcp scenario")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(strip.text.y.left = element_text(angle = 60))
dev.off()

#Figure for paper that use mid and end of century
png(paste0(path.figures, "HarvestStructure_Mid-End.png"), width=12, height=8, units="in", res=220)
ggplot(data=dat.harvest[dat.harvest$time == "mid-century" | dat.harvest$time == "end-century",]) +
  facet_grid(ind~rcp, scales="free_y", labeller = labeller(ind = var.labs), switch = "y") +
  geom_boxplot(aes(x=as.factor(year), y=values, fill=Management)) +
  scale_x_discrete(name="Time", labels=c("mid-century", "end-century")) +
  scale_fill_manual(values=c("None"="#1f78b4", "Under"="#a6cee3", "Shelter"="#33a02c", "Gap"="#b2df8a")) +
  theme.clean+
  ggtitle("Mid-century and end-century forest structure by management and rcp scenario")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(strip.text.y.left = element_text(angle = 60))
dev.off()

vars.plot <- c("tair", "precip.total", "VPD")

#runs.yr$tair <- runs.yr$tair-273.15
dat.weather <- stack(runs.yr[, vars.plot])
dat.weather[dat.weather$ind=="tair","values"] <- dat.weather[dat.weather$ind=="tair","values"] - 273.15
dat.weather[,c("GCM", "rcp", "RCP.name", "Management", "year")] <- runs.yr[, c("GCM", "rcp", "RCP.name", "Management", "year")]

dat.wagg <- aggregate(values~ind+GCM+rcp+RCP.name+year, dat.weather, FUN = mean)

dat.means <- aggregate(values~ind+rcp+RCP.name, dat.weather[dat.weather$year<2020,], FUN = mean)

met.labs <- c("Temperature (C)", "Total precip (mm)", "VPD (Pa)")
names(met.labs) <- c("tair", "precip.total", "VPD")


weath.time <- ggplot(data=dat.wagg)+
  facet_grid(ind~rcp, scales= "free_y", labeller = labeller(ind = met.labs)) +
  geom_line(aes(x=year, y=values, group=GCM)) +
  geom_hline(data = dat.means, aes(yintercept=values), color="red2", linetype="dashed", size=1)+
  labs(x="Year", y="Weather Metrics") +
  theme(axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size=rel(2), face="bold"),
        strip.text.y = element_text(size=rel(1.5), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))


weath.cent <- ggplot(data=dat.wagg[dat.wagg$year == 2050 | dat.wagg$year == 2099,])+
  facet_grid(ind~., scales= "free_y", labeller = labeller(ind = met.labs)) +
  geom_boxplot(aes(x=as.character(year), y=values, fill = rcp)) +
  labs(x="Year", y="Weather Metrics")+
  scale_x_discrete(name="Period", labels=c("mid-century", "end of century")) +
  #ggpubr::stat_compare_means(aes(x=as.character(year), y=values, fill = rcp), method = "t.test")+
  theme(axis.text.y = element_text(size=rel(2), color="black"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=rel(1.5), color="black"),
        axis.title.x = element_text(size=rel(2), color="black", face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text.x = element_text(size=rel(2), face="bold"),
        strip.text.y = element_text(size=rel(1.5), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5))

t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "tair", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "tair", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "precip.total", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85" & dat.wagg$ind== "precip.total", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "VPD", "values"], dat.wagg[dat.wagg$year==2050 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "VPD", "values"] , paired=T)

t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "tair", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "tair", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "precip.total", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85" & dat.wagg$ind== "precip.total", "values"] , paired=T)
t.test(dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp45" & dat.wagg$ind== "VPD", "values"], dat.wagg[dat.wagg$year==2099 & dat.wagg$rcp== "rcp85"& dat.wagg$ind== "VPD", "values"] , paired=T)

plot_row <- cowplot::plot_grid(weath.time, weath.cent, labels = c("A", "B"), label_size = 15 ,rel_widths = c(2,1))

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Climate Change Over Time by RCP Scenario",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 32
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 150)
  )
weath.plot <- cowplot::plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

png(width= 1000, filename= file.path(path.figures, paste0('Climate_Change_Over_Time.png')))
  weath.plot
dev.off()

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