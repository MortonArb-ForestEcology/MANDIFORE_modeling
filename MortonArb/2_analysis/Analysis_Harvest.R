# -----------------------------------------------------------
# Doing some better analyses of the impacts of climate change and harvest on oak ecosystems
# Using the static CO2 output because it is probably more reasonable
# -----------------------------------------------------------

library(ggplot2); library(gganimate)

path.google <- "/Volumes/GoogleDrive/My Drive/MANDIFORE/MANDIFORE_CaseStudy_MortonArb"
path.figs <- file.path(path.google, "figures")


# -----------------------------------------------------------
# Read in and play with the data
# -----------------------------------------------------------
dat.co <- read.csv(file.path("../data/Summary_PFTs_Cohort_Year.csv"))
dat.co <- dat.co[dat.co$CO2=="static" & dat.co$PFT!=5,]
dat.co$Management <- factor(dat.co$Management, levels=c("None", "Under", "Shelter", "Gap"))
dat.co$PFT <- car::recode(dat.co$PFT, "'9'='Early Succ.'; '10'='Mid. Succ.'; '11'='Late Succ.'")
dat.co$PFT <-factor(dat.co$PFT, levels=c("Early Succ.", "Mid. Succ.", "Late Succ."))
dat.co$RCP.name <- car::recode(dat.co$RCP, "'rcp45'='Low Change'; 'rcp85'='High Change'")
dat.co$RCP.name <- factor(dat.co$RCP.name, levels=c("Low Change", "High Change"))
summary(dat.co)


dbh.dist <- ggplot(data=dat.co[dat.co$PFT!=5 & dat.co$DBH>10,])+
  facet_grid(Management ~ RCP.name) +
  geom_histogram(aes(x=DBH, fill=as.factor(PFT), weight=dens.wt*1e4), binwidth=5) +
  coord_cartesian(ylim=c(0,125), expand=F) +
  scale_y_continuous(name="Stems per Hectare", breaks=c(0,50,100,150)) +
  scale_x_continuous(name="DBH (cm)", expand=c(0,0)) +
  scale_fill_manual(name="Tree Type", values=c("#1b9e77", "#d95f02", "#7570b3")) +
  theme(legend.position="top",
        legend.title = element_text(size=rel(2), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.key.size = unit(1.5, "lines"),
        axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        # panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        # strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5)) +
  transition_time(year) + labs(title= "Year: {frame_time}") 

# animate(dbh.dist, end_pause=25, start_pause = 25)

anim_save(filename="Model_Size_Histogram_PFT_time.gif", animation = dbh.dist, end_pause=5, start_pause=5, fps=5, path=path.figs, height=7.5, width=10, units="in", res=180)
# -----------------------------------------------------------


# -----------------------------------------------------------
# Looking at some of the higher-level variables
# -----------------------------------------------------------
dat.pft <- read.csv(file.path("../data/Summary_PFTs_Site_Year.csv"))
dat.pft$PFT <- car::recode(dat.pft$PFT, "'9'='Early Succ.'; '10'='Mid. Succ.'; '11'='Late Succ.'")
dat.pft$PFT <-factor(dat.pft$PFT, levels=c("Early Succ.", "Mid. Succ.", "Late Succ."))
dat.pft$Management <- factor(dat.pft$Management, levels=c("None", "Under", "Shelter", "Gap"))
summary(dat.pft)

dat.yr <- read.csv(file.path(path.google, "output", "Summary_Site_Year.csv"))
dat.yr$Management <- factor(dat.yr$Management, levels=c("None", "Under", "Shelter", "Gap"))
summary(dat.yr); dim(dat.yr)

dat.yr <- merge(dat.yr, dat.pft[dat.pft$PFT=="Mid. Succ.", c("RunID", "year", "AGB.g45", "Density.g45", "Stress.g45", "Density.sap", "Stress.sap", "AGB.tot", "BA.tot", "Density.Tot", "AGB.prop", "BA.prop")], all.x=T)
dat.yr$RCP.name <- car::recode(dat.yr$RCP, "'rcp45'='Low Change'; 'rcp85'='High Change'")
dat.yr$RCP.name <- factor(dat.yr$RCP.name, levels=c("Low Change", "High Change"))
summary(dat.yr)

# Calculating expected Oak mortality
# expmort = max( lnexp_min, min( lnexp_max                                             &
#                                  , mort2(ipft) * ( cpatch%cbr_bar(ico) - mort0(ipft) ) ) )
# cpatch%mort_rate(2,ico) = mort1(ipft) / (1. + exp(expmort))
mort0.10=0
mort1.10=1
mort2.10=20
expmort <- mort2.10*(dat.yr$Stress.g45-mort0.10)
dat.yr$MortRate.g45 <- mort1.10/(1+exp(expmort))
summary(dat.yr)

stress <- seq(0, 1, by=0.1)
mort <- mort2.10*(stress-mort0.10)
plot(x=stress, y=(mort1.10/(1+exp(mort))))

png(file.path(path.figs, "Harvest_AGB_Tot_year.png"), height=6, width=9.5, units="in", res=180)
ggplot(data=dat.yr[dat.yr$CO2.type=="static",]) +
  facet_wrap(~RCP.name) +
  geom_rect(xmin=2020, xmax=2025, ymin=-Inf, ymax=Inf, fill="gray30") +
  geom_text(x=2026, y=25, label="Harvest\n(2020-2024)", hjust=0, vjust=1, color="gray30", size=8) +
  geom_line(aes(x=year, y=AGB.tot, color=Management), size=2) + 
  scale_color_manual(values=c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a")) +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  scale_y_continuous(name="Biomass (kg/m2)") +
  theme(legend.position="top",
        legend.title = element_text(size=rel(2), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.key.size = unit(1.5, "lines"),
        legend.key = element_rect(fill=NA),
        axis.text = element_text(size=rel(1.5), color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(3, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 2, 1, 1.5), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5)) 
dev.off()


aov.stress <- nlme::lme(Stress.g45 ~ Management, random=list(RCP=~1, year=~1), data=dat.yr)
summary(aov.stress)

png(file.path(path.figs, "Harvest_Stress_PFT10_g45.png"), height=6, width=9.5, units="in", res=180)
ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=Stress.g45, fill=Management, color=Management), alpha=0.5) +
  scale_y_continuous(name="Stress", breaks=c(0.5, 0.75, 1), labels = c("0.50\n(Stressed)", "0.75\n(Not Happy)", "1.00\n(Happy)")) +
  scale_color_manual(values=c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a")) +
  scale_fill_manual(values=c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a")) +
  guides(fill=F, color=F) +
  theme(legend.position="top",
        legend.title = element_text(size=rel(2), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.key.size = unit(1.5, "lines"),
        legend.key = element_rect(fill=NA),
        axis.text.y = element_text(size=rel(1.5), color="black"),
        axis.text.x = element_text(size=rel(1.5), hjust=0, angle=-15, color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 2, 1, 1.5), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5)) 
dev.off()

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  geom_point(aes(x=temp.ground, y=Stress.g45, color=Management))

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=temp.ground, fill=Management, color=Management), alpha=0.5)

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=water.deficit, fill=Management, color=Management), alpha=0.5)

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=runoff, fill=Management, color=Management), alpha=0.5)

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=par.ground, fill=Management, color=Management), alpha=0.5)

ggplot(data=dat.yr) +
  facet_wrap(~RCP.name) +
  # geom_hline(yintercept=c(0.5, 0.75, 1), linetype="dashed", alpha=0.5) +
  geom_boxplot(aes(x=Management, y=water.deficit, fill=Management, color=Management), alpha=0.5) +
  # scale_y_continuous(name="Stress", breaks=c(0.5, 0.75, 1), labels = c("0.50\n(Stressed)", "0.75\n(Not Happy)", "1.00\n(Happy)")) +
  scale_color_manual(values=c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a")) +
  scale_fill_manual(values=c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a")) +
  guides(fill=F, color=F) +
  theme(legend.position="top",
        legend.title = element_text(size=rel(2), face="bold"),
        legend.text = element_text(size=rel(1.5)),
        legend.key.size = unit(1.5, "lines"),
        legend.key = element_rect(fill=NA),
        axis.text.y = element_text(size=rel(1.5), color="black"),
        axis.text.x = element_text(size=rel(1.5), hjust=0, angle=-15, color="black"),
        axis.title = element_text(size=rel(2), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_text(size=rel(2), face="bold"),
        strip.background = element_rect(fill=NA),
        plot.margin = unit(c(1, 2, 1, 1.5), "lines"),
        plot.title = element_text(size=rel(2), face="bold", hjust=0.5)) 



# -----------------------------------------------------------
