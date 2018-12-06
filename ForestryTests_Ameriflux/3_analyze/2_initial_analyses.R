# Doing some end-of-simulation comparisons
library(ggplot2)

path.figs <- "/Volumes/GoogleDrive/My Drive/Conferences_Presentations/AGU 2018/figures"

# Load the data frame
dat.all <- read.csv("../2_runs/extracted_output.v3/MANDIFORE_Ameriflux_month_site_last10.csv")
summary(dat.all)

# Convert values to per year instead per second
s2yr <- 60*60*24*365 # Seconds in a year
vars.flux <- c("Rainf", "Fire_flux", "NEE", "NPP", "Runoff")
dat.all[,vars.flux] <- dat.all[,vars.flux] * s2yr
dat.all$management <- factor(dat.all$management, levels=c("passive", "preservation", "ecological", "production"))
summary(dat.all)

# Aggregate to annual values
vars.met <- c("CO2air", "Tair", "Rainf")
vars.eco <- c("Fire_flux", "NEE", "NPP", "Albedo", "Runoff", "SoilMoist", "AbvGrndBiom", "LAI", "Density_Tree", "BasalArea_Tree")
vars.fac <- c("RunID", "site", "management", "GCM", "scenario", "year")
dat.yr <- aggregate(dat.all[,c(vars.met, vars.eco)], 
                    by=dat.all[,vars.fac],
                    FUN=mean)
summary(dat.yr)

dat.yr2 <- stack(dat.yr[,c(vars.met, vars.eco)])
dat.yr2[,vars.fac] <- dat.yr[,vars.fac]
summary(dat.yr2)

ggplot(data=dat.yr2[dat.yr2$site=="HARVARD_EMS" & dat.yr2$ind %in% c("AbvGrndBiom", "NEE"),]) +
  facet_wrap(~ind, scales="free_y") +
  geom_boxplot(aes(x=management, y=values, fill=scenario)) +
  theme_bw()

for(SITE in unique(dat.yr$site)){
  png(file.path(path.figs, paste0(SITE, "_AGB_mean", "_2090-2100.png")), height=7.5, width=6, units="in", res=180)
  print(
    ggplot(data=dat.yr[dat.yr$site==SITE,]) +
      # facet_wrap(~site) +
      geom_boxplot(aes(x=management, y=AbvGrndBiom, fill=scenario)) +
      scale_fill_manual(name="Scenario",values=c("#0072B2", "#E69F00")) +
      scale_x_discrete(labels=c("Pass.", "Pres.", "Ecol.", "Prod.")) +
      # expression(bold(paste("Precipitation (mm yr"^"-1", ")")))
      scale_y_continuous(name=expression(bold(paste("Aboveground Biomass (kgC m"^"-2", ")")))) +
      theme_bw() +
      theme(legend.position=c(0.85,0.85),
            legend.direction="vertical",
            legend.key.size=unit(2.5, "lines"),
            legend.text = element_text(size=rel(1.25)),
            legend.title = element_text(size=rel(1.5), face="bold"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=rel(2), color="black"),
            axis.text=element_text(size=rel(2), color="black"))
  )
  dev.off()
  
  png(file.path(path.figs, paste0(SITE, "_NEE_mean", "_2090-2100.png")), height=7.5, width=6, units="in", res=180)
  print(
    ggplot(data=dat.yr[dat.yr$site==SITE,]) +
      # facet_wrap(~site) +
      geom_hline(yintercept=0, size=0.25) +
      geom_boxplot(aes(x=management, y=NEE, fill=scenario)) +
      scale_fill_manual(name="Scenario",values=c("#0072B2", "#E69F00")) +
      scale_x_discrete(labels=c("Pass.", "Pres.", "Ecol.", "Prod.")) +
      # expression(bold(paste("Precipitation (mm yr"^"-1", ")")))
      scale_y_continuous(name=expression(bold(paste("NEE (kgC m"^"-2", " s"^"-1", ")")))) +
      guides(fill=F) +
      theme_bw() +
      theme(legend.position=c(0.85,0.85),
            legend.direction="horizontal",
            legend.key.size=unit(2.5, "lines"),
            legend.text = element_text(size=rel(1.25)),
            legend.title = element_text(size=rel(1.5), face="bold"),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=rel(2), color="black"),
            axis.text=element_text(size=rel(2), color="black"))
  )
  dev.off()
  
}


library(lme4)
library(nlme)
# mod.agb <- lmer(AbvGrndBiom ~ management*scenario-1 + (1|site), data=dat.yr)
mod.agb <- lme(AbvGrndBiom ~ management*scenario-1, random=list(site=~1, year=~1), data=dat.yr)
summary(mod.agb)
anova(mod.agb)

mod.agb.sp3 <- lme(AbvGrndBiom ~ management*scenario, random=list(site=~1, year=~1), data=dat.yr[dat.yr$site=="AUSTINCARY",])
summary(mod.agb.sp3)
anova(mod.agb.sp3)

mod.nee.sp3 <- lme(NEE ~ management*scenario, random=list(site=~1, year=~1), data=dat.yr[dat.yr$site=="AUSTINCARY",])
summary(mod.nee.sp3)
anova(mod.nee.sp3)

mod.nee.nr1 <- lme(NEE ~ management*scenario, random=list(site=~1, year=~1), data=dat.yr[dat.yr$site=="NIWOTRIDGE",])
summary(mod.nee.nr1)
anova(mod.nee.nr1)

mod.nee <- lme(NEE ~ management*scenario, random=list(site=~1, year=~1), data=dat.yr)
# summary(mod.nee)
anova(mod.nee)
