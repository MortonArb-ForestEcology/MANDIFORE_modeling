# Doing some end-of-simulation comparisons
library(ggplot2)

path.figs <- "/Volumes/GoogleDrive/My Drive/Conferences_Presentations/AGU 2018/figures"

# Load the data frame
dat.all <- read.csv("../2_runs/extracted_output.v3/MANDIFORE_Ameriflux_month_site.csv")
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

# -----------------------------------------------------------
# Plots of drivers -- all together and at separate sites
# -----------------------------------------------------------
dat.clim <- dat.yr[,c(vars.fac, vars.met)]
dat.clim$Tair <- dat.clim$Tair - 273.15
dat.clim$Rainf <- dat.clim$Rainf*0.001
dat.clim[,paste0("d", vars.met)] <- NA
for(RUN in unique(dat.clim$RunID)){
  row.ind <- which(dat.clim$RunID==RUN)
  clim.ref <- dat.clim[dat.clim$RunID==RUN & dat.clim$year==2006,vars.met]
  
  dat.clim[row.ind,paste0("d", vars.met)] <- sweep(dat.clim[row.ind, vars.met], 2, as.numeric(clim.ref), "-")
}
summary(dat.clim)

dat.clim2 <- stack(dat.clim[,vars.met])
names(dat.clim2) <- c("raw", "ind")
dat.clim2$diff <- stack(dat.clim[,paste0("d",vars.met)])[,1]
dat.clim2[,vars.fac] <- dat.clim[,vars.fac]
summary(dat.clim2)

clim.region <- aggregate(dat.clim2[,c("raw", "diff")], 
                         by = dat.clim2[,c("year", "ind", "scenario")],
                         FUN=mean)
clim.region[,c("raw.lo", "diff.lo")] <- aggregate(dat.clim2[,c("raw", "diff")], 
                                                  by = dat.clim2[,c("year", "ind", "scenario")],
                                                  FUN=min)[,c("raw", "diff")]
clim.region[,c("raw.hi", "diff.hi")] <- aggregate(dat.clim2[,c("raw", "diff")], 
                                                  by = dat.clim2[,c("year", "ind", "scenario")],
                                                  FUN=max)[,c("raw", "diff")]
summary(clim.region)
# clim.region[clim.region$ind=="CO2air",]

# Adding the reference point back in
for(IND in unique(clim.region$ind)){
  row.ind <- clim.region$ind==IND
  ref <- clim.region[clim.region$year==2006 & clim.region$ind==IND,"raw"]
  
  clim.region[row.ind, "diff.lo"] <- clim.region[row.ind, "diff.lo"]+ref
  clim.region[row.ind, "diff.hi"] <- clim.region[row.ind, "diff.hi"]+ref
}

vars.smooth <- c("raw", "diff.lo", "diff.hi")
for(IND in unique(clim.region$ind)){
  for(RCP in unique(clim.region$scenario)){
    row.ind <- which(clim.region$ind==IND & clim.region$scenario==RCP)
    
    for(VAR in vars.smooth){
      clim.region[row.ind, paste0(VAR, ".05")] <- zoo::rollapply(clim.region[row.ind, VAR], 05, mean, fill=NA, align="right")
      clim.region[row.ind, paste0(VAR, ".10")] <- zoo::rollapply(clim.region[row.ind, VAR], 10, mean, fill=NA, align="right")
    }
  }
}

clim.region$ind <- car::recode(clim.region$ind, "'CO2air'='CO2 (ppm)'; 'Rainf'='Precip. (mm/yr)'; 'Tair'='Temp. (deg. C)'")
clim.region$ind <- factor(clim.region$ind, levels=c("CO2 (ppm)", "Temp. (deg. C)", "Precip. (mm/yr)"))
ggplot(data=clim.region[,]) +
  facet_grid(ind~., scales="free_y") +
  # facet_grid(ind ~ site, scales="free_y") +
  geom_ribbon(aes(x=year, ymin=diff.lo, ymax=diff.hi, fill=scenario), alpha=0.5) +
  geom_line(aes(x=year, y=raw, color=scenario)) +
  theme_bw()

png(file.path(path.figs, "MetDrivers_smooth05.png"), height=7.5, width=6, units="in", res=180)
ggplot(data=clim.region[,]) +
  facet_grid(ind~., scales="free_y") +
  # facet_grid(ind ~ site, scales="free_y") +
  annotate("rect", xmin=2090, xmax=2100, ymin=-Inf, ymax=Inf, alpha=0.5) +
  geom_ribbon(aes(x=year, ymin=diff.lo.05, ymax=diff.hi.05, fill=scenario), alpha=0.5) +
  geom_line(aes(x=year, y=raw.05, color=scenario), size=1.5) +
  geom_vline(xintercept=2090, linetype="dashed", size=2) +
  scale_fill_manual(name="Scenario",values=c("#0072B2", "#E69F00"), labels=c("rcp45\n(low)", "rcp85\n(high)")) +
  scale_color_manual(name="Scenario",values=c("#0072B2", "#E69F00"), labels=c("rcp45\n(low)", "rcp85\n(high)")) +
  scale_x_continuous(name="Year", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top",
        # legend.direction="vertical",
        legend.key.size=unit(2.5, "lines"),
        legend.text = element_text(size=rel(1.25)),
        legend.title = element_text(size=rel(1.5), face="bold"),
        strip.text = element_text(size=rel(1.5), face="bold"),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=rel(1.5), color="black"),
        axis.text=element_text(size=rel(1.5), color="black"))
dev.off()

# -----------------------------------------------------------

# -----------------------------------------------------------
# Plots of Ecosystem Model Output
# -----------------------------------------------------------
for(SITE in unique(dat.yr$site)){
  png(file.path(path.figs, paste0(SITE, "_AGB_mean", "_2090-2100.png")), height=7.5, width=6, units="in", res=180)
  print(
    ggplot(data=dat.yr[dat.yr$site==SITE & dat.yr$year>=2090,]) +
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
    ggplot(data=dat.yr[dat.yr$site==SITE & dat.yr$year>=2090,]) +
      # facet_wrap(~site) +
      geom_hline(yintercept=0, size=0.25) +
      geom_boxplot(aes(x=management, y=NEE, fill=scenario)) +
      scale_fill_manual(name="Scenario",values=c("#0072B2", "#E69F00")) +
      scale_x_discrete(labels=c("Pass.", "Pres.", "Ecol.", "Prod.")) +
      # expression(bold(paste("Precipitation (mm yr"^"-1", ")")))
      scale_y_continuous(name=expression(bold(paste("NEE (kgC m"^"-2", " yr"^"-1", ")")))) +
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
# -----------------------------------------------------------
