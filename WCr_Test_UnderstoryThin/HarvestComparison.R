# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

dat.cohort.all <- data.frame()
dat.patch.all <- data.frame()
# dat.site.all <- data.frame()
for(HARV in c("None2", "None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly")){
  files.over <- dir(paste0("Harvest",HARV,"/analy/"), "-Y-")
  # files.over
  
  for(FILE in files.over){
    yr.now <- as.numeric(strsplit(FILE, "-")[[1]][3])
    mo.now <- as.numeric(strsplit(FILE, "-")[[1]][4])
    
    fnow <- ncdf4::nc_open(file.path(paste0("Harvest", HARV), "analy", FILE)) 
    # summary(fnow$var)
    
    # dat.ste <- data.frame(Harvest=HARV,
    #                       year=yr.now,
    #                       AGB = sum(ncdf4::ncvar_get(fnow, "AGB_PY")),
    #                       BA = sum(ncdf4::ncvar_get(fnow, "BASAL_AREA_PY")))
    # tst <- ncdf4::ncvar_get(fnow, "AGE")
    df.pch <- data.frame(Harvest=HARV,
                         year=yr.now,
                         month=mo.now,
                         patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"),
                         n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                         patch.area = ncdf4::ncvar_get(fnow, "AREA"),
                         patch.age = ncdf4::ncvar_get(fnow, "AGE"))
    df.pch$patchID <- 1:nrow(df.pch)
    
    df.co <- data.frame(Harvest=HARV,
                        year=yr.now,
                        month=mo.now,
                        PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")),
                        DBH=ncdf4::ncvar_get(fnow, "DBH"),
                        AGB=ncdf4::ncvar_get(fnow, "AGB_CO"),
                        Density=ncdf4::ncvar_get(fnow, "NPLANT"))
    
    ncdf4::nc_close(fnow)
    
    for(i in 1:nrow(df.pch)){
      df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("patchID", "patch.area", "patch.age")] <- df.pch[i,c("patchID", "patch.area", "patch.age")]
    }
    tail(df.co)
    
    df.co$dens.wt <- df.co$Density*df.co$patch.area
    df.co$DBH.rnd <- round(df.co$DBH*5, -1)/5 # Roudning to closest 2 cm  just to help reduce dimensions
    # df.co <- df.co[df.co$PFT!=5 & df.co$DBH>5,] # Ignore grasses and things <5 cm DBH
    summary(df.co)
    
    df.co2 <- aggregate(df.co[,c("Density", "dens.wt")],
                        by=df.co[,c("Harvest", "year", "month", "PFT", "DBH.rnd", "patchID")],
                        FUN=sum)
    # names(df.co)[3] <- "dens.wt"
    summary(df.co2)
    df.co2[,c("AGB", "patch.area", "patch.age")] <- aggregate(df.co[,c("AGB", "patch.area", "patch.age")],
                                   by=df.co[,c("Harvest", "year", "month", "PFT", "DBH.rnd", "patchID")],
                                   FUN=mean)[,c("AGB", "patch.area", "patch.age")]
    
    
    # dat.site.all <- rbind(dat.site.all, dat.ste)
    
    dat.cohort.all <- rbind(dat.cohort.all, df.co2)
    dat.patch.all <- rbind(dat.patch.all, df.pch)
    
    
  } # end file loop
}# End harvest scheme
dat.cohort.all$Harvest <- factor(dat.cohort.all$Harvest, levels=c("None2", "None", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly", "All"))
dat.patch.all$Harvest <- factor(dat.patch.all$Harvest, levels=c("None2", "None", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly", "All"))
# dat.site.all$Harvest <- factor(dat.site.all$Harvest, levels=c("None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly"))
summary(dat.cohort.all)
summary(dat.patch.all)
# summary(dat.site.all)


# ggplot(data=dat.site.all) +
#   geom_line(aes(x=year, y=AGB, color=Harvest))


patch.chk <- aggregate(dat.patch.all[,c("patch.area", "n.cohorts")],
                       by=dat.patch.all[,c("year", "Harvest")],
                       FUN=sum)
summary(patch.chk)
ggplot(data=dat.patch.all) +
  facet_wrap(~Harvest) +
  geom_line(aes(x=year, y=n.cohorts, color=as.factor(patchID)))
ggplot(data=dat.patch.all) +
  facet_wrap(~Harvest) +
  geom_line(aes(x=year, y=patch.area, color=as.factor(patchID)))
ggplot(data=dat.patch.all) +
  facet_wrap(~Harvest) +
  geom_line(aes(x=year, y=patch.age, color=as.factor(patchID)))

ggplot(data=dat.patch.all) +
  facet_wrap(~Harvest, ncol=2) +
  geom_histogram(aes(x=year, weight=patch.area, fill=as.factor(round(patch.age*2,-1)/2)), binwidth=1) +
  scale_fill_discrete(name="Age")


ggplot(data=dat.patch.all) +
  facet_wrap(~year) +
  geom_histogram(aes(x=patch.age, weight=patch.area, fill=as.factor(Harvest)))

summary(dat.cohort.all)
dat.cohort.all$AGB.wt <- dat.cohort.all$AGB * dat.cohort.all$dens.wt

dat.cohorts <- aggregate(dat.cohort.all[,c("Density", "dens.wt", "AGB.wt")],
                         by=dat.cohort.all[,c("Harvest", "year", "PFT", "DBH.rnd")],
                         FUN=sum)
summary(dat.cohorts)

# summary(dat.patches)

ggplot(data=dat.patch.all[,]) +
  facet_wrap(~Harvest) +
  geom_histogram(aes(x=year, weight=patch.area, fill=as.factor(patchID)),binwidth=1) +
  # geom_line(aes(x=year, y=patch.age, color=as.factor(patchID))) +
  # geom_vline(xintercept=30, linetype="dashed") +
  scale_y_continuous(name="density", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")


ggplot(data=dat.patch.all[,]) +
  facet_wrap(~year) +
  geom_bar(aes(x=Harvest, weight=patch.area, fill=as.factor(patchID))) +
  # geom_line(aes(x=year, y=patch.age, color=as.factor(patchID))) +
  # geom_vline(xintercept=30, linetype="dashed") +
  scale_y_continuous(name="density", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")

ggplot(data=dat.cohort.all[dat.cohort.all$PFT!=5 & dat.cohort.all$DBH.rnd>=10,]) +
  facet_wrap(~Harvest, ncol=2) +
  geom_histogram(aes(x=patch.age, weight=AGB.wt, fill=PFT))

ggplot(data=dat.cohorts[dat.cohorts$PFT!=5 & dat.cohorts$DBH.rnd>=10 & dat.cohorts$year<2015,]) +
  facet_grid(Harvest ~ year) +
  geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=AGB.wt),binwidth=2) +
  geom_vline(xintercept=30, linetype="dashed") +
  scale_y_continuous(name="AGB", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")

pdf("HarvestDifferences_AGB.pdf")
for(YR in min(dat.cohorts$year):max(dat.cohorts$year)){
  HARV <- ifelse(YR<=2011 & YR>2006, "Harvest", "Recovery")
  print(
    ggplot(data=dat.cohorts[dat.cohorts$PFT!=5 & dat.cohorts$DBH.rnd>=1 & dat.cohorts$year==YR,]) +
      ggtitle(paste0(YR, " - ", HARV )) +
      facet_wrap(~Harvest, ncol=2) +
      geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=AGB.wt),binwidth=2) +
      geom_vline(xintercept=30, linetype="dashed") +
      scale_x_continuous(name="DBH", limits=c(1, max(dat.cohorts$DBH.rnd[dat.cohorts$PFT!=5]))) +
      scale_y_continuous(name="AGB (kg/m2)", expand=c(0,0), limits=range(0,5)) +
      theme_bw() +
      theme(legend.position="top")
  )  
}
dev.off()


pdf("HarvestDifferences_Density.pdf")
for(YR in min(dat.cohorts$year):max(dat.cohorts$year)){
  HARV <- ifelse(YR<=2011 & YR>2006, "Harvest", "Recovery")
  print(
    ggplot(data=dat.cohorts[dat.cohorts$PFT!=5 & dat.cohorts$DBH.rnd>=1 & dat.cohorts$year==YR,]) +
      ggtitle(paste0(YR, " - ", HARV )) +
      facet_wrap(~Harvest, ncol=2) +
      geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=dens.wt),binwidth=2) +
      geom_vline(xintercept=30, linetype="dashed") +
      scale_y_continuous(name="density", expand=c(0,0), limits=range(0,0.1)) +
      scale_x_continuous(name="DBH", limits=c(1, max(dat.cohorts$DBH.rnd[dat.cohorts$PFT!=5]))) +
      theme_bw() +
      theme(legend.position="top")
  )  
}
dev.off()
# ggplot(data=dat.cohort.all[dat.cohort.all$year==2006 & dat.cohort.all$PFT!=5 & dat.cohort.all$Harvest=="None",]) +
#   facet_grid(Harvest ~ patchID) +
#   geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=Density),binwidth=2) +
#   geom_vline(xintercept=30, linetype="dashed") +
#   geom_text(aes(x=40, y=0.075, label=round(patch.area,2))) +
#   geom_text(aes(x=40, y=0.05, label=round(patch.age,0))) +
#   scale_y_continuous(name="density", expand=c(0,0), limits=c(0,0.1)) +
#   theme_bw() +
#   theme(legend.position="top")
