# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

dat.cohort.all <- data.frame()
dat.patch.all <- data.frame()
dat.site.all <- data.frame()
for(HARV in c("None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly")){
  files.over <- dir(paste0("Harvest",HARV,"/analy/"), "-Y-")
  # files.over
  
  for(FILE in files.over){
    yr.now <- as.numeric(strsplit(FILE, "-")[[1]][3])
    mo.now <- as.numeric(strsplit(FILE, "-")[[1]][4])
    
    fnow <- ncdf4::nc_open(file.path(paste0("Harvest", HARV), "analy", FILE)) 
    # summary(fnow$var)
    
    dat.ste <- data.frame(Harvest=HARV,
                          year=yr.now,
                          AGB = sum(ncdf4::ncvar_get(fnow, "TOTAL_AGB")),
                          BA = sum(ncdf4::ncvar_get(fnow, "TOTAL_BASAL_AREA")))
    # tst <- ncdf4::ncvar_get(fnow, "AGE")
    df.pch <- data.frame(Harvest=HARV,
                         year=yr.now,
                         patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"),
                         n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                         patch.area = ncdf4::ncvar_get(fnow, "AREA"),
                         patch.age = ncdf4::ncvar_get(fnow, "AGE"))
    df.pch$patchID <- 1:nrow(df.pch)
    
    df.co <- data.frame(Harvest=HARV,
                        year=yr.now,
                        PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")),
                        DBH=ncdf4::ncvar_get(fnow, "DBH"),
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
    
    # df.co <- aggregate(df.co[,c("dens.wt")],
    #                     by=df.co[,c("PFT", "DBH.rnd")],
    #                     FUN=sum)
    # names(df.co)[3] <- "dens.wt"
    dat.cohort.all <- rbind(dat.cohort.all, df.co)
    dat.patch.all <- rbind(dat.patch.all, df.pch)
    dat.site.all <- rbind(dat.site.all, dat.ste)
  } # end file loop
}# End harvest scheme
dat.cohort.all$Harvest <- factor(dat.cohort.all$Harvest, levels=c("None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly"))
dat.patch.all$Harvest <- factor(dat.patch.all$Harvest, levels=c("None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly"))
dat.site.all$Harvest <- factor(dat.site.all$Harvest, levels=c("None", "All", "Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly"))
summary(dat.cohort.all)
summary(dat.patch.all)
summary(dat.site.all)


# ggplot(data=dat.site.all) +
#   geom_line(aes(x=year, y=AGB, color=Harvest))


patch.chk <- aggregate(dat.patch.all[,c("patch.area", "n.cohorts")],
                       by=dat.patch.all[,c("year", "Harvest")],
                       FUN=sum)
summary(patch.chk)

dat.patches <- aggregate(dat.harvest[,c("patch.area", "patch.age")],
                         by=dat.harvest[,c("Harvest", "year", "patchID")],
                         FUN=mean)
dat.cohorts <- aggregate(dat.cohort.all[,c("Density", "dens.wt")],
                         by=dat.cohort.all[,c("Harvest", "year", "PFT", "DBH.rnd")],
                         FUN=sum)

summary(dat.patches)
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

ggplot(data=dat.cohorts[,]) +
  facet_grid(Harvest ~ year) +
  geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=dens.wt),binwidth=2) +
  geom_vline(xintercept=30, linetype="dashed") +
  scale_y_continuous(name="density", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")


ggplot(data=dat.cohort.all[dat.cohort.all$year==2009 & dat.cohort.all$PFT!=5,]) +
  facet_grid(Harvest ~ patchID) +
  geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=Density),binwidth=2) +
  geom_vline(xintercept=30, linetype="dashed") +
  geom_text(aes(x=40, y=0.075, label=round(patch.area,2))) +
  geom_text(aes(x=40, y=0.05, label=round(patch.age,0))) +
  scale_y_continuous(name="density", expand=c(0,0), limits=c(0,0.1)) +
  theme_bw() +
  theme(legend.position="top")
