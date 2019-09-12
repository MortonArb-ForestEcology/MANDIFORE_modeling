# Comparing size distributions between understory and overstory thin attempts
library(ggplot2)

dat.harvest <- data.frame()
for(HARV in c("Above", "Above_MaplesOnly", "Below", "Below_MaplesOnly")){
  files.over <- dir(paste0("Harvest",HARV,"/analy/"), "-Y-")
  # files.over
  
  for(FILE in files.over){
    yr.now <- as.numeric(strsplit(FILE, "-")[[1]][3])
    
    fnow <- ncdf4::nc_open(file.path(paste0("Harvest", HARV), "analy", FILE)) 
    # summary(fnow$var)
    
    # tst <- ncdf4::ncvar_get(fnow, "AGE")
    df.pch <- data.frame(patch.start=ncdf4::ncvar_get(fnow, "PACO_ID"),
                         n.cohorts=ncdf4::ncvar_get(fnow, "PACO_N"),
                         patch.area = ncdf4::ncvar_get(fnow, "AREA"),
                         patch.age = ncdf4::ncvar_get(fnow, "AGE"))
    df.pch$patchID <- 1:nrow(df.pch)
    
    df.co <- data.frame(year=yr.now,
                        PFT=as.factor(ncdf4::ncvar_get(fnow, "PFT")),
                        DBH=ncdf4::ncvar_get(fnow, "DBH"),
                        Density=ncdf4::ncvar_get(fnow, "NPLANT"))
    ncdf4::nc_close(fnow)
    
    for(i in 1:nrow(df.pch)){
      df.co[(df.pch$patch.start[i]-1):(df.pch$patch.start[i]-1+df.pch$n.cohorts[i]), c("patchID", "patch.area", "patch.age")] <- df.pch[i,c("patchID", "patch.area", "patch.age")]
    }
    
    df.co$dens.wt <- df.co$Density*df.co$patch.area
    df.co$DBH.rnd <- round(df.co$DBH*5, -1)/5 # Roudning to closest 2 cm  just to help reduce dimensions
    df.co <- df.co[df.co$PFT!=5 & df.co$DBH>5,] # Ignore grasses and things <5 cm DBH
    summary(df.co)
    
    # df.co <- aggregate(df.co[,c("dens.wt")],
    #                     by=df.co[,c("PFT", "DBH.rnd")],
    #                     FUN=sum)
    # names(df.co)[3] <- "dens.wt"
    df.co$year <- yr.now
    df.co$Harvest = HARV
    
    dat.harvest <- rbind(dat.harvest, df.co)
  } # end file loop
}# End harvest scheme
dat.harvest$Harvest <- as.factor(dat.harvest$Harvest)
summary(dat.harvest)

dat.patches <- aggregate(dat.harvest[,c("patch.area", "patch.age")],
                         by=dat.harvest[,c("Harvest", "year", "patchID")],
                         FUN=mean)
dat.cohorts <- aggregate(dat.harvest[,c("Density", "dens.wt")],
                         by=dat.harvest[,c("Harvest", "year", "PFT", "DBH.rnd")],
                         FUN=sum)

summary(dat.patches)
ggplot(data=dat.patches[,]) +
  facet_wrap(~Harvest) +
  # geom_line(aes(x=year, y=patch.area, color=as.factor(patchID))) +
  geom_line(aes(x=year, y=patch.age, color=as.factor(patchID))) +
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


ggplot(data=dat.harvest[dat.harvest$year==2007,]) +
  facet_grid(Harvest ~ patchID) +
  geom_histogram(aes(x=DBH.rnd, fill=PFT, weight=Density),binwidth=2) +
  geom_vline(xintercept=30, linetype="dashed") +
  geom_text(aes(x=40, y=0.075, label=round(patch.area,2))) +
  geom_text(aes(x=40, y=0.05, label=round(patch.age,0))) +
  scale_y_continuous(name="density", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")
