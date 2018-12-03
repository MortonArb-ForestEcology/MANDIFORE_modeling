# Identify sites
# Goal: Build a handy-dandy csv file with the variables we're interested in and the appropriate predictors that can then get passed to fun analyses

library(ggplot2)

# Setting File paths
run.table <- read.csv("../0_setup/ExperimentalDesign_Test.csv")
path.extract <- "../2_runs/extracted_output.v3/"
summary(run.table)

# some site metadata
# sitelat =  45.805822
# sitelon = -90.079722
start_date <- "2006-01-01" # The runs actually start in June 1800, but lets skip that first year because it's only partial
end_date = "2100-12-31"

vars.met <- c("CO2air", "Tair", "Rainf")
# vars.site <- c("SoilMoist", "SoilTemp", "Fire_flux", "NEE", "NPP")
vars.site <- c("Fire_flux", "NEE", "NPP")
vars.pft <- c("AbvGrndBiom", "LAI", "Density_Tree", "BasalArea_Tree", "DBH_Tree")
pfts.extract <- c(5:11)

slz <- c(-1.17, -0.80, -0.60, -0.45, -0.30, -0.20, -0.12, -0.06)
depth.use <- which(slz>= -0.30) # Just use the top 30 cm
dslz.use <- rev(diff(-c(0,rev(slz[depth.use]))))
wt.depth <- dslz.use/sum(dslz.use)



## Get a list of the output we have available
all.runs <- dir(path.extract) # Get a list of what's been run (at least partially)

dat.mo <- data.frame()
dat.pft <- data.frame()
for(RUNID in all.runs){
  print(paste0("-- Processing ", RUNID))
  exp.ind <- which(run.table$RunID==RUNID)
  
  # Get a list of the files to pull
  frun <- dir(file.path(path.extract, RUNID), ".nc")
  frun <- frun[which(substr(frun, nchar(frun)-2, nchar(frun))==".nc")] # Get rid of the .var files
  
  # Loop through to extract what we want
  pb <- txtProgressBar(min=0, max=length(frun), style=3)
  for(i in 1:length(frun)){
    setTxtProgressBar(pb, i)
    yr.now  <- as.numeric(substr(frun[i], 1, 4))
    
    tmp.mo  <- data.frame(RunID=rep(RUNID, 12), 
                          site=run.table$SiteName[exp.ind],
                          management=run.table$management[exp.ind], 
                          GCM=run.table$GCM[exp.ind], 
                          scenario=run.table$scenario[exp.ind], 
                          year=yr.now, month=1:12)
    tmp.pft <- data.frame(RunID=RUNID, 
                          site=run.table$SiteName[exp.ind],
                          management=run.table$management[exp.ind], 
                          GCM=run.table$GCM[exp.ind], 
                          scenario=run.table$scenario[exp.ind], 
                          year=yr.now, month=rep(1:12, length(pfts.extract)),
                          pft=rep(pfts.extract, each=12))
    
    ncT <- ncdf4::nc_open(file.path(path.extract, RUNID, frun[i]))
    # Extract vars where we want 1 var per site
    for(VAR in c(vars.met, vars.site)){
      if(VAR %in% c("SoilMoist", "SoilTemp")){
        tmp.mo[,VAR] <- apply(ncdf4::ncvar_get(ncT, VAR)[depth.use,] * wt.depth, 2, sum)
      } else {
        tmp.mo[,VAR] <- ncdf4::ncvar_get(ncT, VAR)
      }
    } # End site vars
    
    # Extract info where we have PFT information
    for(VAR in vars.pft){
      if(!VAR %in% names(ncT$var)) next
      var.tmp <- ncdf4::ncvar_get(ncT, VAR) # extract the dat once for speed
      
      if(VAR %in% c("DBH_Tree")){
        tmp.mo[,VAR] <- apply(var.tmp[,pfts.extract[!pfts.extract==5]], 1, mean, na.rm=T) # Store the mean DBH
      } else if(VAR %in% c("BasalArea_Tree", "Density_Tree")) {
        tmp.mo[,VAR] <- apply(var.tmp[,pfts.extract[!pfts.extract==5]], 1, sum, na.rm=T) # Store the sum
      } else {
        tmp.mo[,VAR] <- apply(var.tmp[,pfts.extract], 1, sum, na.rm=T) # Store the sum
      }
      
      for(PFT in pfts.extract){
        if(PFT==5 & VAR %in% c("DBH_Tree", "BasalArea_Tree", "Density_Tree")) next
        tmp.pft[tmp.pft$pft==PFT,VAR] <- var.tmp[,PFT]
      }
    } # End PFT vars
    ncdf4::nc_close(ncT)
    
    dat.mo <- rbind(dat.mo, tmp.mo)
    dat.pft <- rbind(dat.pft, tmp.pft)
  }
}
summary(dat.mo)
summary(dat.pft)

write.csv(dat.mo, "processed_out/MANDIFORE_Ameriflux_month_site.csv", row.names=F)
write.csv(dat.pft, "processed_out/MANDIFORE_Ameriflux_month_pft.csv", row.names=F)

dat.yr <- aggregate(dat.mo[,c(vars.site, vars.pft)], 
                    by=dat.mo[,c("site", "management", "GCM", "scenario", "year")],
                    FUN=mean, na.rm=T)
dat.yr$management <- factor(dat.yr$management, levels=c("passive", "preservation", "ecological", "production"))
summary(dat.yr)

dat.yr.pft <- aggregate(dat.pft[,c(vars.pft)],
                        by=dat.pft[,c("site", "management", "GCM", "scenario", "year", "pft")],
                        FUN=mean, na.rm=T)
dat.yr.pft$management <- factor(dat.yr.pft$management, levels=c("passive", "preservation", "ecological", "production"))
dat.yr.pft$pft <- car::recode(dat.yr.pft$pft, "'5'='Grass'; '6'='N. Pine'; '7'='S. Pine'; '8'='Late Conifer'; '9'='Early Hardwood'; '10'='Mid Hardwood'; '11'='Late Hardwood'")
dat.yr.pft$pft <- factor(dat.yr.pft$pft, levels=c("Early Hardwood", "Mid Hardwood", "Late Hardwood", "S. Pine", "N. Pine", "Late Conifer", "Grass"))
summary(dat.yr.pft)


pdf("figures.v3/MANDIFORE_Output_QuickGraphs.pdf", height=8, width=10)
for(VAR in c(vars.site, vars.pft)){
  if(VAR %in% c("DBH")) next
  dat.yr$VAR <- dat.yr[,VAR]
  print(
    ggplot(data=dat.yr[,]) +
      ggtitle(VAR) +
      facet_wrap(~site) +
      geom_line(aes(x=year, y=VAR, linetype=scenario, color=management), size=0.75) +
      scale_color_manual(values=c("black", "blue2", "green4", "darkorange2")) +
      theme_bw()
  )
}
dev.off()

pdf("figures.v3/MANDIFORE_Output_QuickGraphs_PFT.pdf", height=8, width=10)
for(VAR in c(vars.pft)){
  # if(VAR %in% c("BasalArea", "DBH", "Density")) next
  dat.yr.pft$VAR <- dat.yr.pft[,VAR]
  print(
    ggplot(data=dat.yr.pft[,]) +
      ggtitle(VAR) +
      facet_grid(pft~site) +
      geom_line(aes(x=year, y=VAR, linetype=scenario, color=management), size=0.75) +
      scale_color_manual(values=c("black", "blue2", "green4", "darkorange2")) +
      theme_bw() +
      theme(legend.position="bottom")
  )
}
dev.off()

