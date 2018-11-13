# Identify sites
# Goal: Build a handy-dandy csv file with the variables we're interested in and the appropriate predictors that can then get passed to fun analyses

# Setting File paths
run.table <- read.csv("../0_setup/ExperimentalDesign_AGU2018.csv")
path.extract <- "../1_runs/extracted_output.v1/"
summary(run.table)

# some site metadata
sitelat =  45.805822
sitelon = -90.079722
start_date <- "2007-01-01" # The runs actually start in June 1800, but lets skip that first year because it's only partial
end_date = "2100-12-31"

vars.site <- c("CO2air", "Tair", "Rainf", "SoilMoist", "SoilTemp", "Fire_flux", "NEE", "NPP")
vars.pft <- c("AbvGrndBiom", "Density", "BasalArea", "LAI")
pfts.extract <- c(6, 8, 9:11)

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
                          management=run.table$management[exp.ind], 
                          GCM=run.table$GCM[exp.ind], 
                          scenario=run.table$scenario[exp.ind], 
                          year=yr.now, month=1:12)
    tmp.pft <- data.frame(RunID=RUNID, 
                          management=run.table$management[exp.ind], 
                          GCM=run.table$GCM[exp.ind], 
                          scenario=run.table$scenario[exp.ind], 
                          year=yr.now, month=rep(1:12, length(pfts.extract)),
                          pft=rep(pfts.extract, each=12))
    
    ncT <- ncdf4::nc_open(file.path(path.extract, RUNID, frun[i]))
    # Extract vars where we want 1 var per site
    for(VAR in vars.site){
      if(VAR %in% c("SoilMoist", "SoilTemp")){
        tmp.mo[,VAR] <- apply(ncdf4::ncvar_get(ncT, VAR)[depth.use,] * wt.depth, 2, sum)
      } else {
        tmp.mo[,VAR] <- ncdf4::ncvar_get(ncT, VAR)
      }
    } # End site vars
    
    # Extract info where we have PFT information
    for(VAR in vars.pft){
      var.tmp <- ncdf4::ncvar_get(ncT, VAR) # extract the dat once for speed
      
      tmp.mo[,VAR] <- apply(var.tmp, 1, sum, na.rm=T) # Store the sum
      for(PFT in pfts.extract){
        tmp.pft[tmp.pft$pft==PFT,VAR] <- var.tmp[,PFT]
      }
    } # End PFT vars
    ncdf4::nc_close(ncT)
    
    dat.mo <- rbind(dat.mo, tmp.mo)
    dat.pft <- rbind(dat.pft, tmp.pft)
  }
}