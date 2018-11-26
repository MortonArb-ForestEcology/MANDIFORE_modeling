# Dupe met into a pseudo-spin to equilibrate prior to adding in land use
# Copy the 2006-2008 met from the low emissions scenario prior to instigating land use

n.spin = 25
met.min = 2006
met.max = 2008

path.met <- "../met_ed"

sites.all <- dir(path.met)

# Set up some vectors of how things will be indexed
met.vec <- rep(met.min:met.max, length.out=n.spin) # Which met goes with each year
yrs.add <- (met.min-n.spin):(met.min-1)
for(SITE in sites.all){
  gcm.low <- dir(file.path(path.met, SITE), "rcp45")
  
  gcm.all <- dir(file.path(path.met, SITE))
  for(GCM in gcm.all){
    for(i in 1:length(yrs.add)){
      met.cp <- dir(file.path(path.met, SITE, gcm.low), paste(met.vec[i]))
      met.new <- paste0(yrs.add[i], substr(met.cp, 5, 10))
      for(j in 1:length(met.cp)){
        sys.cmd <- paste("cp", file.path(path.met, SITE, gcm.low, met.cp[j]), 
                         file.path(path.met, SITE, GCM, met.new[j]))
        system(sys.cmd)
      }
    }
  }
}