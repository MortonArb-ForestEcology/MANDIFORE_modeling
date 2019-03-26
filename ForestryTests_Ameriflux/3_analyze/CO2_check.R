# Quick test to see of CO2 was successfully turned off
library(ggplot2)

dir.on <- "../2_runs/extracted_output.v3/WCr-pass-lo/"
dir.off <- "../2_runs/extracted_output.v3/WCr-pass-lo-statCO2/"  

fon <- dir(file.path(dir.on), ".nc")
foff <- dir(file.path(dir.off), ".nc")
fon <- fon[which(substr(fon, nchar(fon)-2, nchar(fon))==".nc")] # Get rid of the .var files
foff <- foff[which(substr(foff, nchar(foff)-2, nchar(foff))==".nc")] # Get rid of the .var files

dat.all <- data.frame(Year=2006:(2006+length(fon)-1), co2.on=NA, co2.off=NA)
for(i in 1:length(fon)){
  nc.on <- ncdf4::nc_open(file.path(dir.on, fon[i]))
  nc.off <- ncdf4::nc_open(file.path(dir.off, foff[i]))
  names(nc.on$var)
  agb.on <- apply(ncdf4::ncvar_get(nc.on, "AbvGrndBiom"), 1, sum, na.rm=T)
  agb.off <- apply(ncdf4::ncvar_get(nc.off, "AbvGrndBiom"), 1, sum, na.rm=T)

  dat.all[i,"co2.on"] <- mean(agb.on)
  dat.all[i,"co2.off"] <- mean(agb.off)
  
  ncdf4::nc_close(nc.on)
  ncdf4::nc_close(nc.off)
}

ggplot(data=dat.all) +
  geom_line(aes(x=Year, y=co2.on), color="green3") +
  geom_line(aes(x=Year, y=co2.off), color="black")
