# Parse fluxnet options from Mike Dietze to see what our options are

path.dietze <- "../init_files/fluxnet_dietze/"
f.dietze <- dir(path.dietze, ".pss")


fsplit <- strsplit(f.dietze, "lat")
fsplit[1:10]


sites <- data.frame(Code=unlist(lapply(fsplit, FUN=function(x){x[1]})), lat=NA, lon=NA)
for(i in 1:length(fsplit)){
  latlon <- strsplit(fsplit[[i]][2], "lon")[[1]]
  latlon[1] <- as.numeric(latlon[1])
  latlon[2] <- as.numeric(paste(strsplit(latlon[2], "[.]")[[1]][1:2], collapse="."))
  
  if(latlon[1]<0 | latlon[2] >0) next
  sites[i, c("lat", "lon")] <- as.numeric(latlon)

}
summary(sites)
sites[is.na(sites$lon),]

library(ggplot2)
us <- map_data("state")
ggplot(data=sites) +
  coord_equal() +
  geom_path(data=us, aes(x=long, y=lat, group=group)) +
  geom_text(aes(x=lon, y=lat, label=Code), color="red") +
  theme_bw()

write.csv(sites, "Fluxnet_DietzeInits.csv", row.names=F)
