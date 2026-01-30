library(tidyverse)
library(data.table)
library(readr)

lp.all<-read.csv("~/thesis-repository/wosis.lp.date.prof.csv")
ghcl.stations <- read_table("~/thesis-repository/ghcnd-stations.txt", col_names = FALSE)

#double check that all data in lp.all is actually within the leptosol units
setDT(lp.all)
test_1 <- lp_smu_latlon.1[lp.all,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]

#clean-up ghcl_stations
ghcl_stations<-rename(ghcl_stations,station_id="X1")
ghcl_stations<-rename(ghcl_stations,latitude="X2")
ghcl_stations<-rename(ghcl_stations,longitude="X3")
ghcl_stations<-rename(ghcl_stations,elevation="X4")
ghcl_stations[,5:9]<-NULL


ghcl_stations$latitude<-as.character(ghcl_stations$latitude)
ghcl_stations$longitude<-as.character(ghcl_stations$longitude)

ghcl_stations <- ghcl_stations %>% dplyr::mutate(lat = NA)
ghcl_stations <- ghcl_stations %>% dplyr::mutate(lon = NA)

#limit latitude value to one decimal point, so can match with soil data within one tenth latitude/longitude
for (i in 1:nrow(ghcl_stations)) {
  latitude<-ghcl_stations$latitude[i]
  longitude<-ghcl_stations$longitude[i]
  if(latitude>=0){
    ghcl_stations$lat[i]<-substr(ghcl_stations$latitude[i],1,4)}
    else {ghcl_stations$lat[i]<-substr(ghcl_stations$latitude[i],1,5)
  }
}

#limit longitude value to one decimal point, so can match with soil data within one tenth latitude/longitude
for (i in 1:nrow(ghcl_stations)) {
  longitude<-ghcl_stations$longitude[i]
  long<-as.numeric(ghcl_stations$longitude[i])
  if(long>=0 & long<100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,4)}
  if(long>=100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,5)}
  if(long<0 & long>-100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,5)}
  if(long<=-100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,6)}
}

lp.all <- lp.all %>% dplyr::mutate(lat = NA)
lp.all <- lp.all %>% dplyr::mutate(lon = NA)

#limit latitude value to one decimal point, so can match with climate data within one tenth latitude/longitude
for (i in 1:nrow(lp.all)) {
  latitude<-lp.all$latitude[i]
  longitude<-lp.all$longitude[i]
  if(latitude>=0){
    lp.all$lat[i]<-substr(lp.all$latitude[i],1,4)}
  else {lp.all$lat[i]<-substr(lp.all$latitude[i],1,5)
  }
}

#limit longitude value to one decimal point, so can match with climate data within one tenth latitude/longitude
for (i in 1:nrow(lp.all)) {
  longitude<-lp.all$longitude[i]
  long<-as.numeric(lp.all$longitude[i])
  if(long>=0 & long<100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,4)}
  if(long>=100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(long<0 & long>-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(long<=-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,6)}
}

  long<-as.numeric(ghcl_stations$longitude[i])
  if(long>=0 & long<100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,4)}
  if(long>=100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,5)}
  if(long<0 & long>-100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,5)}
  if(long<=-100){
    ghcl_stations$lon[i]<-substr(ghcl_stations$longitude[i],1,6)}
}

lp.all <- lp.all %>% dplyr::mutate(lat = NA)
lp.all <- lp.all %>% dplyr::mutate(lon = NA)

#limit latitude value to one decimal point, so can match with climate data within one tenth latitude/longitude
for (i in 1:nrow(lp.all)) {
  latitude<-lp.all$latitude[i]
  longitude<-lp.all$longitude[i]
  if(latitude>=0){
    lp.all$lat[i]<-substr(lp.all$latitude[i],1,4)}
  else {lp.all$lat[i]<-substr(lp.all$latitude[i],1,5)
  }
}

#limit longitude value to one decimal point, so can match with climate data within one tenth latitude/longitude
for (i in 1:nrow(lp.all)) {
  longitude<-lp.all$longitude[i]
  long<-as.numeric(lp.all$longitude[i])
  if(long>=0 & long<100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,4)}
  if(long>=100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(long<0 & long>-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(long<=-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,6)}
}

#merge climate data and soil data based on latitude and longitude limited values
lp.clim<-merge(lp.all,ghcl_stations,by=c("lat","lon"),all.x = TRUE,all.y=FALSE)

#save dataframe into thesis repo
write.csv(lp.clim,"~/thesis-repository/lp.clim.csv",row.names=FALSE)
