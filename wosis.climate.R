library(tidyverse)
library(data.table)
library(readr)

lp.all<-readr::read_delim("wosis.lp.date.prof.csv",show_col_types=FALSE)
ghcl_stations <- read_fwf(
  "ghcl_stations.txt",
  fwf_widths(
    c(11, 9, 10, 8, 35, 10),
    c("station_id", "latitude", "longitude", "elevation", "station_name", "extra")
  ),show_col_types=FALSE
)


#clean-up ghcl_stations
ghcl_stations[,5:6]<-NULL


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

lp.all$latitude<-as.character(lp.all$latitude)
lp.all$longitude<-as.character(lp.all$longitude)

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
  lon<-as.numeric(lp.all$longitude[i])
  if(lon>=0 & long<100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,4)}
  if(lon>=100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(lon<0 & long>-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,5)}
  if(lon<=-100){
    lp.all$lon[i]<-substr(lp.all$longitude[i],1,6)}
}

lp.all$latitude<-as.numeric(lp.all$latitude)
lp.all$longitude<-as.numeric(lp.all$longitude)
lp.all$lat<-as.numeric(lp.all$lat)
lp.all$lon<-as.numeric(lp.all$lon)

lp.all$min_lat<-lp.all$latitude-0.1
lp.all$max_lat<-lp.all$latitude+0.1
lp.all$min_lon<-lp.all$longitude-0.1
lp.all$max_lon<-lp.all$longitude+0.1

ghcl_stations$latitude<-as.numeric(ghcl_stations$latitude)
ghcl_stations$longitude<-as.numeric(ghcl_stations$longitude)
ghcl_stations$lat<-as.numeric(ghcl_stations$lat)
ghcl_stations$lon<-as.numeric(ghcl_stations$lon)

setDT(lp.all)
setDT(ghcl_stations)
lp.clim.1 <- lp.all[ghcl_stations,on = .(min_lat <= latitude, max_lat >= latitude,min_lon <= longitude, max_lon >= longitude),nomatch = 0]

lp.clim.1<-lp.clim.1%>%relocate(date,.before = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(year,.after = date)
lp.clim.1<-lp.clim.1%>%relocate(month,.after = year)
lp.clim.1<-lp.clim.1%>%relocate(day,.after = month)
lp.clim.1<-lp.clim.1%>%relocate(longitude,.before = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(latitude,.before = layer_id)
lp.clim.1<-rename(lp.clim.1,longitude_wosis="longitude")
lp.clim.1<-rename(lp.clim.1,latitude_wosis="latitude")
lp.clim.1<-lp.clim.1%>%relocate(min_lon,.before = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(min_lat,.before = layer_id)
lp.clim.1<-rename(lp.clim.1,latitude_ghcn="min_lat")
lp.clim.1<-rename(lp.clim.1,longitude_ghcn="min_lon")
lp.clim.1$max_lat<-NULL
lp.clim.1$max_lon<-NULL
lp.clim.1<-lp.clim.1%>%relocate(elevation,.before = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(station_id,.after = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(dataset_id,.after = layer_id)
lp.clim.1<-lp.clim.1%>%relocate(dataset_code,.after = profile_code.x)
lp.clim.1<-rename(lp.clim.1,wosis_profile_code="profile_code.x")
lp.clim.1<-lp.clim.1%>%relocate(continent.x,.after = dataset_code)
lp.clim.1<-rename(lp.clim.1,continent="continent.x")
lp.clim.1<-lp.clim.1%>%relocate(region.x,.after = continent)
lp.clim.1<-rename(lp.clim.1,region="region.x")
lp.clim.1<-lp.clim.1%>%relocate(country_name.x,.after = region)
lp.clim.1<-rename(lp.clim.1,country_name="country_name.x")

#merge climate data and soil data based on latitude and longitude limited values
lp.clim.1<-merge(lp.all,ghcl_stations,by=c("lat","lon"),all.x = TRUE,all.y=FALSE)

#save dataframe into thesis repo
write.csv(lp.clim.1,file="~/thesis-repository/lp.clim.csv",row.names=FALSE)
