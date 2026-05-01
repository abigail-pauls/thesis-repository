library(data.table)
library(tidyverse)
library(sf)

sf_use_s2(FALSE)

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = 0)

ecoregions<-st_read("/disks/home/abigail/thesis-repository/code/data/ecoregions.geojson")

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data.1<-data[,c("station_id","noaa.lon","noaa.lat")]

data.1<-unique(data.1)

points <- st_as_sf(data.1,coords = c("noaa.lon", "noaa.lat"),crs = 4326)

result <- st_join(points,ecoregions, join = st_intersects)

results.1<-as.data.frame(result)

write.csv(results.1,"/disks/home/abigail/thesis-repository/code/data/results.1.csv")

result.1<-result[,c("station_id","code")]

write.csv(result.1, "/disks/home/abigail/thesis-repository/code/data/ecosystems.csv")

result.1<-as.data.frame(result)

result.1<-result.1[,c("profile_id","BIOME_NAME")]

profiles<-merge(profiles,result.1,all.x=TRUE)
