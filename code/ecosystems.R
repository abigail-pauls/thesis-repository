library(data.table)
library(tidyverse)
library(jsonlite)
library(sf)

#final<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")
#ecosystems<-st_read("/disks/home/abigail/thesis-repository/code/data/eco-data/F2.4.web.mix_v1.0.json")

coordinates<-subset(final,select=c("wosis.lon","wosis.lat","noaa.lon","noaa.lat"))

coordinates<-coordinates[1:10,]

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = 0)

sf_use_s2(FALSE)

files<-list.files("data/eco-data/",pattern="\\.json$",full.names=TRUE)

ecosystem_list <- lapply(files, function(f) {
eco_name <- tools::file_path_sans_ext(basename(f))

eco<-st_read(f, quiet = TRUE)

eco<-st_zm(eco,drop = TRUE,what="ZM")

eco<-st_make_valid(eco)

if(is.na(st_crs(eco))){
	bbox<-st_bbox(eco)
	if(abs(bbox$xmin)<=180 && abs(bbox$ymin) <=90){
		eco <- st_set(crs(eco,4326))}
	else{
		stop(paste("CRS missing and not in degrees:",f))}}
eco<-st_transform(eco,4326)

eco$file<-eco_name

return(eco)})

ecosystems<-do.call(rbind,ecosystem_list)

points<-st_as_sf(coordinates,coords = c("wosis.lon","wosis.lat"),crs=4362)

eco.points<-st_join(ecosystems,points,join=st_within)

