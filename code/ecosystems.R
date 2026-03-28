#library(data.table)
#library(tidyverse)
#library(jsonlite)
#library(sf)

#untar("/disks/home/abigail/thesis-repository/code/data/eco-data/all-maps-vector-geojson.tar.bz2")

sf_use_s2(FALSE)

#files <- list.files(pattern = "^T.*\\.json$", full.names = TRUE)

Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = 0)

#geo_list <- lapply(files, function(f){
#	temp<-try(st_read(f, quiet = TRUE), silent = TRUE)
#	temp$file.name<-f
#	return(temp)})

ecoregions<-st_read("/disks/home/abigail/thesis-repository/code/data/resolve.ecoregions.geojson")

#geo_list <- geo_list[!sapply(geo_list, inherits, "try-error")]

#geo_list <- geo_list[sapply(geo_list, function(x) inherits(x, "sf"))]

#geo_list <- lapply(geo_list, function(f) {
#	geo<-tryCatch(st_transform(f, crs = 4326), error = function(e){message(f,"failed");return(NULL)})
#	if (!is.null(geo)) {
#		geo <- st_make_valid(geo)
#		geo$file_name <- f
#		return(geo)}
#else return(NULL)})

#for(i in geo_list){
#	geo_list_2[i]<-st_transform(i,crs=4326)}

#geo_list <- lapply(geo_list, function(x) {
#  if ("sf" %in% class(x)) st_transform(x, crs = 4326) else NULL
#})

#geo_list <- geo_list[!sapply(geo_list, is.null)]

#biomes <- bind_rows(geo_list)

#biomes <- st_make_valid(biomes)

#biomes <- biomes[st_is_valid(biomes), ]

#biomes_proj <- st_transform(biomes, 3857)

#biomes_proj <- st_simplify(biomes_proj, dTolerance = 10000, preserveTopology=TRUE)

#biomes <- st_transform(biomes_proj, 4326)

#biomes <- st_simplify(biomes, dTolerance = 0.05)

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data.1<-data[,c("station_id","noaa.lon","noaa.lat")]

data.1<-unique(data.1)

points <- st_as_sf(data.1,coords = c("noaa.lon", "noaa.lat"),crs = 4326)

result <- st_join(points,ecoregions, join = st_intersects)

#results.1<-as.data.frame(result)

#write.csv(results.1,"/disks/home/abigail/thesis-repository/code/data/results.1.csv")

#result$code<-substr(result$file.name,3,6)
#result$code.backup<-substr(result$file.name,3,7)

#result.1<-result[,c("station_id","code")]

#write.csv(result.1, "/disks/home/abigail/thesis-repository/code/data/ecosystems.csv")

