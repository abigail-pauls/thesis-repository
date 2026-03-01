library(tidyverse)
library(data.table)

lp.clim<-read.csv("lp.clim.csv")

stations<-as.vector(unique(lp.clim$station_id))

stations.1<-stations[! stations %in% c("NA")]

clim.stations<-file.path(paste0(stations,".csv"))

clim.stations.1<-clim.stations[! clim.stations %in% c("NA.csv","US1COLR0556.csv")]

files<-file.path("NOAA_climate_data_1",clim.stations.1)

files.1<-files[file.exists(files)]

data_list<-setNames(lapply(files.1,fread),tools::file_path_sans_ext(basename(files.1)))

clim<-rbindlist(lapply(files.1,function(f){
dt<-fread(f)	
data.table(
	station_id = basename(f),
	n_rows = nrow(dt),
        min.date=min(dt$DATE, na.rm = TRUE),
        max.date=max(dt$DATE, na.rm = TRUE))}))



#clim <- vector("list", length(files.1))

#for(i in seq_along(data_list)){
#	clim[[i]]<-data.table(
#	n_rows = nrow(data_list),
#	station_id=basename(files.1),
#	min.date=min(data_list[[i]]$DATE),
#	max.date=max(data_list[[i]]$DATE))
#}

#clim.1<-rbindlist(clim)

#precip<-list()

#for(h in seq_along(data_list)){
#	if(((is.na(data_list[[h]][[PRCP]])/(nrows(data_list[[h]][[PRCP]]))<=0.1){
#	precip[h]<-data_list[h]}}

#temp <- temp %>% 
#  dplyr::mutate(min.year = NA)
#temp <- temp %>% 
#  dplyr::mutate(max.year = NA)

#for(j in seq_along(stations)){
#test$min.year[j]<-paste(min(j$year))
#temp$max.year[j]<-paste(max(j$year))}
