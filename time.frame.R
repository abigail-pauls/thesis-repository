#write.csv(clim,"/disks/home/abigail/thesis-repository/clim.dates.csv")

clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))

clim.1.1<-as.vector(clim.1$station_id)

#clim.1.files<-file.path(paste0(clim.1.1,".csv"))

clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)

clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

clim.1.3<-list()

for(i in seq_along(clim.1.3.files)){
    if((is.na(clim.1.3.files[[i]][[PRCP]]))/(nrow(clim.1.3.files[[i]][[PRCP]]))){
	clim.1.3[[i]]<-basename(clim.1.3.files[[i]])}}
