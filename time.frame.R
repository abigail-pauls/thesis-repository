#write.csv(clim,"/disks/home/abigail/thesis-repository/clim.dates.csv")

clim.1<-with(clim,subset(clim,min.date<="1948-01-01"&max.date>="2023-01-01"))

clim.1.1<-as.vector(clim.1$station_id)

clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)

clim.2<-rbindlist(lapply(clim.1.2.files,function(h){
dt<-fread(h,na.strings="")
data.table(
        station_id = basename(h),
        n_rows = nrow(dt),
        min.date=min(dt$DATE, na.rm = TRUE),
        max.date=max(dt$DATE, na.rm = TRUE),
	data.miss.precip=(sum(is.na(dt$PRCP)))/(nrow(dt)),
	data.miss.tmax=(sum(is.na(dt$TMAX)))/(nrow(dt)),
        data.miss.tmin=(sum(is.na(dt$TMIN)))/(nrow(dt)),
        data.miss.tavg=(sum(is.na(dt$TAVG)))/(nrow(dt)))
}))

#data_list<-setNames(lapply(files.1,fread),tools::file_path_sans_ext(basename(files.1)))

#clim<-rbindlist(lapply(files.1,function(f){
#dt<-fread(f)
#data.table(
#        station_id = basename(f),
#        n_rows = nrow(dt),
#        min.date=min(dt$DATE, na.rm = TRUE),
#        max.date=max(dt$DATE, na.rm = TRUE))}))


#clim.1.3<-list()

#for(i in seq_along(clim.1.3.files)){
#    if((is.na(clim.1.3.files[[i]][[PRCP]]))/(nrow(clim.1.3.files[[i]][[PRCP]]))){
#	clim.1.3[[i]]<-basename(clim.1.3.files[[i]])}}
