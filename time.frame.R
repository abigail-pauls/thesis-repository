#write.csv(clim,"/disks/home/abigail/thesis-repository/clim.dates.csv")

clim.1<-with(clim,subset(clim,min.date<="1983-01-01"&max.date>="2023-01-01"))

clim.1.1<-as.vector(clim.1$station_id)

clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)

clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

clim.2<-map(clim.1.3.files,~.x %>% 
	mutate(
	DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

clim.3<-map(clim.2, ~.x %>% filter(YEAR >= 1973, YEAR <= 2023))

clim.4 <- lapply(clim.3, function(dt)
  tidyr::complete(
    dplyr::mutate(dt, DATE = as.Date(DATE)),
    DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")
  )
)


clim.5<-rbindlist(lapply(names(clim.4),function(h){
dt<-clim.4[[h]]
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

#Precipitation
precip<-with(clim.5,subset(clim.5,data.miss.precip <=0.1,select = c("station_id","n_rows","data.miss.precip")))

#for(i in seq_along(precip.2)){
#precip.2[[i]]$PRCP<-as.numeric(precip.2[[i]]$PRCP)}

#MAP<-data.frame()

#MAP<-rbindlist(lapply(precip.2,function(i){
#dt<-with(precip.2[[i]],subset(precip[[i]],DATE<="2023-01-01"&DATE>="1973-01-01"))
#data.table(
#station_id = 



#for(i in seq_along(precip.2)){
#	dt<-with(precip.2[[i]],subset(precip.2,DATE>="1973-01-01"&DATE<="2023-01-01"))
#	MAP[i]<-aggregate(PRCP~YEAR, data=dt,sum,na.rm=TRUE)

#precip.3<-rebindlist(lapply(precip.stations.2,function(i){
#dt<-fread(i,na.strings="")
#data.table(
#stations_id = basename(h)


#MAP <- map(precip.2, ~ .x %>%
#  group_by(YEAR) %>%
#  summarise(total = sum(PRCP, na.rm = TRUE))
#)

#Temperature
#temp<-with(clim.2,subset(clim.2,data.miss.tmax <=0.1&data.miss.tmin<=0.1&data.miss.tavg<=0.1,select = c("station_id","n_rows","data.miss.tmax","data.miss.tmin",
#"data.miss.tavg")))

