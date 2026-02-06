#write.csv(clim,"/disks/home/abigail/thesis-repository/clim.dates.csv")

#library(tidyverse)
#library(data.table)

#clim<-read.csv("/disks/home/abigail/thesis-repository/clim.dates.csv")

#clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
#clim.1.1<-as.vector(clim.1$station_id)
#clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)
#clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

#clim.2<-map(clim.1.3.files,~.x %>% 
#mutate(DATE = as.Date(DATE),YEAR = lubridate::year(DATE),MONTH = lubridate::month(DATE),DAY = lubridate::day(DATE)))

#clim.3<-map(clim.2, ~.x %>% filter(YEAR >= 1973, YEAR <= 2023))

#clim.4 <- lapply(clim.3, function(dt)
#tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#clim.5<-rbindlist(lapply(names(clim.4),function(h){
#dt<-clim.4[[h]]
#data.table(
#station_id = basename(h),n_rows = nrow(dt),min.date=min(dt$DATE, na.rm = TRUE),max.date=max(dt$DATE, na.rm = TRUE),
#data.miss.precip=(sum(is.na(dt$PRCP)))/(nrow(dt)),data.miss.tmax=(sum(is.na(dt$TMAX)))/(nrow(dt)),data.miss.tmin=(sum(is.na(dt$TMIN)))/(nrow(dt)),
#data.miss.tavg=(sum(is.na(dt$TAVG)))/(nrow(dt)))}))

#Precipitation
#precip<-with(clim.5,subset(clim.5,data.miss.precip <=0.1,select = c("station_id","n_rows","data.miss.precip")))
#precip.1.1<-as.vector(precip$station_id)
#precip.1.0<-file.path(paste0(precip.1.1,".csv"))
#precip.1.2<-file.path("NOAA_climate_data_1",precip.1.0)
#precip.1.3<-setNames(lapply(precip.1.2,fread),tools::file_path_sans_ext(basename(precip.1.2)))

#precip.2.1<-map(precip.1.3,~.x%>%
#mutate(
#	DATE = as.Date(DATE),
#	YEAR = lubridate::year(DATE),
#	MONTH = lubridate::month(DATE),
#	DAY = lubridate::day(DATE)))

#precip.2.2<-map(precip.2.1,~.x%>%
#filter(YEAR>=1973,YEAR<=2023))

#precip.2.3<-lapply(precip.2.2,function(a)
#tidyr::complete(dplyr::mutate(a,DATE = as.Date(DATE)),DATE=seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day")))

#precip.2.4<-precip.2.3[sapply(precip.2.3,function(b) "PRCP" %in% names(b))]

#precip.2.5<- lapply(precip.2.4,function(c){
#	c$PRCP<-(as.numeric(c$PRCP)/10)*25.4
#	c
#})

#map <-lapply(precip.2.5, function(d){aggregate(PRCP ~ YEAR*STATION, d, sum, na.rm = TRUE)})

#map.1<-rbindlist(lapply(names(map),function(e){
#x<-map[[e]]
#data.table(station_id = basename(e),map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))

#precip.3<-rbindlist(lapply(names(precip.2.5),function(j){
#	x<-precip.2.5[[j]]
#	data.table(
#	station_id = basename(j),
#	mean.all = mean(x$PRCP, na.rm=TRUE),
#	sd.all = sd(x$PRCP,na.rm=TRUE),
#	mean.events = with(subset(x,PRCP!=0),mean(PRCP,na.rm=TRUE)),
#	sd.events = with(subset(x,PRCP!=0),sd(PRCP, na.rm=TRUE))
#)}))

#precip.3$one.above.all<-precip.3$mean.all+precip.3$sd.all
#precip.3$two.above.all-precip.3$mean.all+2*(precip.3$sd.all)
#precip.3$one.below.all<-precip.3$mean.all-precip.3$sd.all
#precip.3$two.below.all<-precip.3$mean.all-2*(precip.3$sd.all)

#precip.3$events.one.above<-precip.3$mean.events+precip.3$sd.events
#precip.3$events.two.above-precip.3$mean.events+2*(precip.3$sd.events)
#precip.3$events.one.below<-precip.3$mean.events-precip.3$sd.events
#precip.3$events.two.below<-precip.3$mean.events-2*(precip.3$sd.events)

write.csv(precip.3,"/disks/home/abigail/thesis-repository/precipitation.csv")

#Temperature Average
#temp<-with(clim.5,subset(clim.5,data.miss.tavg<=0.1,select = c("station_id","n_rows","data.miss.tmax","data.miss.tmin","data.miss.tavg")))

#temp.1.1<-as.vector(temp$station_id)

#temp.1.2<-file.path(paste0(temp.1.1,".csv"))

#temp.1.3<-file.path("NOAA_climate_data_1",temp.1.2)

#temp.1.4<-setNames(lapply(temp.1.3,fread),tools::file_path_sans_ext(basename(temp.1.3)))

#temp.2.1<-map(temp.1.4,~.x%>%
#mutate(DATE = as.Date(DATE),YEAR = lubridate::year(DATE),MONTH = lubridate::month(DATE),DAY = lubridate::day(DATE)))

#temp.2.2<-map(temp.2.1, ~.x %>% filter(YEAR >= 1973, YEAR <= 2023))
#temp.2.3 <- lapply(temp.2.2, function(dt)
#tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#temp.2.3<-temp.2.3[sapply(temp.2.3,function(a) "TAVG" %in% names(a))]

#temp.2.4<-map(temp.2.3,~.x%>%mutate(TAVG = as.numeric(TAVG)))

#temp.3<-rbindlist(lapply(names(temp.2.4),function(j){
#       x<-temp.2.4[[j]]
#       data.table(
#       station_id = basename(j),
#       mean.all = mean(x$TAVG, na.rm=TRUE),
#       sd.all = sd(x$TAVG,na.rm=TRUE),
#       mean.events = with(subset(x,TAVG!=0),mean(TAVG,na.rm=TRUE)),
#       sd.events = with(subset(x,TAVG!=0),sd(TAVG, na.rm=TRUE))
#)}))

#temp.3$one.above.all<-temp.3$mean.all+temp.3$sd.all
#temp.3$two.above.all-temp.3$mean.all+2*(temp.3$sd.all)
#temp.3$one.below.all<-temp.3$mean.all-temp.3$sd.all
#temp.3$two.below.all<-temp.3$mean.all-2*(temp.3$sd.all)

#temp.3$events.one.above<-temp.3$mean.events+temp.3$sd.events
#temp.3$events.two.above-temp.3$mean.events+2*(temp.3$sd.events)
#temp.3$events.one.below<-temp.3$mean.events-temp.3$sd.events
#temp.3$events.two.below<-temp.3$mean.events-2*(temp.3$sd.events)

#Temperature Max
#temp.max<-with(clim.5,subset(clim.5,data.miss.tmax<=0.1,select = c("station_id","n_rows","data.miss.tmax","data.miss.tmin","data.miss.tavg")))

#temp.max.1.1<-as.vector(temp.max$station_id)

#temp.max.1.2<-file.path(paste0(temp.max.1.1,".csv"))

#temp.max.1.3<-file.path("NOAA_climate_data_1",temp.max.1.2)

#temp.max.1.4<-setNames(lapply(temp.max.1.3,fread),tools::file_path_sans_ext(basename(temp.max.1.3)))

#temp.max.2.1<-map(temp.max.1.4,~.x%>%
#mutate(DATE = as.Date(DATE),YEAR = lubridate::year(DATE),MONTH = lubridate::month(DATE),DAY = lubridate::day(DATE)))

#temp.max.2.2<-map(temp.max.2.1, ~.x %>% filter(YEAR >= 1973, YEAR <= 2023))
#temp.max.2.3 <- lapply(temp.max.2.2, function(dt)
#tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#temp.max.2.3<-temp.max.2.3[sapply(temp.max.2.3,function(a) "TMAX" %in% names(a))]

#temp.max.2.4<- lapply(temp.max.2.3,function(b){b$TMAX<-as.numeric(b$TMAX)/10})

#temp.max.3<-rbindlist(lapply(names(temp.max.2.4),function(j){
#       x<-temp.max.2.4[[j]]
#       data.table(
#       station_id = basename(j),
#       mean.all = mean(x$TMAX, na.rm=TRUE),
#       sd.all = sd(x$TMAX,na.rm=TRUE)
#)}))

#temp.max.3$one.above.all<-temp.max.3$mean.all+temp.max.3$sd.all
#temp.max.3$two.above.all-temp.max.3$mean.all+2*(temp.max.3$sd.all)
#temp.max.3$one.below.all<-temp.max.3$mean.all-temp.max.3$sd.all
#temp.max.3$two.below.all<-temp.max.3$mean.all-2*(temp.max.3$sd.all)





