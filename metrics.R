#library(lubridate)
#library(tidyverse)
#library(data.table)

#clim<-read.csv("/disks/home/abigail/thesis-repository/clim.dates.csv")

#clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
#clim.1.1<-as.vector(clim.1$station_id)
#clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)
#clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

#clim.2<-map(clim.1.3.files,~.x %>%
#	mutate(DATE = as.Date(DATE),
#	YEAR = lubridate::year(DATE),
#	MONTH = lubridate::month(DATE),
#	DAY = lubridate::day(DATE)))

#clim.3<-map(clim.2, ~.x %>% 
#	filter(YEAR >= 1973, YEAR <= 2023))

#clim.4 <- lapply(clim.3, function(dt)
#	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#clim.5<-rbindlist(lapply(names(clim.4),function(h){
#	dt<-clim.4[[h]]
#	data.table(
#		station_id = basename(h),n_rows = nrow(dt),min.date=min(dt$DATE, na.rm = TRUE),max.date=max(dt$DATE, na.rm = TRUE),
#		data.miss.precip = (sum(is.na(dt$PRCP)))/(nrow(dt)),
#		data.miss.tmax = (sum(is.na(dt$TMAX)))/(nrow(dt)),
#		data.miss.tmin = (sum(is.na(dt$TMIN)))/(nrow(dt)),
#		data.miss.tavg = (sum(is.na(dt$TAVG)))/(nrow(dt)))}))

#precip<-with(clim.5,subset(clim.5,data.miss.precip <=0.1,select = c("station_id","n_rows","data.miss.precip")))
#precip.1.1<-as.vector(precip$station_id)
#precip.1.0<-file.path(paste0(precip.1.1,".csv"))
#precip.1.2<-file.path("NOAA_climate_data_1",precip.1.0)
#precip.1.3<-setNames(lapply(precip.1.2,fread),tools::file_path_sans_ext(basename(precip.1.2)))

#precip.2.1<-map(precip.1.3,~.x%>%
#mutate(
#       DATE = as.Date(DATE),
#       YEAR = lubridate::year(DATE),
#       MONTH = lubridate::month(DATE),
#       DAY = lubridate::day(DATE)))

#precip.2.2<-map(precip.2.1,~.x%>%
#	filter(YEAR>=1973,YEAR<=2023))

#precip.2.3<-lapply(precip.2.2,function(a)
#	tidyr::complete(dplyr::mutate(a,DATE = as.Date(DATE)),DATE=seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day")))

#precip.2.4<-precip.2.3[sapply(precip.2.3,function(b) "PRCP" %in% names(b))]

#precip.2.5<- lapply(precip.2.4,function(c){
#       c$PRCP<-(as.numeric(c$PRCP)/10)*25.4
#       c
#})

#map <-lapply(precip.2.5, function(d){
#	aggregate(PRCP ~ YEAR*STATION, d, sum, na.rm = TRUE)})

#map.1<-rbindlist(lapply(names(map),function(e){
#	x<-map[[e]]
#	data.table(station_id = basename(e),map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))

#prcp<-rbindlist(lapply(names(precip.2.5),function(j){
#       x<-precip.2.5[[j]]
#       data.table(
#		station_id = basename(j),
#		mean.prcp.all = mean(x$PRCP, na.rm=TRUE),
#		sd.prcp.all = sd(x$PRCP,na.rm=TRUE),
#		mean.prcp.events = with(subset(x,PRCP!=0),mean(PRCP,na.rm=TRUE)),
#		sd.prcp.events = with(subset(x,PRCP!=0),sd(PRCP, na.rm=TRUE)),
#		ex.high.prcp.all = quantile(x$PRCP,c(.95),na.rm=TRUE),
#		high.prcp.all = quantile(x$PRCP,c(.75),na.rm=TRUE),
#		ex.high.prcp.events = with(subset(x,PRCP!=0),quantile(PRCP,c(.95),na.rm=TRUE)),
#		high.prcp.events = with(subset(x,PRCP!=0),quantile(PRCP,c(.75),na.rm=TRUE)))}))

#temp.max.0<-with(clim.5,subset(clim.5,data.miss.tmax<=0.1,select=c("station_id","n_rows","data.miss.tmax")))

#temp.max.1.1<-as.vector(temp.max.0$station_id)

#temp.max.1.2<-file.path(paste0(temp.max.1.1,".csv"))

#temp.max.1.3<-file.path("NOAA_climate_data_1",temp.max.1.2)

#temp.max.1.4<-setNames(lapply(temp.max.1.3,fread),tools::file_path_sans_ext(basename(temp.max.1.3)))

#temp.max.2.1<-map(temp.max.1.4,~.x%>%
#	mutate(DATE = as.Date(DATE),
#	YEAR = lubridate::year(DATE),
#	MONTH = lubridate::month(DATE),
#	DAY = lubridate::day(DATE)))

#temp.max.2.2<-map(temp.max.2.1, ~.x %>% 
#	filter(YEAR >= 1973, YEAR <= 2023))

#temp.max.2.3 <- lapply(temp.max.2.2, function(dt)
#	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#temp.max.2.3<-temp.max.2.3[sapply(temp.max.2.3,function(a) "TMAX" %in% names(a))]

#temp.max.2.4<- lapply(temp.max.2.3,function(b){
#	b$TMAX<-as.numeric(b$TMAX)
#	b$TMAX<-b$TMAX/10
#	b$TMAX<-b$TMAX-32
#	b$TMAX<-b$TMAX/1.8
#	b})

#avg.tmax <-lapply(temp.max.2.4, function(d){
#        aggregate(TMAX ~ STATION, d, mean, na.rm = TRUE)})


#temp.max<-rbindlist(lapply(names(temp.max.2.4),function(j){
#       x<-temp.max.2.4[[j]]
#       data.table(
#		station_id = basename(j),
#		mean.t.max = mean(x$TMAX, na.rm=TRUE),
#		sd.t.max = sd(x$TMAX,na.rm=TRUE),
#	        ex.high.max = quantile(x$TMAX,c(.95),na.rm=TRUE),
#		high.max = quantile(x$TMAX,c(.75),na.rm=TRUE),
#		low.max = quantile(x$TMAX,c(.25),na.rm=TRUE),
#		ex.low.max = quantile(x$TMAX,c(.05),na.rm=TRUE))}))

#temp.min<-with(clim.5,subset(clim.5,data.miss.tmin<=0.1,select=c("station_id","n_rows","data.miss.tmin")))

#temp.min.1.1<-as.vector(temp.min$station_id)

#temp.min.1.2<-file.path(paste0(temp.min.1.1,".csv"))

#temp.min.1.3<-file.path("NOAA_climate_data_1",temp.min.1.2)

#temp.min.1.4<-setNames(lapply(temp.min.1.3,fread),tools::file_path_sans_ext(basename(temp.min.1.3)))

#temp.min.2.1<-map(temp.min.1.4,~.x%>%
#        mutate(DATE = as.Date(DATE),
#        YEAR = lubridate::year(DATE),
#        MONTH = lubridate::month(DATE),
#        DAY = lubridate::day(DATE)))

#temp.min.2.2<-map(temp.min.2.1, ~.x %>%
#        filter(YEAR >= 1973, YEAR <= 2023))

#temp.min.2.3 <- lapply(temp.min.2.2, function(dt)
#        tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

#temp.min.2.3<-temp.min.2.3[sapply(temp.min.2.3,function(a) "TMIN" %in% names(a))]

#temp.min.2.4<- lapply(temp.min.2.3,function(b){
#        b$TMIN<-as.numeric(b$TMIN)/10
#        b$TMIN<-b$TMIN-32
#        b$TMIN<-b$TMIN/1.8
#        b})

#avg.tmin <-lapply(temp.min.2.4, function(d){
#        aggregate(TMIN ~ STATION, d, mean, na.rm = TRUE)})

#temp.min<-rbindlist(lapply(names(temp.min.2.4),function(i){
#       x<-temp.min.2.4[[i]]
#       data.table(
#		station_id = basename(i),
#	        mean.t.min = mean(x$TMIN,na.rm=TRUE),
#		sd.t.min = sd(x$TMIN,na.rm=TRUE),
#	        ex.high.min = quantile(x$TMIN,c(.95),na.rm=TRUE),
#		high.min = quantile(x$TMIN,c(.75),na.rm=TRUE),
#		low.min = quantile(x$TMIN,c(.25),na.rm=TRUE),
#		ex.low.min = quantile(x$TMIN,c(.05),na.rm=TRUE))}))

#one<-merge(temp.max,temp.min,by="station_id",all=TRUE)
#two<-merge(one,prcp,by="station_id",all=TRUE)

soil<-read.csv("/disks/home/abigail/thesis-repository/lp.clim.csv")

soil[,c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500")]<-NULL

soil.all<-merge(soil,two,by="station_id",all.x=FALSE,all.y=TRUE)

soil.all$date<-as.Date(soil.all$date)

soil.all<-soil.all %>% mutate(start.date = date %m-% years(1))

for(i in 1:nrow(soil.all)){
	x<-fread(file.path("NOAA_climate_data_1",paste0(soil.all$station_id[[i]],".csv")))
	x$DATE<-as.Date(x$DATE)
	x$PRCP<-((as.numeric(x$PRCP))/10)*25.4
	x$TMAX<-(((as.numeric(x$TMAX))/10)-32)/1.8
	x$TMIN<-(((as.numeric(x$TMIN))/10)-32)/1.8
	time<-x$DATE<=soil.all$date[i]&x$DATE>=soil.all$start.date[i]
	        soil.all$avg.prcp[i]<-with(subset(x,time),mean(PRCP,na.rm=TRUE))
	        soil.all$mean.prcp[i]<-with(subset(x,time&x$PRCP!=0),mean(PRCP,na.rm=TRUE))
	        soil.all$events[i]<-nrow(subset(x,time&x$PRCP!=0&x$PRCP!="NA"))
	        soil.all$dry.days[i]<-nrow(subset(x,time&x$PRCP==0))
	        soil.all$high.prcp.events[i]<-nrow(subset(x,time&x$PRCP>=soil.all$high.prcp.events&x$PRCP!="NA"))
	        soil.all$ex.high.prcp.events[i]<-nrow(subset(x,time&x$PRCP<=soil.all$ex.high.prcp.events&PRCP!="NA"))
	        soil.all$avg.temp.max[i]<-with(subset(x,time),mean(TMAX,na.rm=TRUE))
	        soil.all$ex.high.temp.max[i]<-nrow(subset(x,time&x$TMAX>=soil.all$ex.high.max[i]))
	        soil.all$high.temp.max[i]<-nrow(subset(x,time&x$TMAX>=soil.all$high.max[i]))
	        soil.all$ex.low.temp.max[i]<-nrow(subset(x,time&x$TMAX<=soil.all$ex.low.max[i]))
	        soil.all$low.temp.max[i]<-nrow(subset(x,time&x$TMAX<=soil.all$low.max[i]))
	        soil.all$avg.temp.min[i]<-with(subset(x,x$DATE<=soil.all$date[i]&x$DATE>=soil.all$start.date[i]),mean(TMIN,na.rm=TRUE))
	        soil.all$ex.high.temp.min[i]<-nrow(subset(x,time&x$TMIN>=soil.all$ex.high.min[i]))
	        soil.all$high.temp.min[i]<-nrow(subset(x,time&x$TIN>=soil.all$high.min[i]))
	        soil.all$ex.low.temp.min[i]<-nrow(subset(x,time&x$TMIN<=soil.all$ex.low.min[i]))
	        soil.all$low.temp.min[i]<-nrow(subset(x,time&x$TMIN<=soil.all$low.min[i]))}
