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

#soil<-read.csv("/disks/home/abigail/thesis-repository/lp.clim.csv")

#soil[,c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500")]<-NULL

#soil.all<-merge(soil,two,by="station_id",all.x=FALSE,all.y=TRUE)

#soil.all$date<-as.Date(soil.all$date)

#soil.all<-soil.all %>% mutate(start.date = date %m-% years(1))

#setDT(soil.all)

#soil.all$date<-as.IDate(soil.all$date)

#soil.all$start.date<-as.IDate(soil.all$start.date)

#stations<-unique(soil.all$station_id)

#for(s in stations){
#   x<-fread(file.path("NOAA_climate_data_1/",s,".csv",fsep=""))
#   x$DATE<-as.IDate(x$DATE)
#   x$PRCP<-as.numeric(x$PRCP)/10*25.4
#   x$TMAX<-(as.numeric(x$TMAX)/10-32)/1.8
#   x$TMIN<-(as.numeric(x$TMIN)/10-32)/1.8 
#   rows<-which(soil.all$station_id==s)
#	for(i in rows){
#	   time<-x$DATE<=soil.all$date[i]&x$DATE>=soil.all$start.date[i]
#	      prcp_t<-x$PRCP[time]
#	      tmax_t<-x$TMAX[time]
#	      tmin_t<-x$TMIN[time]
#	        soil.all$prcp.avg[i]<-mean(prcp_t,na.rm=TRUE)
#	        soil.all$prcp.mean[i]<-mean(prcp_t!=0,na.rm=TRUE)
#	        soil.all$prcp.event[i]<-sum(prcp_t!=0,na.rm=TRUE)
#	        soil.all$dry.days[i]<-sum(prcp_t==0,na.rm=TRUE)
#	        soil.all$high.prcp.events[i]<-sum(prcp_t>=soil.all$high.prcp.events[i],na.rm=TRUE)
#	        soil.all$ex.high.prcp.events[i]<-sum(prcp_t<=soil.all$ex.high.prcp.events[i],na.rm=TRUE)	        
#		soil.all$tmax.avg[i]<-mean(tmax_t,na.rm=TRUE)
#	        soil.all$tmax.xhigh[i]<-sum(tmax_t>=soil.all$ex.high.max[i],na.rm=TRUE)	        
#		soil.all$tmax.high[i]<-sum(tmax_t>=soil.all$high.max[i],na.rm=TRUE)	        
#		soil.all$tmax.xlow[i]<-sum(tmax_t<=soil.all$ex.low.max[i],na.rm=TRUE)	        
#		soil.all$tmax.low[i]<-sum(tmax_t<=soil.all$low.max[i],na.rm=TRUE)		
#		soil.all$tmin.avg[i]<-mean(tmin_t,na.rm=TRUE)
#		soil.all$tmin.xhigh[i]<-sum(tmin_t>=soil.all$ex.high.min[i],na.rm=TRUE)
#	        soil.all$tmin.high[i]<-sum(tmin_t>=soil.all$high.min[i],na.rm=TRUE)
#	        soil.all$tmin.xlow[i]<-sum(tmin_t<=soil.all$ex.low.min[i],na.rm=TRUE)
#	        soil.all$tmin.low[i]<-sum(tmin_t<=soil.all$low.min[i],na.rm=TRUE)
#		a<-rle(prcp_t==0)
#		soil.all$avg.dry.days[i]<-mean(a$length[a$values])
#		b<-rle(tmax_t>=soil.all$ex.high.max[i])
#		soil.all$avg.xhigh.max[i]<-mean(b$length[b$values])
#		c<-rle(tmax_t>=soil.all$high.max[i])
 #               soil.all$avg.high.max[i]<-mean(c$length[c$values])
  #              d<-rle(tmin_t>=soil.all$ex.high.min[i])
   #             soil.all$avg.xhigh.min[i]<-mean(d$length[d$values])
#                e<-rle(tmin_t>=soil.all$high.min[i])
#                soil.all$avg.high.min[i]<-mean(e$length[e$values])
  #              f<-rle(tmax_t<=soil.all$ex.low.max[i])
 #               soil.all$avg.xlow.max[i]<-mean(f$length[f$values])
   #             g<-rle(tmax_t<=soil.all$low.max[i])
    #            soil.all$avg.low.max[i]<-mean(g$length[g$values])
     #           h<-rle(tmin_t<=soil.all$ex.low.min[i])
      #          soil.all$avg.xlow.min[i]<-mean(h$length[h$values])
       #         j<-rle(tmin_t<=soil.all$high.min[i])
        #        soil.all$avg.low.min[i]<-mean(j$length[j$values])}}

#for(s in stations){
#   x<-fread(file.path("NOAA_climate_data_1/",s,".csv",fsep=""))
#x[, DATE := as.IDate(DATE)]
#if ("PRCP" %in% names(x)) {
#  x[, PRCP := as.numeric(PRCP)]
#  x[, PRCP := PRCP/10 * 25.4]}
#else {
#  x[, PRCP := NA_real_]}
#if ("TMAX" %in% names(x)) {
#  x[, TMAX := as.numeric(TMAX)]
#  x[, TMAX := (TMAX/10 - 32)/1.8]}
#else {
#  x[, TMAX := NA_real_]}
#if ("TMIN" %in% names(x)) {
#  x[, TMIN := as.numeric(TMIN)]
#  x[, TMIN := (TMIN/10 - 32)/1.8]}
#else {
#  x[, TMIN := NA_real_]}
#   rows<-which(soil.all$station_id==s)
#        for(i in rows){
#            time<-x[DATE>=soil.all$start.date[i]&DATE<=soil.all$date[i]]
#              prcp_t<-time$PRCP
#              tmax_t<-time$TMAX
#              tmin_t<-time$TMIN
#                soil.all$prcp.avg[i]<-mean(prcp_t,na.rm=TRUE)
#                soil.all$prcp.mean[i]<-mean(prcp_t!=0,na.rm=TRUE)
#                soil.all$prcp.event[i]<-sum(prcp_t!=0,na.rm=TRUE)
#                soil.all$dry.days[i]<-sum(prcp_t==0,na.rm=TRUE)
#                soil.all$high.prcp.events[i]<-sum(prcp_t>=soil.all$high.prcp.events[i],na.rm=TRUE)
#                soil.all$ex.high.prcp.events[i]<-sum(prcp_t<=soil.all$ex.high.prcp.events[i],na.rm=TRUE)
#                soil.all$tmax.avg[i]<-mean(tmax_t,na.rm=TRUE)
#                soil.all$tmax.xhigh[i]<-sum(tmax_t>=soil.all$ex.high.max[i],na.rm=TRUE)
#                soil.all$tmax.high[i]<-sum(tmax_t>=soil.all$high.max[i],na.rm=TRUE)
#                soil.all$tmax.xlow[i]<-sum(tmax_t<=soil.all$ex.low.max[i],na.rm=TRUE)
#                soil.all$tmax.low[i]<-sum(tmax_t<=soil.all$low.max[i],na.rm=TRUE)
#                soil.all$tmin.avg[i]<-mean(tmin_t,na.rm=TRUE)
#                soil.all$tmin.xhigh[i]<-sum(tmin_t>=soil.all$ex.high.min[i],na.rm=TRUE)
#                soil.all$tmin.high[i]<-sum(tmin_t>=soil.all$high.min[i],na.rm=TRUE)
#                soil.all$tmin.xlow[i]<-sum(tmin_t<=soil.all$ex.low.min[i],na.rm=TRUE)
#                soil.all$tmin.low[i]<-sum(tmin_t<=soil.all$low.min[i],na.rm=TRUE)
#                r<-rle(prcp_t==0)
#                soil.all$avg.dry.days[i]<-mean(r$length[r$values])
#}}


#write.csv(soil.all,"/disks/abigail/home/thesis-repository/soil.all.csv")

names(soil.all)[names(soil.all)=="latitude.x"]<-"wosis.lat"
names(soil.all)[names(soil.all)=="longitude.x"]<-"wosis.lon"

soil.all[,c("lat","lon","layer_id","profile_code.x","dataset_id","dataset_code","min_lat","max_lat","min_lon","max_lon")]<-NULL

names(soil.all)[names(soil.all)=="tceq_avg"]<-"tceq"
names(soil.all)[names(soil.all)=="orgm_avg"]<-"orgm"
names(soil.all)[names(soil.all)=="orgc_avg"]<-"orgc"
names(soil.all)[names(soil.all)=="nitkjd_avg"]<-"nitkjd"
names(soil.all)[names(soil.all)=="ecec_avg"]<-"ecec"
names(soil.all)[names(soil.all)=="cfgr_avg"]<-"cfgr"
names(soil.all)[names(soil.all)=="cfvo_avg"]<-"cfvo"
names(soil.all)[names(soil.all)=="clay_avg"]<-"clay"
names(soil.all)[names(soil.all)=="silt_avg"]<-"silt"
names(soil.all)[names(soil.all)=="sand_avg"]<-"sand"
names(soil.all)[names(soil.all)=="phaq_avg"]<-"phaq"
names(soil.all)[names(soil.all)=="phetol_avg"]<-"phetol"
names(soil.all)[names(soil.all)=="wg1500_avg"]<-"wg1500"
names(soil.all)[names(soil.all)=="longitude.y"]<-"noaa.lon"
names(soil.all)[names(soil.all)=="latitude.y"]<-"noaa.lat"
