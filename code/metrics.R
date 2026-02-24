#library(lubridate)
#library(tidyverse)
#library(data.table)
#library(jsonlite)
#library(sf)

#clim<-read.csv("/disks/home/abigail/thesis-repository/code/data/clim.dates.csv")

#clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
#clim.1.1<-as.vector(clim.1$station_id)
#clim.1.2.files<-file.path("data/ghcn-daily",clim.1.1)
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
#precip.1.2<-file.path("data/ghcn-daily",precip.1.0)
#precip.1.3<-setNames(lapply(precip.1.2,fread),tools::file_path_sans_ext(basename(precip.1.2)))

#precip.2.1<-map(precip.1.3,~.x%>%
#mutate(
#       DATE = as.Date(DATE),
#       YEAR = lubridate::year(DATE),
#       MONTH = lubridate::month(DATE),
#       DAY = lubridate::day(DATE)))

#precip.2.2<-map(precip.2.1,~.x%>%
#	filter(YEAR>=1973,YEAR<=2023))
#50
#precip.2.3<-lapply(precip.2.2,function(a)
#	tidyr::complete(dplyr::mutate(a,DATE = as.Date(DATE)),DATE=seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day")))

#precip.2.4<-precip.2.3[sapply(precip.2.3,function(b) "PRCP" %in% names(b))]

#precip.2.5<- lapply(precip.2.4,function(c){
#	c$PRCP<-(as.numeric(c$PRCP)/10)*25.4
#	c
#})

#map <-lapply(precip.2.5, function(d){
#	aggregate(PRCP ~ YEAR*STATION, d, sum, na.rm = TRUE)})

#map.1<-rbindlist(lapply(names(map),function(e){
#	x<-map[[e]]
#	data.table(
#	station_id = basename(e),
#	map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))

prcp<-rbindlist(lapply(names(precip.2.5),function(j){
	x<-precip.2.5[[j]]
	x.0<-subset(x,x$PRCP!=0)
	data.table(
		station_id = x$STATION[j],
		mean.prcp.all = mean(x$PRCP, na.rm=TRUE),
		sd.prcp.all = sd(x$PRCP,na.rm=TRUE),
		mean.prcp.events = mean(x.0$PRCP,na.rm=TRUE),
		sd.prcp.events = sd(x.0$PRCP, na.rm=TRUE),
		ex.high.prcp.all = quantile(x$PRCP,c(.95),na.rm=TRUE),
		high.prcp.all = quantile(x$PRCP,c(.75),na.rm=TRUE),
		ex.high.prcp.events = quantile(x.0$PRCP,c(.95),na.rm=TRUE),
		high.prcp.events = quantile(x.0$PRCP,c(.75),na.rm=TRUE))}))

norm.prcp<-rbindlist(lapply(names(precip.2.5),function(a){
	z<-precip.2.5[[a]]
	one<-subset(z,z$PRCP==0)
	two<-subset(z,z$PRCP!=0)
	b<-rle(one$PRCP)
	c<-aggregate(PRCP~YEAR,two,FUN=sum,na.rm=TRUE)
	d<-aggregate(PRCP~YEAR,one,FUN=sum,na.rm=TRUE)
	g<-subset(z,z$PRCP>=prcp$high.prcp.events[a])
	if(nrow(g)>0){
	e<-aggregate(PRCP~YEAR,g,FUN=sum,na.rm=TRUE)}
	h<-subset(z,z$PRCP<=prcp$ex.high.prcp.events[a])
	if(nrow(h)>0){
	f<-aggregate(PRCP~YEAR,h,FUN=sum,na.rm=TRUE)}
data.table(
	station_id = z$STATION[a],
	prcp.avg = mean(z$PRCP,na.rm=TRUE),
	prcp.avg.sd = sd(z$PRCP,na.rm=TRUE),
	prcp.mean = mean(two$PRCP,na.rm=TRUE),
	prcp.mean.sd = sd(two$PRCP,na.rm=TRUE),
	prcp.event.u = mean(c$PRCP,na.rm=TRUE),
	prcp.event.sd = sd(c$PRCP,na.rm=TRUE),
	dry.days.u = mean(d$PRCP,na.rm=TRUE),
	dry.days.sd = sd(d$PRCP,na.rm=TRUE),
	prcp.high.u = mean(e$PRCP,sum,na.rm=TRUE),
	prcp.high.sd = sd(e$PRCP,na.rm=TRUE),
	prcp.xhigh.u = mean(f$PRCP,na.rm=TRUE),
	prcp.xhigh.sd = sd(f$PRCP,na.rm=TRUE),
	dry.length.u = mean(b$length[b$values]),
	dry.length.sd = sd(b$length[b$values]))}))


temp.max.0<-with(clim.5,subset(clim.5,data.miss.tmax<=0.1,select=c("station_id","n_rows","data.miss.tmax")))

temp.max.1.1<-as.vector(temp.max.0$station_id)

temp.max.1.2<-file.path(paste0(temp.max.1.1,".csv"))

temp.max.1.3<-file.path("data/ghcn-daily",temp.max.1.2)

temp.max.1.4<-setNames(lapply(temp.max.1.3,fread),tools::file_path_sans_ext(basename(temp.max.1.3)))

temp.max.2.1<-map(temp.max.1.4,~.x%>%
	mutate(DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

temp.max.2.2<-map(temp.max.2.1, ~.x %>% 
	filter(YEAR >= 1973, YEAR <= 2023))
#101
temp.max.2.3 <- lapply(temp.max.2.2, function(dt)
	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

temp.max.2.3<-temp.max.2.3[sapply(temp.max.2.3,function(a) "TMAX" %in% names(a))]

temp.max.2.4<- lapply(temp.max.2.3,function(b){
	b$TMAX<-as.numeric(b$TMAX)
	b$TMAX<-b$TMAX/10
	b$TMAX<-b$TMAX-32
	b$TMAX<-b$TMAX/1.8
	b})

avg.tmax<-rbindlist(lapply(names(temp.max.2.4),function(e){
        x<-temp.max.2.4[[e]]
        data.table(
	station_id = basename(e),
	avg.tmax = mean(x$TMAX,na.rm=TRUE))}))

temp.max<-rbindlist(lapply(names(temp.max.2.4),function(j){
	x<-temp.max.2.4[[j]]
	data.table(
		station_id = basename(j),
		mean.t.max = mean(x$TMAX, na.rm=TRUE),
		sd.t.max = sd(x$TMAX,na.rm=TRUE),
	        ex.high.max = quantile(x$TMAX,c(.95),na.rm=TRUE),
		high.max = quantile(x$TMAX,c(.75),na.rm=TRUE),
		low.max = quantile(x$TMAX,c(.25),na.rm=TRUE),
		ex.low.max = quantile(x$TMAX,c(.05),na.rm=TRUE))}))

norm.temp.max<-rbindlist(lapply(names(temp.max.2.4),function(i){
	z<-temp.max.2.4
	b<-rle(subset(z,z$TMAX>=temp.max$ex.high.max[i]))
	c<-rle(subset(z,z$TMAX>=temp.max$high.max[i]))
	f<-rle(subset(z,z$TMAX<=temp.max$ex.low.max[i]))
	g<-rle(subset(z,z$TMAX<=temp.max$low.max[i]))
	h<-with(subset(z,z$TMAX>=temp.max$ex.high.max[i]),aggregate(TMAX~YEAR,FUN=sum,na.rm=TRUE))
	j<-with(subset(z,z$TMAX>=temp.max$high.max[i]),aggregate(TMAX~YEAR,FUN=sum,na.rm=TRUE))
	k<-with(subset(z,z$TMAX<=temp.max$ex.low.max[i]),aggregate(TMAX~YEAR,FUN=sum,na.rm=TRUE))
	m<-with(subset(z,z$TMAX<=temp.max$low.max[i]),aggregate(TMAX~YEAR,FUN=sum,na.rm=TRUE))
data.table(
	station_id = basename(i),
	tmax.avg = mean(z$TMAX,na.rm=TRUE),
	tmax.sd = sd(z$TMAX,na.rm=TRUE),
	tmax.xhigh.u = mean(h,na.rm=TRUE),
	tmax.xhigh.sd = sd(h,sum,na.rm=TRUE),
	tmax.high.u = mean(j,sum,na.rm=TRUE),
	tmax.high.sd = sd(j,na.rm=TRUE),
	tmax.xlow.u = mean(k,na.rm=TRUE),
	tmax.xlow.sd = sd(k,na.rm=TRUE),
	tmax.low.u = mean(m,na.rm=TRUE),
	tmax.low.sd = sd(m,na.rm=TRUE),
	tmax.xhigh.length.u = mean(b$length[b$values]),
	tmax.xhigh.length.sd = sd(b$length[b$values]),
	tmax.high.length.u = mean(c$length[c$values]),
	tmax.high.length.sd = sd(c$length[c$values]),
	tmax.xlow.length.u = mean(f$length[f$values]),
	tmax.xlow.length.sd = sd(f$length[f$values]),
	tmax.low.length.u = mean(g$length[g$values]),
	tmax.low.length.sd = sd(g$length[g$values]))}))


temp.min<-with(clim.5,subset(clim.5,data.miss.tmin<=0.1,select=c("station_id","n_rows","data.miss.tmin")))

temp.min.1.1<-as.vector(temp.min$station_id)

temp.min.1.2<-file.path(paste0(temp.min.1.1,".csv"))

temp.min.1.3<-file.path("data/ghcn-daily",temp.min.1.2)

temp.min.1.4<-setNames(lapply(temp.min.1.3,fread),tools::file_path_sans_ext(basename(temp.min.1.3)))

temp.min.2.1<-map(temp.min.1.4,~.x%>%
	mutate(DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

temp.min.2.2<-map(temp.min.2.1, ~.x %>%
	filter(YEAR >= 1973, YEAR <= 2023))

temp.min.2.3 <- lapply(temp.min.2.2, function(dt)
	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))
#151
temp.min.2.3<-temp.min.2.3[sapply(temp.min.2.3,function(a) "TMIN" %in% names(a))]

temp.min.2.4<- lapply(temp.min.2.3,function(b){
	b$TMIN<-as.numeric(b$TMIN)/10
	b$TMIN<-b$TMIN-32
	b$TMIN<-b$TMIN/1.8
	b})

avg.tmin<-rbindlist(lapply(names(temp.min.2.4),function(w){
	x<-temp.min.2.4[[w]]
	data.table(
	station_id = basename(w),
	avg.tmin = mean(x$TMIN,na.rm=TRUE))}))

temp.min<-rbindlist(lapply(names(temp.min.2.4),function(i){
	x<-temp.min.2.4[[i]]
	data.table(
		station_id = basename(i),
	        mean.t.min = mean(x$TMIN,na.rm=TRUE),
		sd.t.min = sd(x$TMIN,na.rm=TRUE),
	        ex.high.min = quantile(x$TMIN,c(.95),na.rm=TRUE),
		high.min = quantile(x$TMIN,c(.75),na.rm=TRUE),
		low.min = quantile(x$TMIN,c(.25),na.rm=TRUE),
		ex.low.min = quantile(x$TMIN,c(.05),na.rm=TRUE))}))

norm.temp.min<-rbindlist(lapply(names(temp.min.2.4),function(i){
	z<-temp.min.2.4[[i]]
	a<-with(subset(z,z$TMIN>=temp.min$ex.high.min[i]),aggregate(TMIN~YEAR,FUN=sum,na.rm=TRUE))
	b<-with(subset(z,z$TMIN>=temp.min$high.min[i]),aggregate(TMIN~YEAR,FUN=sum,na.rm=TRUE))
	c<-with(subset(z,z$TMIN<=temp.min$low.min[i]),aggregate(TMIN~YEAR,FUN=sum,na.rm=TRUE))
	f<-with(subset(z,z$TMIN<=temp.min$low.min[i]),aggregate(TMIN~YEAR,FUN=sum,na.rm=TRUE))
	d<-rle(subset(z,z$TMIN>=temp.min$ex.high.min[i]))
	e<-rle(subset(z,z$TMIN>=temp.min$high.min[i]))
	h<-rle(subset(z,z$TMIN<=temp.min$ex.low.min[i]))
	j<-rle(subset(z,z$TMIN<=temp.min$high.min[i]))
data.table(
	station_id = basename(i),
	tmin.avg = mean(z$TMIN,na.rm=TRUE),
	tmin.sd = sd(z$TMIN,na.rm=TRUE),
	min.xhigh.u = mean(a,na.rm=TRUE),
	tmin.xhigh.sd = sd(a,na.rm=TRUE),
	tmin.high = mean(b,na.rm=TRUE),
	tmin.high.sd = sd(b,na.rm=TRUE),
	tmin.xlow = mean(c,na.rm=TRUE),
	tmin.xlow.sd = sd(c,na.rm=TRUE),
	tmin.low = mean(f,na.rm=TRUE),
	tmin.low.sd = sd(f,na.rm=TRUE),
	tmin.xhigh.length.u = mean(d$length[d$values]),
	tmin.xhigh.length.sd = sd(d$length[d$values]),
	tmin.high.length.u = mean(e$length[e$values]),
	tmin.high.length.sd = sd(e$length[e$values]),
	tmin.xlow.length.u = mean(h$length[h$values]),
	tmin.xlow.length.sd = sd(h$length[h$values]),
	tmin.low.length.u = mean(j$length[j$values]),
	tmin.low.length.sd = sd(j$length[j$values]))}))


one<-merge(temp.max.1,temp.min.1,by="station_id",all=TRUE)
two<-merge(one,prcp.1,by="station_id",all=TRUE)

soil<-read.csv("/disks/home/abigail/thesis-repository/code/data/lp.clim.csv")

soil[,c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500")]<-NULL

soil.all<-merge(soil,two,by="station_id",all.x=FALSE,all.y=TRUE)

soil.all<-soil.all %>% mutate(start.date = date %m-% years(1))

stations<-soil.all$station_id

prcp.1<-rbindlist(lapply(stations,function(i){
        x<-precip.2.5[[i]]
        x[soil.all,on=.(DATE<=date,DATE>=start.date)]
        a<-rle(x$PRCP==0)
data.table(
        station_id = basename(i),
        prcp.avg = mean(x$PRCP,na.rm=TRUE),
        soil.prcp.mean = mean(x$PRCP!=0,na.rm=TRUE),
        prcp.event = sum(x$PRCP!=0,na.rm=TRUE),
        dry.days = sum(x$PRCP==0,na.rm=TRUE),
        prcp.high = sum(x$PRCP>=prcp$high.prcp.events[i],na.rm=TRUE),
        prcp.xhigh = sum(x$PRCP<=prcp$ex.high.prcp.events[i],na.rm=TRUE),
        avg.dry.days = mean(a$length[a$values]))}))

temp.min.1<-rbindlist(lapply(names(temp.min.2.4),function(p){
        y<-temp.min.2.4[[p]]
        d<-rle(y$TMIN>=temp.min$ex.high.min[p])
        e<-rle(y$TMIN>=temp.min$high.min[p])
        h<-rle(y$TMIN<=temp.min$ex.low.min[p])
        j<-rle(y$TMIN<=temp.min$high.min[p])
data.table(
        station_id = basename(p),
        tmin.avg=mean(y$TMIN,na.rm=TRUE),
        tmin.xhigh=sum(y$TMIN>=temp.min$ex.high.min[p],na.rm=TRUE),
        tmin.high=sum(y$TMIN>=temp.min$high.min[p],na.rm=TRUE),
        tmin.xlow=sum(y$TMIN<=temp.min$ex.low.min[p],na.rm=TRUE),
        tmin.low<-sum(y$TMIN<=temp.min$low.min[p],na.rm=TRUE),
        tmin.avg.xhigh = mean(d$length[d$values]),
        tmin.avg.high = mean(e$length[e$values]),
        tmin.avg.xlow = mean(h$length[h$values]),
        tmin.avg.low = mean(j$length[j$values]))}))

temp.max.1<-rbindlist(lapply(names(temp.max.2.4),function(s){
        y<-temp.max.2.4[[s]]
        b<-rle(y$TMAX>=temp.max$ex.high.max[s])
        c<-rle(y$TMAX>=temp.max$high.max[s])
        f<-rle(y$TMAX<=temp.max$ex.low.max[s])
        g<-rle(y$TMAX<=temp.max$low.max[s])
data.table(
        station_id = basename(s),
        tmax.avg = mean(y$TMAX,na.rm=TRUE),
        tmax.xhigh = sum(y$TMAX>=temp.max$ex.high.max[s],na.rm=TRUE),
        tmax.high = sum(y$TMAX>=temp.max$high.max[s],na.rm=TRUE),
        tmax.xlow = sum(y$TMAX<=temp.max$ex.low.max[s],na.rm=TRUE),
        tmax.low = sum(y$TMAX<=temp.max$low.max[s],na.rm=TRUE),
        tmax.avg.xhigh = mean(b$length[b$values]),
        tmax.avg.high = mean(c$length[c$values]),
        tmax.avg.xlow = mean(f$length[f$values]),
        tmax.avg.low = mean(g$length[g$values]))}))


#soil.all$date<-as.Date(soil.all$date)

#soil.all<-soil.all %>% mutate(start.date = date %m-% years(1))

#setDT(soil.all)

#data.seq<-data.table(DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day"))

#stations<-soil.all$station_id

#test<-lapply(stations,function(s){
#   y<-fread(file.path("data/ghcn-daily/",s,".csv",fsep=""))
#   y[,DATE := as.IDate(DATE)]
#   y<-y[DATE >= "1973-01-01"&DATE <= "2023-12-31"]
#   y<-merge(data.seq,y,by="DATE",all.x=TRUE)
#   y[,PRCP := as.numeric(PRCP)/10 * 25.4]
#   if("TMAX" %in% names(y)==TRUE){
#    y[,TMAX := (as.numeric(TMAX)/10 - 32)/1.8]}
#   if("TMIN" %in% names(y)==TRUE){
#    y[,TMIN := (as.numeric(TMIN)/10 - 32)/1.8]}
#   y[soil.all,on=.(DATE>=start.date,DATE<=date)]
#	a<-rle(y$PRCP==0)
#	b<-rle(y$TMAX>=soil.all$ex.high.max[s])
#	c<-rle(y$TMAX>=soil.all$high.max[s])
#	d<-rle(y$TMIN>=soil.all$ex.high.min[s])
#	e<-rle(y$TMIN>=soil.all$high.min[s])
#	f<-rle(y$TMAX<=soil.all$ex.low.max[s])
#	g<-rle(y$TMAX<=soil.all$low.max[s])
#	h<-rle(y$TMIN<=soil.all$ex.low.min[s])
#	j<-rle(y$TMIN<=soil.all$high.min[s])
#data.table(
#	station_id=s,
#	prcp.avg=mean(y$PRCP,na.rm=TRUE),
#	soil.prcp.mean=mean(y$PRCP!=0,na.rm=TRUE),
#	prcp.event=sum(y$PRCP!=0,na.rm=TRUE),
#	dry.days=sum(y$PRCP==0,na.rm=TRUE),
#	prcp.high=sum(y$PRCP>=soil.all$high.prcp.events[s],na.rm=TRUE),
#	prcp.xhigh=sum(y$PRCP<=soil.all$ex.high.prcp.events[s],na.rm=TRUE),
#	tmax.avg=mean(y$TMAX,na.rm=TRUE),
#	tmax.xhigh=sum(y$TMAX>=soil.all$ex.high.max[s],na.rm=TRUE),
#	tmax.high=sum(y$TMAX>=soil.all$high.max[s],na.rm=TRUE),
#	tmax.xlow=sum(y$TMAX<=soil.all$ex.low.max[s],na.rm=TRUE),
#	tmax.low=sum(y$TMAX<=soil.all$low.max[s],na.rm=TRUE),
#	tmin.avg=mean(y$TMIN,na.rm=TRUE),
#	tmin.xhigh=sum(y$TMIN>=soil.all$ex.high.min[s],na.rm=TRUE),
#	tmin.high=sum(y$TMIN>=soil.all$high.min[s],na.rm=TRUE),
#	tmin.xlow=sum(y$TMIN<=soil.all$ex.low.min[s],na.rm=TRUE),
#	tmin.low<-sum(y$TMIN<=soil.all$low.min[s],na.rm=TRUE),	  
#	avg.dry.days<-mean(a$length[a$values]),
#	tmax.avg.xhigh<-mean(b$length[b$values]),
#	tmax.avg.high<-mean(c$length[c$values]),
#	tmin.avg.xhigh<-mean(d$length[d$values]),
#	tmin.avg.high<-mean(e$length[e$values]),
#	tmax.avg.xlow<-mean(f$length[f$values]),
#	tmax.avg.low<-mean(g$length[g$values]),
#	tmin.avg.xlow<-mean(h$length[h$values]),
#	tmin.avg.low<-mean(j$length[j$values]))})

#write.csv(soil.all,"/disks/home/abigail/thesis-repository/code/data/soil.all.csv",row.names=FALSE)

#names(soil.all)[names(soil.all)=="tceq_avg"]<-"tceq"
#names(soil.all)[names(soil.all)=="orgm_avg"]<-"orgm"
#names(soil.all)[names(soil.all)=="orgc_avg"]<-"orgc"
#names(soil.all)[names(soil.all)=="nitkjd_avg"]<-"nitkjd"
#names(soil.all)[names(soil.all)=="ecec_avg"]<-"ecec"
#names(soil.all)[names(soil.all)=="cfgr_avg"]<-"cfgr"
#names(soil.all)[names(soil.all)=="cfvo_avg"]<-"cfvo"
#names(soil.all)[names(soil.all)=="clay_avg"]<-"clay"
#names(soil.all)[names(soil.all)=="silt_avg"]<-"silt"
#names(soil.all)[names(soil.all)=="sand_avg"]<-"sand"
#names(soil.all)[names(soil.all)=="phaq_avg"]<-"phaq"
#names(soil.all)[names(soil.all)=="phetol_avg"]<-"phetol"
#names(soil.all)[names(soil.all)=="wg1500_avg"]<-"wg1500"
#names(soil.all)[names(soil.all)=="longitude.y"]<-"noaa.lon"
#names(soil.all)[names(soil.all)=="latitude.y"]<-"noaa.lat"

#soil.all<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.all.csv")

three<-merge(soil.all,map.1,all.x=TRUE)
four<-merge(three,avg.tmax,all.x=TRUE)
final<-merge(four,avg.tmin,all.x=TRUE)

#prcp.final<-with(final,subset(final,!is.na(map.PRCP)))
#tmax.final<-with(final,subset(final,!is.na(avg.tmax)))
#tmin.final<-with(final,subset(final,!is.na(avg.tmin)))

#write.csv(final,"/disks/home/abigail/thesis-repository/code/data/final.csv",row.names=FALSE)

#final<-read.csv("/disks/home/abigail/thesis-repository/code/data/final.csv")

#for(i in files){
#x<-st_read(file.path("/disks/home/abigail/thesis-repository/code/data/eco-data-1.zip",i)
#for(j in 1:nrows(final)){
#	pt<-st_stf(st_point(c(final$wosis.lon[j],final$wosis.lat[j])),crs=4326)
#	results<-st_join(st_sf(geometry=pt),x)
#	final$ecosystem<-results$ecosystem_type}}

#for(s in stations){
#   z<-fread(file.path("data/ghcn-daily/",s,".csv",fsep=""))
#   z$DATE<-as.IDate(z$DATE)
#   z$PRCP<-as.numeric(z$PRCP)/10*25.4
#   z$TMAX<-(as.numeric(z$TMAX)/10-32)/1.8
#   z$TMIN<-(as.numeric(z$TMIN)/10-32)/1.8
#   z<-z[DATE >= as.Date("1973-01-01")&DATE <= as.Date("2023-12-31")]
#   z<-tidyr::complete(z,DATE=as.seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day"))
#      for(i in 1:nrow(soil.all)){
#         normal$prcp.avg[i]<-mean(z$PRCP,na.rm=TRUE)
#         normal$prcp.avg.sd[i]<-sd(z$PRCP,na.rm=TRUE)
#         normal$prcp.mean[i]<-mean(z$PRCP!=0,na.rm=TRUE)
#         normal$prcp.mean.sd[i]<-sd(z$PRCP!=0,na.rm=TRUE)
#         normal$prcp.event.u[i]<-mean(with(z$PRCP!=0,aggregate(PRCP~YEAR,sum,na.rm=TRUE)),na.rm=TRUE)
#	 normal$prcp.event.sd[i]<-sd(with(z$PRCP!=0,aggregate(PRCP~YEAR,sum,na.rm=TRUE)),na.rm=TRUE)
#         normal$dry.days.u[i]<-mean(with(z$PRCP==0,aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$dry.days.sd[i]<-sd(with(z$PRCP==0,aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$prcp.high.u[i]<-mean(with(z$PRCP>=soil.all$high.prcp.events[i],aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$prcp.high.sd[i]<-sd(with(z$PRCP>=soil.all$high.prcp.events[i],aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$prcp.xhigh.u[i]<-mean(with(z$PRCP<=soil.all$ex.high.prcp.events[i],aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$prcp.xhigh.sd[i]<-sd(with(z$PRCP<=soil.all$ex.high.prcp.events[i],aggregate(PRCP~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.avg[i]<-mean(z$TMAX,na.rm=TRUE)
#         normal$tmax.sd[i]<-sd(z$TMAX,na.rm=TRUE)
#         normal$tmax.xhigh.u[i]<-mean(with(z$TMAX>=soil.all$ex.high.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.xhigh.sd[i]<-sd(with(z$TMAX>=soil.all$ex.high.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.high.u[i]<-mean(with(z$TMAX>=soil.all$high.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.high.sd[i]<-sd(with(z$TMAX>=soil.all$high.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.xlow.u[i]<-mean(with(z$TMAX<=soil.all$ex.low.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.xlow.sd[i]<-sd(with(z$TMAX<=soil.all$ex.low.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.low.u[i]<-mean(with(z$TMAX<=soil.all$low.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmax.low.sd[i]<-sd(with(z$TMAX<=soil.all$low.max[i],aggregate(TMAX~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.avg[i]<-mean(z$TMIN,na.rm=TRUE)
#         normal$tmin.sd[i]<-sd(z$TMIN,na.rm=TRUE)
#         normal$tmin.xhigh.u[i]<-mean(with(z$TMIN>=soil.all$ex.high.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.xhigh.sd[i]<-sd(with(z$TMIN>=soil.all$ex.high.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.high[i]<-mean(with(z$TMIN>=soil.all$high.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.high.sd[i]<-sd(with(z$TMIN>=soil.all$high.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.xlow[i]<-mean(with(z$TMIN<=soil.all$ex.low.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.xlow.sd[i]<-sd(with(z$TMIN<=soil.all$ex.low.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.low[i]<-mean(with(z$TMIN<=soil.all$low.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#         normal$tmin.low.sd[i]<-sd(with(z$TMIN<=soil.all$low.min[i],aggregate(TMIN~YEAR,sum,na.rm=TRUE)))
#		a<-rle(prcp_t==0)
#		normal$dry.length.u[i]<-mean(a$length[a$values])
#		normal$dry.length.sd[i]<-sd(a$length[a$values])
#		b<-rle(z$TMAX>=soil.all$ex.high.max[i])
#		normal$tmax.xhigh.length.u[i]<-mean(b$length[b$values])
#		normal$tmax.xhigh.length.sd[i]<-sd(b$length[b$values])
#		c<-rle(z$TMAX>=soil.all$high.max[i])
#		normal$tmax.high.length.u[i]<-mean(c$length[c$values])
#		normal$tmax.high.length.sd[i]<-sd(c$length[c$values])
 #               d<-rle(z$TMIN>=soil.all$ex.high.min[i])
#		normal$tmin.xhigh.length.u[i]<-mean(d$length[d$values])
#		normal$tmin.xhigh.length.sd[i]<-sd(d$length[d$values])
#		e<-rle(z$TMIN>=soil.all$high.min[i])
#		normal$tmin.high.length.u[i]<-mean(e$length[e$values])
#		normal$tmin.high.length.sd[i]<-sd(e$length[e$values])
#		f<-rle(z$TMAX<=soil.all$ex.low.max[i])
#		norm$tmax.xlow.length.u[i]<-mean(f$length[f$values])
#		norm$tmax.xlow.length.sd[i]<-sd(f$length[f$values])
#		g<-rle(z$TMAX<=soil.all$low.max[i])
#		normal$tmax.low.length.u[i]<-mean(g$length[g$values])
#		normal$tmax.low.length.sd[i]<-sd(g$length[g$values])
 #               h<-rle(z$TMIN<=soil.all$ex.low.min[i])
#		normal$tmin.xlow.length.u[i]<-mean(h$length[h$values])
#		normal$tmin.xlow.length.sd[i]<-sd(h$length[h$values])
#		j<-rle(z$TMIN<=soil.all$high.min[i])
#		normal$tmin.low.length.u[i]<-mean(j$length[j$values])
#		normal$tmin.low.length.sd[i]<-sd(j$length[j$values])
#}}

#for(i in 1:nrow(final)){
#	final$n.prcp.avg[i]<-(final$prcp.avg[i]-normal$prcp.avg[i])/normal$prcp.avg.sd[i]
#	final$n.prcp.mean[i]<-(final$prcp.mean[i]-normal$prcp.mean[i])/normal$prcp.mean.sd[i]
#	final$n.prcp.event[i]<-(final$prcp.event[i]-normal$prcp.event.u[i])/normal$prcp.event.sd[i]
#	final$n.dry.days[i]<-(final$dry.days[i]-normal$dry.days.u[i])/normal$dry.days.u[i]
#	final$n.prcp.xhigh[i]<-(final$prcp.xhigh[i]-normal$prcp.xhigh.u[i])/normal$prcp.xhigh.sd[i]
#	final$n.prcp.high[i]<-(final$prcp.high[i]-normal$prcp.high.u[i])/normal$prcp.xhigh.sd[i]
#	final$n.tmax.avg[i]<-(final$tmax.avg[i]-normal$tmax.avg[i])/normal$tmax.avg.sd[i]
#	final$n.tmax.xhigh[i]<-(final$tmax.xhigh[i]-normal$tmax.xhigh.u[i])/normal$tmax.xhigh.sd[i]
#	final$n.tmax.high[i]<-(final$tmax.high[i]-normal$tmax.high.u[i])/normal$tmax.high.sd[i]
#	final$n.tmax.xlow[i]<-(final$tmax.xlow[i]-normal$tmax.xlow.u[i])/normal$tmax.xlow.sd[i]
#	final$n.tmax.low[i]<-(final$tmax.low[i]-normal$tmax.low.u[i])/normal$tmax.low.sd[i]
#	final$n.tmin.avg[i]<-(final$tmin.avg[i]-normal$tmin.avg[i])/normal$tmin.avg.sd[i]
#	final$n.tmin.xhigh[i]<-(final$tmin.xhigh[i]-normal$tmin.xhigh.u[i])/normal$tmin.xhigh.sd[i]
#	final$n.tmin.high[i]<-(final$tmin.high[i]-normal$tmin.high.u[i])/normal$tmin.high.sd[i]
#	final$n.tmin.xlow[i]<-(final$tmin.xlow[i]-normal$tmin.xlow.u[i])/normal$tmin.xlow.sd[i]
#	final$n.tmin.low[i]<-(final$tmin.low[i]-normal$tmin.low.u[i])/normal$tmin.low.sd[i]
#	final$n.dry.length[i]<-(final$avg.dry.days[i]-normal$dry.length.u[i])/normal$dry.length.sd[i]
#	final$n.tmax.xhigh.length[i]<-(final$tmax.avg.xhigh[i]-normal$tmax.xhigh.length.u[i])/normal$tmax.xhigh.length.sd[i]
#	final$n.tmax.high.length[i]<-(final$tmax.avg.high[i]-normal$tmax.high.length.u[i])/normal$tmax.high.length.sd[i]
#	final$n.tmax.xlow.length[i]<-(final$tmax.avg.xlow[i]-normal$tmax.xlow.length.u[i])/normal$tmax.xlow.length.sd[i]
#	final$n.tmax.low.length[i]<-(final$tmax.avg.low[i]-normal$tmax.low.length.u[i])/normal$tmax.low.length.sd[i]
#	final$n.tmin.xhigh.length[i]<-(final$tmin.avg.xhigh[i]-normal$tmin.xhigh.length.u[i])/normal$tmin.xhigh.length.sd[i]
 #       final$n.tmin.high.length[i]<-(final$tmin.avg.high[i]-normal$tmin.high.length.u[i])/normal$tmin.high.length.sd[i]
  #      final$n.tmin.xlow.length[i]<-(final$tmin.avg.xlow[i]-normal$tmin.xlow.length.u[i])/normal$tmin.xlow.length.sd[i]
#        final$n.tmin.low.length[i]<-(final$tmin.avg.low[i]-normal$tmin.low.length.u[i])/normal$tmin.low.length.sd[i]
#}

#write.csv(final,"/disks/home/abigail/thesis-repository/code/data/final.part.2.csv")
