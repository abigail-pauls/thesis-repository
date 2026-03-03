library(lubridate)
library(tidyverse)
library(data.table)
library(jsonlite)
library(sf)

norm<-function(y){
	r<-rle(y)
	lengths <- r$lengths[r$values]
	list(
		mean = if (length(lengths)) mean(lengths,na.rm=TRUE) else NA_real_,
		sd = if (length(lengths)) sd(lengths,na.rm=TRUE) else NA_real_)}

clim<-read.csv("/disks/home/abigail/thesis-repository/code/data/clim.dates.csv")

clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
clim.1.1<-as.vector(clim.1$station_id)
clim.1.2.files<-file.path("data/ghcn-daily",clim.1.1)
clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

clim.2<-map(clim.1.3.files,~.x %>%
	mutate(DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

clim.3<-map(clim.2, ~.x %>% 
	filter(YEAR >= 1973, YEAR <= 2023))

clim.4 <- lapply(clim.3, function(dt)
	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

clim.5<-rbindlist(lapply(names(clim.4),function(h){
	dt<-clim.4[[h]]
	data.table(
		station_id = basename(h),n_rows = nrow(dt),min.date=min(dt$DATE, na.rm = TRUE),max.date=max(dt$DATE, na.rm = TRUE),
		data.miss.precip = (sum(is.na(dt$PRCP)))/(nrow(dt)),
		data.miss.tmax = (sum(is.na(dt$TMAX)))/(nrow(dt)),
		data.miss.tmin = (sum(is.na(dt$TMIN)))/(nrow(dt)),
		data.miss.tavg = (sum(is.na(dt$TAVG)))/(nrow(dt)))}),fill=TRUE)

precip<-with(clim.5,subset(clim.5,data.miss.precip <=0.1,select = c("station_id","n_rows","data.miss.precip")))
precip.1.1<-as.vector(precip$station_id)
precip.1.0<-file.path(paste0(precip.1.1,".csv"))
precip.1.2<-file.path("data/ghcn-daily",precip.1.0)
precip.1.3<-setNames(lapply(precip.1.2,fread),tools::file_path_sans_ext(basename(precip.1.2)))
precip.2.1<-map(precip.1.3,~.x%>%

mutate(
	DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

precip.2.2<-map(precip.2.1,~.x%>%
	filter(YEAR>=1973,YEAR<=2023))

precip.2.3<-lapply(precip.2.2,function(a)
	tidyr::complete(dplyr::mutate(a,DATE = as.Date(DATE)),DATE=seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day")))

precip.2.4<-precip.2.3[sapply(precip.2.3,function(b) "PRCP" %in% names(b))]

precip.2.5<- lapply(precip.2.4,function(c){
	c$PRCP<-(as.numeric(c$PRCP)/10)*25.4
	c
})

map <-lapply(precip.2.5, function(d){
	aggregate(PRCP ~ YEAR*STATION, d, sum, na.rm = TRUE)})

map.1<-rbindlist(lapply(names(map),function(e){
	x<-map[[e]]
	data.table(
	station_id = basename(e),
	map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))

prcp<-rbindlist(precip.2.5,idcol="station_id",fill=TRUE)

limits.prcp<-prcp[,.(
	xhigh.prcp = quantile(PRCP,c(.95),na.rm=TRUE),
	high.prcp = quantile(PRCP,c(.75),na.rm=TRUE),
	low.prcp = quantile(PRCP,c(.25),na.rm=TRUE),
	xlow.prcp = quantile(PRCP,c(.05),na.rm=TRUE)),
	by = station_id]

prcp<-limits.prcp[prcp,on = "station_id"]

norm.prcp<-prcp[,{
	xhigh<-norm(PRCP>=xhigh.prcp)
	high<-norm(PRCP>=high.prcp)
	low<-norm(PRCP<=low.prcp)
	xlow<-norm(PRCP<=xlow.prcp)
	dry<-norm(PRCP==0)
.(
	prcp.xhigh.length.u = xhigh$mean,
	prcp.xhigh.length.sd = xhigh$sd,
	prcp.high.length.u = high$mean,
	prcp.high.length.sd = high$sd,
	prcp.xlow.length.u = xlow$mean,
	prcp.xlow.length.sd = xlow$sd,
	prcp.low.length.u = low$mean,
	prcp.low.length.sd = low$sd,
	dry.days.length.u = dry$mean,
	dry.days.length.sd = dry$sd)},
by=station_id]

norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.xhigh.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.xhigh.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.high.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.high.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.xlow.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.xlow.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.low.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.low.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.event.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.event.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(dry.days.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(dry.days.sd = NA_real_)

for(h in names(precip.2.5)){
	z<-precip.2.5[[h]]
		xhigh<-limits.prcp[station_id==h,xhigh.prcp]
		high<-limits.prcp[station_id==h,high.prcp]
		low<-limits.prcp[station_id==h,low.prcp]
		xlow<-limits.prcp[station_id==h,xlow.prcp]
	q<-subset(z,z$PRCP>=xhigh)
if(nrow(q)>0){
	w<-q %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h, prcp.xhigh.u := mean(w$n,na.rm=TRUE)]
	norm.prcp[station_id==h, prcp.xhigh.sd := sd(w$n,na.rm=TRUE)]}
	j<-subset(z,z$PRCP>=high)
if(nrow(j)>0){
	r<-j %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h,prpc.high.u := mean(r$n,na.rm=TRUE)]
	norm.prcp[station_id==h,prcp.high.sd := sd(r$n,na.rm=TRUE)]}
	k<-subset(z,z$PRCP<=xlow)
if(nrow(k)>0){
	p<-k %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h,prcp.xlow.u := mean(p$n,na.rm=TRUE)]
	norm.prcp[station_id==h,prcp.xlow.sd := sd(p$n,na.rm=TRUE)]}
	m<-subset(z,z$PRCP<=low)
if(nrow(m)>0){
	n<-m %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h,prcp.low.u:=mean(n$n,na.rm=TRUE)]
	norm.prcp[station_id==h,prcp.low.sd:=sd(n$n,na.rm=TRUE)]}
	a<-subset(z,z$PRCP!=0)
if(nrow(a)>0){
	c<-a %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h,prcp.event.u := mean(c$n,na.rm=TRUE)]
	norm.prcp[station_id==h,prcp.event.sd := sd(c$n,na.rm=TRUE)]}
	b<-subset(z,z$PRCP==0)
if(nrow(b)>0){
	d<-b %>% group_by(YEAR) %>% tally()
	norm.prcp[station_id==h,dry.days.u := mean(d$n,na.rm=TRUE)]
	norm.prcp[station_id==h,dry.days.sd := sd(d$n,na.rm=TRUE)]}}

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

tmax<-rbindlist(temp.max.2.4,idcol="station_id",fill=TRUE)

limits.tmax<-tmax[,.(
	mean.tmax = mean(TMAX, na.rm=TRUE),
	sd.tmax = sd(TMAX,na.rm=TRUE),
	xhigh.tmax = quantile(TMAX,c(.95),na.rm=TRUE),
	high.tmax = quantile(TMAX,c(.75),na.rm=TRUE),
	low.tmax = quantile(TMAX,c(.25),na.rm=TRUE),
	xlow.tmax = quantile(TMAX,c(.05),na.rm=TRUE)),
	by = station_id]

tmax<-limits.tmax[tmax,on = "station_id"]

norm.tmax<-tmax[,{
	xhigh<-norm(TMAX>=xhigh.tmax)
	high<-norm(TMAX>=high.tmax)
	low<-norm(TMAX<=low.tmax)
	xlow<-norm(TMAX<=xlow.tmax)
.(
	tmax.avg = mean(TMAX,na.rm=TRUE),
	tmax.sd = sd(TMAX,na.rm=TRUE),
	tmax.xhigh.length.u = xhigh$mean,
	tmax.xhigh.length.sd = xhigh$sd,
	tmax.high.length.u = high$mean,
	tmax.high.length.sd = high$sd,
	tmax.xlow.length.u = xlow$mean,
	tmax.xlow.length.sd = xlow$sd,
	tmax.low.length.u = low$mean,
	tmax.low.length.sd = low$sd)},
by=station_id]

norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.xhigh.u = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.xhigh.sd = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.high.u = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.high.sd = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.xlow.u = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.xlow.sd = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.low.u = NA_real_)
norm.tmax <- norm.tmax %>% dplyr::mutate(tmax.low.sd = NA_real_)

for(h in names(temp.max.2.4)){
	z<-temp.max.2.4[[h]]
		xhigh<-limits.tmax[station_id==h,xhigh.tmax]
		high<-limits.tmax[station_id==h,high.tmax]
		low<-limits.tmax[station_id==h,low.tmax]
		xlow<-limits.tmax[station_id==h,xlow.tmax]
	q<-subset(z,z$TMAX>=xhigh)
if(nrow(q)>0){ 
	w<-q %>% group_by(YEAR) %>% tally()
	norm.tmax[station_id==h, tmax.xhigh.u := mean(w$n,na.rm=TRUE)]
	norm.tmax[station_id==h, tmax.xhigh.sd := sd(w$n,na.rm=TRUE)]}
	j<-subset(z,z$TMAX>=high)
if(nrow(j)>0){
	u<-j %>% group_by(YEAR) %>% tally()
	norm.tmax[station_id==h,tmax.high.u := mean(u$n,na.rm=TRUE)]
	norm.tmax[station_id==h,tmax.high.sd := sd(u$n,na.rm=TRUE)]}
	k<-subset(z,z$TMAX<=xlow)
if(nrow(k)>0){
	v<-k %>% group_by(YEAR) %>% tally()
	norm.tmax[station_id==h,tmax.xlow.u := mean(v$n,na.rm=TRUE)]
	norm.tmax[station_id==h,tmax.xlow.sd := sd(v$n,na.rm=TRUE)]}
	m<-subset(z,z$TMAX<=low)
if(nrow(m)>0){
	n<-m %>% group_by(YEAR) %>% tally()
	norm.tmax[station_id==h,tmax.low.u:=mean(n$n,na.rm=TRUE)]
	norm.tmax[station_id==h,tmax.low.sd:=sd(n$n,na.rm=TRUE)]}}

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

tmin<-rbindlist(temp.min.2.4,idcol="station_id",fill=TRUE)

limits.tmin<-tmin[,.(
	mean.tmin = mean(TMIN, na.rm=TRUE),
	sd.tmin = sd(TMIN,na.rm=TRUE),
	xhigh.tmin = quantile(TMIN,c(.95),na.rm=TRUE),
	high.tmin = quantile(TMIN,c(.75),na.rm=TRUE),
	low.tmin = quantile(TMIN,c(.25),na.rm=TRUE),
	xlow.tmin = quantile(TMIN,c(.05),na.rm=TRUE)),
by = station_id]

tmin<-limits.tmin[tmin,on = "station_id"]

norm.tmin<-tmin[,{
	xhigh<-norm(TMIN>=xhigh.tmin)
	high<-norm(TMIN>=high.tmin)
	low<-norm(TMIN<=low.tmin)
	xlow<-norm(TMIN<=xlow.tmin)
.(
	tmin.avg = mean(TMIN,na.rm=TRUE),
	tmin.sd = sd(TMIN,na.rm=TRUE),
	tmin.xhigh.length.u = xhigh$mean,
	tmin.xhigh.length.sd = xhigh$sd,
	tmin.high.length.u = high$mean,
	tmin.high.length.sd = high$sd,
	tmin.xlow.length.u = xlow$mean,
	tmin.xlow.length.sd = xlow$sd,
	tmin.low.length.u = low$mean,
	tmin.low.length.sd = low$sd)},
by=station_id]

norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.xhigh.u = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.xhigh.sd = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.high.u = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.high.sd = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.xlow.u = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.xlow.sd = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.low.u = NA_real_)
norm.tmin <- norm.tmin %>% dplyr::mutate(tmin.low.sd = NA_real_)

for(h in names(temp.min.2.4)){
	z<-temp.min.2.4[[h]]
	xhigh<-limits.tmin[station_id==h,xhigh.tmin]
		high<-limits.tmin[station_id==h,high.tmin]
		low<-limits.tmin[station_id==h,low.tmin]
		xlow<-limits.tmin[station_id==h,xlow.tmin]
	q<-subset(z,z$TMIN>=xhigh)
if(nrow(q)>0){
	w<-q %>% group_by(YEAR) %>% tally()
	norm.tmin[station_id==h, tmin.xhigh.u := mean(w$n,na.rm=TRUE)]
	norm.tmin[station_id==h, tmin.xhigh.sd := sd(w$n,na.rm=TRUE)]}
	j<-subset(z,z$TMIN>=high)
if(nrow(j)>0){
	u<-j %>% group_by(YEAR) %>% tally()
	norm.tmin[station_id==h,tmin.high.u := mean(u$n,na.rm=TRUE)]
	norm.tmin[station_id==h,tmin.high.sd := sd(u$n,na.rm=TRUE)]}
	k<-subset(z,z$TMIN<=xlow)
if(nrow(k)>0){
	v<-k %>% group_by(YEAR) %>% tally()
	norm.tmin[station_id==h,tmin.xlow.u := mean(v$n,na.rm=TRUE)]
	norm.tmin[station_id==h,tmin.xlow.sd := sd(v$n,na.rm=TRUE)]}
	m<-subset(z,z$TMIN<=low)
if(nrow(m)>0){
	n<-m %>% group_by(YEAR) %>% tally()
	norm.tmin[station_id==h,tmin.low.u:=mean(n$n,na.rm=TRUE)]
	norm.tmin[station_id==h,tmin.low.sd:=sd(n$n,na.rm=TRUE)]}}

write.csv(norm.prcp,"/disks/home/abigail/thesis-repository/code/data/norm.prcp.csv")
write.csv(norm.tmax,"/disks/home/abigail/thesis-repository/code/data/norm.tmax.csv")
write.csv(norm.tmin,"/disks/home/abigail/thesis-repository/code/data/norm.tmin.csv")
write.csv(prcp,"/disks/home/abigail/thesis-repository/code/data/prcp.csv")
write.csv(tmax,"/disks/home/abigail/thesis-repository/code/data/tmax.csv")
write.csv(tmin,"/disks/home/abigail/thesis-repository/code/data/tmin.csv")


#soil<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.final.csv")

#soil$date<-as.Date(soil$date,format="%Y-%m-%d")

#soil$YEAR  <- format(soil$date,"%Y")
#soil$MONTH <- format(soil$date,"%m")
#soil$DAY   <- format(soil$date,"%d")

#soil<-soil %>% filter(YEAR >= 1973, YEAR <= 2023)

#soil<-soil %>% mutate(start.date = date %m-% years(1))

#soil[,c("tceq","orgm","orgc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500",
#	"max_lat","max_lon","licence","station_name","year","month","month.1","month.2","day","day.1","day.2","day.3","extra","layer_name")]<-NULL

#names(soil)[names(soil)=="tceq_avg"]<-"tceq"
#names(soil)[names(soil)=="orgm_avg"]<-"orgm"
#names(soil)[names(soil)=="orgc_avg"]<-"orgc"
#names(soil)[names(soil)=="nitkjd_avg"]<-"nitkjd"
#names(soil)[names(soil)=="ecec_avg"]<-"ecec"
#names(soil)[names(soil)=="cfgr_avg"]<-"cfgr"
#names(soil)[names(soil)=="cfvo_avg"]<-"cfvo"
#names(soil)[names(soil)=="clay_avg"]<-"clay"
#names(soil)[names(soil)=="silt_avg"]<-"silt"
#names(soil)[names(soil)=="sand_avg"]<-"sand"
#names(soil)[names(soil)=="phaq_avg"]<-"phaq"
#names(soil)[names(soil)=="phetol_avg"]<-"phetol"
#names(soil)[names(soil)=="wg1500_avg"]<-"wg1500"
#names(soil)[names(soil)=="min_lon"]<-"noaa.lon"
#names(soil)[names(soil)=="min_lat"]<-"noaa.lat"
#names(soil)[names(soil)=="latitude"]<-"wosis.lat"
#names(soil)[names(soil)=="longitude"]<-"wosis.lon"
#names(soil)[names(soil)=="elevation"]<-"noaa.elev"

#soil<-soil %>% dplyr::mutate(prcp.avg = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.mean = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.events = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh.length = NA_real_)

#setDT(prcp)
#setDT(soil)
#setDT(limits.prcp)

#prcp[,DATE := as.Date(DATE)]
#soil[,row_id := .I]

#test<-merge(soil[,.(row_id,station_id,start.date,date)],prcp,by="station_id",allow.cartesian = TRUE)

#test.1<-test[DATE<=date&DATE>=start.date]

#test.2<-merge(test.1,limits.prcp,by="station_id",all.x=TRUE)

#prcp.1<-test.2[,{
#	y.1<-PRCP!=0
#	y.2<-PRCP==0
#	y.3<-PRCP>=xhigh
#	y.4<-PRCP>=high
#
#list(
#        station_id = first(station_id),
#        prcp.avg = mean(PRCP,na.rm=TRUE),
#        prcp.mean = mean(PRCP[y.1],na.rm=TRUE),
#        prcp.events= y.1 %>% tally(),
#        dry.days = y.2 %>% tally(),
#        prcp.xhigh = y.3 %>% tally(),
#        prcp.high = y.4 %>% tally(),
#        dry.days.length = norm(PRCP==0)$mean,
#        prcp.high.length = norm(PRCP>=xhigh)$mean,
#        prcp.xhigh.length = norm(PRCP>=high)$mean)},
#by="row_id"]

#stations<-as.vector(stations)

#prcp$DATE<-as.Date(prcp$DATE)

#prcp.1<-lapply(stations,function(i){
#	y<-prcp[prcp$station_id==i,]
 #       start.date<-soil$start.date[soil$station_id==i][1]
  #      date<-soil$date[soil$station_id==i][1]
   #     y<-subset(y,y$DATE>=start.date&y$DATE<=date)
    #          xhigh<-limits.prcp$xhigh.prcp[limits.prcp$station_id==i]
     #         high<-limits.prcp$high.prcp[limits.prcp$station_id==i]
      #  y.1<-subset(y,y$PRCP!=0)
       # y.2<-subset(y,y$PRCP==0)
        #y.3<-subset(y,y$PRCP>=xhigh)
        #y.4<-subset(y,y$PRCP>=high)
#data.table(
#	station_id = i,
#	prcp.avg = mean(y$PRCP,na.rm=TRUE),
 #       prcp.mean = mean(y.1$PRCP,na.rm=TRUE),
  #      prcp.events= y.1 %>% tally(),
   #     dry.days = y.2 %>% tally(),
    #    prcp.xhigh = y.3 %>% tally(),
     #   prcp.high = y.4 %>% tally(),
      #  dry.days.length = norm(y$PRCP==0)$mean,
       # prcp.high.length = norm(y$PRCP>=xhigh)$mean,
        #prcp.xhigh.length = norm(y$PRCP>=high)$mean)})


#for(i in station){
#if(i %in% prcp$station_id){
#	y<-prcp[prcp$station_id==i,]
#	start.date<-soil$start.date[soil$station_id==i][1]
#	date<-soil$date[soil$station_id==i][1]
#	y$DATE<-as.Date(y$DATE)
#	y<-subset(y,y$DATE>=start.date&y$DATE<=date)
 #             xhigh<-limits.prcp$xhigh.prcp[limits.prcp$station_id==i]
  #            high<-limits.prcp$high.prcp[limits.prcp$station_id==i]
#	y.1<-subset(y,y$PRCP!=0)
#	y.2<-subset(y,y$PRCP==0)
#	y.3<-subset(y,y$PRCP>=xhigh)
#	y.4<-subset(y,y$PRCP>=high)
#		z.1<-norm(y$PRCP==0)
#		z.2<-norm(y$PRCP>=xhigh)
#		z.3<-norm(y$PRCP>=high)
#	soil$prcp.avg[soil$station_id==i]<-mean(y$PRCP,na.rm=TRUE)
#	soil$prcp.mean[soil$station_id==i]<-mean(y.1$PRCP,na.rm=TRUE)
#	soil$prcp.events[soil$staiton_id==i]<-y.1 %>% tally()
#	soil$dry.days[soil$station_id==i]<-y.2 %>% tally()
#	soil$prcp.xhigh[soil$station_id==i]<-y.3 %>% tally()
#	soil$prcp.high[soil$station_id==i]<-y.4 %>% tally()
#	soil$dry.days.length[soil$station_id==i]<-z.1$mean
#	soil$prcp.high.length[soil$station_id==i]<-z.2$mean
#	soil$prcp.xhigh.length[soil$station_id==i]<-z.3$mean
#}}

#soil <- soil %>% dplyr::mutate(tmax.xhigh = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.high = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.low = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.xlow = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.xhigh.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.high.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.low.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.xlow.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmax.mean = NA_real_)

#for(k in soil$station_id){
#	y<-temp.max.2.4[[k]]
#	start.date<-soil$start.date[k]
#	date<-soil$date[k]
#	 y<-map(y,~.x%>% filter(DATE>=start.date,DATE<=date))
 #               xhigh<-limits.tmax[station_id==k,xhigh.tmax]
  #              high<-limits.tmax[station_id==k,high.tmax]
#		low<-limits.tmax[station_id==k,low.tmax]
#		xlow<-limits.tmax[station_id==k,xlow.tmax]
#	y.1<-subset(y,y$TMAX>=xhigh)
#	y.2<-subset(y,y$TMAX>=high)
#	y.3<-subset(y,y$TMAX<=low)
#	y.4<-subset(y,y$TMAX<=xlow)
#		z.1<-norm(y$TMAX>=xhigh)
#		z.2<-norm(y$TMAX>=high)
#		z.3<-norm(y$TMAX<=low)
#		z.4<-norm(y$TMAX<=xlow)
#	soil[station_id==k, tmax.mean := mean(y$TMAX, na.rm=TRUE)]
#	soil[station_id==k, tmax.xhigh := sum(y.1$TMAX,na.rm=TRUE)]
#	soil[station_id==k, tmax.high := sum(y.2$TMAX,na.rm=TRUE)]
#	soil[station_id==k, tmax.low := sum(y.3$TMAX,na.rm=TRUE)]
#	soil[staiton_id==k, tmax.xlow := sum(y.4$TMAX,na.rm=TRUE)]
#		soil[station_id==k, tmax.xhigh.length := z.1$mean]
#		soil[station_id==k, tmax.high.length := z.2$mean]
#		soil[staiton_id==k, tmax.low.length := z.3$mean]
#		soil[station_id==k, tmax.xlow.length := z.4$mean]}

#soil <- soil %>% dplyr::mutate(tmin.xhigh = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.high = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.low = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.xlow = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.xhigh.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.high.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.low.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.xlow.length = NA_real_)
#soil <- soil %>% dplyr::mutate(tmin.mean = NA_real_)

#for(k in soil$station_id){
 #       y<-temp.min.2.4[[k]]
  #      start.date<-soil$start.date[k]
   #     date<-soil$date[k]
    #     y<-map(y,~.x%>% filter(DATE>=start.date,DATE<=date))
     #           xhigh<-limits.tmin[station_id==k,xhigh.tmin]
      #          high<-limits.tmin[station_id==k,high.tmin]
       #         low<-limits.tmin[station_id==k,low.tmin]
        #        xlow<-limits.tmin[station_id==k,xlow.tmin]
#        y.1<-subset(y,y$TMIN>=xhigh)
 #       y.2<-subset(y,y$TMIN>=high)
  #      y.3<-subset(y,y$TMIN<=low)
   #     y.3<-subset(y,y$TMIN<=xlow)
#		z.1<-norm(y.1$TMIN)
 #               z.2<-norm(y.2$TMIN)
  #              z.3<-norm(y.3$TMIN)
   #             z.4<-norm(y.4$TMIN)
#	soil[station_id==k, tmin.mean := mean(y$TMIN, na.rm=TRUE)]
 #       soil[station_id==k, tmin.xhigh := sum(y.1$TMIN,na.rm=TRUE)]
  ##      soil[station_id==k, tmin.high := sum(y.2$TMIN,na.rm=TRUE)]
    #    soil[station_id==k, tmin.low := sum(y.3$TMIN,na.rm=TRUE)]
     #   soil[staiton_id==k, tmin.xlow := sum(y.4$TMIN,na.rm=TRUE)]
#		soil[station_id==k, tmin.xhigh.length := z.1$mean]
 #               soil[station_id==k, tmin.high.length := z.2$mean]
  #              soil[staiton_id==k, tmin.low.length := z.3$mean]
   #             soil[station_id==k, tmin.xlow.length := z.4$mean]}


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

#three<-merge(soil.all,map.1,all.x=TRUE)
#four<-merge(three,avg.tmax,all.x=TRUE)
#final<-merge(four,avg.tmin,all.x=TRUE)

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
