library(future.apply)
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

lp.all<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.final.csv")

soil.stations<-as.vector(unique(lp.all$station_id))

soil.stations<-soil.stations[! soil.stations %in% c("NA")]

station.file<-file.path(paste0(soil.stations,".csv"))

files<-file.path("/disks/home/abigail/thesis-repository/code/data/ghcn-daily",station.file)

files.1<-files[file.exists(files)]

plan(multisession)

data_list<-setNames(future_lapply(files.1,fread),tools::file_path_sans_ext(basename(files.1)))

clim<-data.frame(
	station_id = character(),
	min.date = character(),
	max.date = character())

for(i in names(data_list)){
	clim<-rbind(clim,data.frame(
	station_id = i,
	min.date = min(data_list[[i]]$DATE),
	max.date = max(data_list[[i]]$DATE)))}

clim.station<-read.csv("/disks/home/abigail/thesis-repository/code/data/clim.dates.csv")

clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
clim.1.1<-as.vector(clim.1$station_id)
clim.1.2<- data_list[names(data_list) %in% clim.1.1]

clim.2<-map(clim.1.2,~.x %>%
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
		station_id = basename(h),
		n_rows = nrow(dt),
		min.date=min(dt$DATE, na.rm = TRUE),
		max.date=max(dt$DATE, na.rm = TRUE),
		data.miss.precip = (sum(is.na(dt$PRCP)))/(nrow(dt)),
		data.miss.tmax = (sum(is.na(dt$TMAX)))/(nrow(dt)),
		data.miss.tmin = (sum(is.na(dt$TMIN)))/(nrow(dt)),
		data.miss.tavg = (sum(is.na(dt$TAVG)))/(nrow(dt)))}),fill=TRUE)

write.csv(clim.5,"/disks/home/abigail/thesis-repository/code/data/clim.5.csv")

clim.5<-read.csv("/disks/home/abigail/thesis-repository/code/data/clim.5.csv")

prcp<-with(clim.5,subset(clim.5,data.miss.precip<=0.1,select = c("station_id","n_rows","data.miss.precip")))

prcp<-clim.5[,c(2:3,6)]
prcp.1<-subset(prcp,prcp$data.miss.precip<=0.1)
prcp.1.1<-as.vector(prcp.1$station_id)
prcp.1.2<- clim.4[names(clim.4) %in% prcp.1.1]

prcp.2<-prcp.1.2[sapply(prcp.1.2, function(a) "PRCP" %in% names(a))]

prcp.2.1<-lapply(prcp.2, function(b){
	b$PRCP<-(as.numeric(b$PRCP)/10)*25.4
	b})

map <-lapply(prcp.2.1, function(b){
	aggregate(PRCP ~ YEAR*STATION, b, sum, na.rm = TRUE)})

map.1<-rbindlist(lapply(names(map),function(c){
	x<-map[[c]]
	data.table(
	station_id = basename(c),
	map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))

prcp<-rbindlist(prcp.2.1,idcol="station_id",fill=TRUE)

#prcp<-read.csv("/disks/home/abigail/thesis-repository/code/data/prcp.csv")

limits.prcp<-prcp[,.(
	xhigh.prcp = quantile(PRCP,c(.95),na.rm=TRUE),
	high.prcp = quantile(PRCP,c(.75),na.rm=TRUE),
	low.prcp = quantile(PRCP,c(.25),na.rm=TRUE),
	xlow.prcp = quantile(PRCP,c(.05),na.rm=TRUE)),
	by = station_id]

write.csv(limits.prcp,"/disks/home/abigail/thesis-repository/code/data/limits.prcp.csv")

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
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.avg.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.avg.sd = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.mean.u = NA_real_)
norm.prcp <- norm.prcp %>% dplyr::mutate(prcp.mean.sd = NA_real_)

for(h in names(prcp.2.1)){
	z<-prcp.2.1[[h]]
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
	norm.prcp[station_id==h,prcp.high.u := mean(r$n,na.rm=TRUE)]
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
	norm.prcp[station_id==h,dry.days.sd := sd(d$n,na.rm=TRUE)]}
norm.prcp[station_id==h,prcp.avg.u := mean(z$PRCP, na.rm=TRUE)]
norm.prcp[station_id==h,prcp.avg.sd := sd(z$PRCP, na.rm=TRUE)]
t<-subset(z,z$PRCP!=0)
if(nrow(t)>0){
	norm.prcp[station_id==h, prcp.mean.u := mean(t$PRCP,na.rm=TRUE)]
	norm.prcp[station_id==h, prcp.mean.sd := sd(t$PRCP,na.rm=TRUE)]}}

tmax.0<-clim.5[,c(2:3,7)]
tmax.1<-subset(tmax.0,tmax.0$data.miss.tmax<=0.1)
tmax.1.1<-as.vector(tmax.1$station_id)
tmax.1.2<- clim.4[names(clim.4) %in% tmax.1.1]

tmax.2<-map(tmax.1.2, ~.x %>% 
	filter(YEAR >= 1973, YEAR <= 2023))

tmax.2.1 <- lapply(tmax.2, function(dt)
	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

tmax.2.2<-tmax.2.1[sapply(tmax.2.1,function(a) "TMAX" %in% names(a))]

tmax.2.3<- lapply(tmax.2.2,function(b){
	b$TMAX<-as.numeric(b$TMAX)
	b$TMAX<-b$TMAX/10
	b$TMAX<-b$TMAX-32
	b$TMAX<-b$TMAX/1.8
	b})

avg.tmax<-rbindlist(lapply(names(tmax.2.3),function(e){
	x<-tmax.2.3[[e]]
	data.table(
	station_id = basename(e),
	avg.tmax = mean(x$TMAX,na.rm=TRUE))}))

tmax<-rbindlist(tmax.2.3,idcol="station_id",fill=TRUE)

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

for(h in names(tmax.2.3)){
	z<-tmax.2.3[[h]]
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

tmin.0<-clim.5[,c(2:3,8)]
tmin.1<-subset(tmin.0,tmin.0$data.miss.tmin<=0.1)
tmin.1.1<-as.vector(tmin.1$station_id)
tmin.1.2<- clim.4[names(clim.4) %in% tmin.1.1]

tmin.2<-map(tmin.1.2, ~.x %>%
	filter(YEAR >= 1973, YEAR <= 2023))

tmin.2.1 <- lapply(tmin.2, function(dt)
	tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

tmin.2.2<-tmin.2.1[sapply(tmin.2.1,function(a) "TMIN" %in% names(a))]

tmin.2.3<- lapply(tmin.2.2,function(b){
	b$TMIN<-as.numeric(b$TMIN)/10
	b$TMIN<-b$TMIN-32
	b$TMIN<-b$TMIN/1.8
	b})

avg.tmin<-rbindlist(lapply(names(tmin.2.3),function(w){
	x<-tmin.2.3[[w]]
	data.table(
	station_id = basename(w),
	avg.tmin = mean(x$TMIN,na.rm=TRUE))}))

tmin<-rbindlist(tmin.2.3,idcol="station_id",fill=TRUE)

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

for(h in names(tmin.2.3)){
	z<-tmin.2.3[[h]]
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
