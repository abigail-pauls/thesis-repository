#library(data.table)
#library(tidyverse)

soil<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.final.csv")
#prcp<-read.csv("/disks/home/abigail/thesis-repository/code/data/prcp.csv")

soil$date<-as.Date(soil$date,format="%Y-%m-%d")

soil$YEAR  <- format(soil$date,"%Y")
soil$MONTH <- format(soil$date,"%m")
soil$DAY   <- format(soil$date,"%d")

soil<-soil %>% filter(YEAR >= 1973, YEAR <= 2023)

soil<-soil %>% mutate(start.date = date %m-% years(1))

soil[,c("tceq","orgm","orgc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500",
       "max_lat","max_lon","licence","station_name","year","month","month.1","month.2","day","day.1","day.2","day.3","extra","layer_name")]<-NULL

names(soil)[names(soil)=="tceq_avg"]<-"tceq"
names(soil)[names(soil)=="orgm_avg"]<-"orgm"
names(soil)[names(soil)=="orgc_avg"]<-"orgc"
names(soil)[names(soil)=="nitkjd_avg"]<-"nitkjd"
names(soil)[names(soil)=="ecec_avg"]<-"ecec"
names(soil)[names(soil)=="cfgr_avg"]<-"cfgr"
names(soil)[names(soil)=="cfvo_avg"]<-"cfvo"
names(soil)[names(soil)=="clay_avg"]<-"clay"
names(soil)[names(soil)=="silt_avg"]<-"silt"
names(soil)[names(soil)=="sand_avg"]<-"sand"
names(soil)[names(soil)=="phaq_avg"]<-"phaq"
names(soil)[names(soil)=="phetol_avg"]<-"phetol"
names(soil)[names(soil)=="wg1500_avg"]<-"wg1500"
names(soil)[names(soil)=="min_lon"]<-"noaa.lon"
names(soil)[names(soil)=="min_lat"]<-"noaa.lat"
names(soil)[names(soil)=="latitude"]<-"wosis.lat"
names(soil)[names(soil)=="longitude"]<-"wosis.lon"
names(soil)[names(soil)=="elevation"]<-"noaa.elev"

setDT(soil)
setDT(prcp)
setDT(limits.prcp)

soil[,row_id := .I]

prcp<-rbindlist(precip.2.5,idcol="station_id",fill=TRUE)

one<-soil[,c("station_id","date","start.date","row_id")]
two<-limits.prcp[,c("station_id","xhigh.prcp","high.prcp")]

three<-merge(one,two,by="station_id",all=TRUE)

four<-subset(three,!is.na(three$xhigh.prcp)|!is.na(three$high))

prcp.id <- split(prcp[, .(DATE, PRCP)], prcp$station_id)

limits.id <- split(three[, .(row_id,station_id,start.date,date,xhigh.prcp,high.prcp)], three$row_id)

filter.prcp<-list()

for(j in names(limits.id)){
        id<-limits.id[[j]]$station_id
if(is.null(prcp.id[[id]])) next
        data<-prcp.id[[id]]
        limits<-limits.id[[j]]
        sd<-limits$start.date
        ed<-limits$date
filter.prcp[[j]]<-data %>% dplyr::filter(DATE<=ed & DATE>=sd)}

prcp.metric<-list()

for(i in names(filter.prcp)){
	xhigh<-limits.id[[i]]$xhigh.prcp
	high<-limits.id[[i]]$high.prcp
temp<-list()
temp$prcp.avg <- with(filter.prcp[[i]],mean(PRCP,na.rm=TRUE))
temp$prcp.mean <- with(filter.prcp[[i]],mean(PRCP[PRCP!=0],na.rm=TRUE)) 
temp$prcp.events <- with(filter.prcp[[i]],sum(PRCP!=0))
temp$dry.days <- with(filter.prcp[[i]],sum(PRCP==0))
temp$prcp.xhigh <- with(filter.prcp[[i]],sum(PRCP>=xhigh))
temp$prcp.high <- with(filter.prcp[[i]],sum(PRCP>=high))
temp$dry.days.length <- norm(filter.prcp[[i]]$PRCP==0)$mean
temp$prcp.xhigh.length <- norm(filter.prcp[[i]]$PRCP>=xhigh)$mean
temp$prcp.high.length <- norm(filter.prcp[[i]]$PRCP>=high)$mean
prcp.metric[[i]]<-temp}

prcp.metric.1<-rbindlist(prcp.metric,idcol="row_id",fill=TRUE)

prcp.metric.1$row_id<-as.integer(prcp.metric.1$row_id)

final<-merge(soil,prcp.metric.1,by="row_id",all.x=TRUE,all.y=TRUE)

setDT(tmax)
setDT(limits.tmax)

tmax<-rbindlist(temp.max.2.4,idcol="station_id",fill=TRUE)

four<-limits.tmax[,c("station_id","xhigh.tmax","high.tmax","low.tmax","xlow.tmax")]

five<-merge(one,four,by="station_id",all=TRUE)

six<-subset(five,!is.na(five$xhigh.tmax)|!is.na(five$high.tmax)|!is.na(five$xlow.tmax)|!is.na(five$low.tmax))

tmax.id <- split(tmax[, .(DATE, TMAX)], tmax$station_id)

limits.tmax.id <- split(six[, .(row_id,station_id,start.date,date,xhigh.tmax,high.tmax,xlow.tmax,low.tmax)], six$row_id)

filter.tmax<-list()

for(j in names(limits.tmax.id)){
        id<-limits.tmax.id[[j]]$station_id
if(is.null(tmax.id[[id]])) next
        data<-tmax.id[[id]]
        limits<-limits.tmax.id[[j]]
        sd<-limits$start.date
        ed<-limits$date
filter.tmax[[j]]<-data %>% dplyr::filter(DATE<=ed & DATE>=sd)}

tmax.metric<-list()

for(i in names(filter.tmax)){
        xhigh<-limits.tmax.id[[i]]$xhigh.tmax
        high<-limits.tmax.id[[i]]$high.tmax
	low<-limits.tmax.id[[i]]$low.tmax
	xlow<-limits.tmax.id[[i]]$xlow.tmax
temp<-list()
temp$tmax.avg <- with(filter.tmax[[i]],mean(TMAX,na.rm=TRUE))
temp$tmax.xhigh <- with(filter.tmax[[i]],sum(TMAX>=xhigh))
temp$tmax.high <- with(filter.tmax[[i]],sum(TMAX>=high))
temp$tmax.low <- with(filter.tmax[[i]],sum(TMAX<=low))
temp$tmax.xlow <- with(filter.tmax[[i]],sum(TMAX<=xlow))
temp$tmax.xhigh.length <- norm(filter.tmax[[i]]$TMAX>=xhigh)$mean
temp$tmax.high.length <- norm(filter.tmax[[i]]$TMAX>=high)$mean
temp$tmax.low.length <- norm(filter.tmax[[i]]$TMAX<=low)$mean
temp$tmax.xlow.length <- norm(filter.tmax[[i]]$TMAX<=xlow)$mean
tmax.metric[[i]]<-temp}

tmax.metric.1<-rbindlist(tmax.metric,idcol="row_id",fill=TRUE)

tmax.metric.1$row_id<-as.integer(tmax.metric.1$row_id)

final<-merge(final,tmax.metric.1,all.x=TRUE,all.y=TRUE)

setDT(tmin)
setDT(limits.tmin)

tmin<-rbindlist(temp.min.2.4,idcol="station_id",fill=TRUE)

seven<-limits.tmin[,c("station_id","xhigh.tmin","high.tmin","low.tmin","xlow.tmin")]

eight<-merge(one,seven,by="station_id",all=TRUE)

nine<-subset(eight,!is.na(eight$xhigh.tmin)|!is.na(eight$high.tmin)|!is.na(eight$xlow.tmin)|!is.na(eight$low.tmin))

tmin.id <- split(tmin[, .(DATE, TMIN)], tmin$station_id)

limits.tmin.id <- split(nine[, .(row_id,station_id,start.date,date,xhigh.tmin,high.tmin,xlow.tmin,low.tmin)], nine$row_id)

filter.tmin<-list()

for(j in names(limits.tmin.id)){
        id<-limits.tmin.id[[j]]$station_id
if(is.null(tmin.id[[id]])) next
        data<-tmin.id[[id]]
        limits<-limits.tmin.id[[j]]
        sd<-limits$start.date
        ed<-limits$date
filter.tmin[[j]]<-data %>% dplyr::filter(DATE<=ed & DATE>=sd)}

tmin.metric<-list()

for(i in names(filter.tmin)){
        xhigh<-limits.tmin.id[[i]]$xhigh.tmin
        high<-limits.tmin.id[[i]]$high.tmin
        low<-limits.tmin.id[[i]]$low.tmin
        xlow<-limits.tmin.id[[i]]$xlow.tmin
temp<-list()
temp$tmin.avg <- with(filter.tmin[[i]],mean(TMIN,na.rm=TRUE))
temp$tmin.xhigh <- with(filter.tmin[[i]],sum(TMIN>=xhigh))
temp$tmin.high <- with(filter.tmin[[i]],sum(TMIN>=high))
temp$tmin.low <- with(filter.tmin[[i]],sum(TMIN<=low))
temp$tmin.xlow <- with(filter.tmin[[i]],sum(TMIN<=xlow))
temp$tmin.xhigh.length <- norm(filter.tmin[[i]]$TMIN>=xhigh)$mean
temp$tmin.high.length <- norm(filter.tmin[[i]]$TMIN>=high)$mean
temp$tmin.low.length <- norm(filter.tmin[[i]]$TMIN<=low)$mean
temp$tmin.xlow.length <- norm(filter.tmin[[i]]$TMIN<=xlow)$mean
tmin.metric[[i]]<-temp}

tmin.metric.1<-rbindlist(tmin.metric,idcol="row_id",fill=TRUE)

tmin.metric.1$row_id<-as.integer(tmin.metric.1$row_id)

final<-merge(final,tmin.metric.1,,all.x=TRUE,all.y=TRUE)

final<-subset(final,!is.na(final$prcp.avg)|!is.na(final$tmax.avg)|!is.na(final$tmin.avg))

write.csv(final,"/disks/home/abigail/thesis-repository/code/data/final.csv")
