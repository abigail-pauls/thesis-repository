#matching soil collection sites with climate data sites and original attempt to calculate temperature metrics 

library(data.table)
library(tidyverse)
library(lubridate)

wosis<-read.csv("/disks/home/abigail/thesis-repository/wosis.lp.date.prof.csv")
ghcn.h<-read.csv("/disks/home/abigail/thesis-repository/ghcnh-station-list.csv")
ghcnh.inv<-read.table("/disks/home/abigail/thesis-repository/ghcnh-inventory.txt")

wosis$min_lat<-wosis$latitude-0.1
wosis$max_lat<-wosis$latitude+0.1
wosis$min_lon<-wosis$longitude-0.1
wosis$max_lon<-wosis$longitude+0.1

wosis$min_lat<-as.numeric(wosis$min_lat)
wosis$max_lat<-as.numeric(wosis$max_lat)
wosis$min_lon<-as.numeric(wosis$min_lon)
wosis$max_lon<-as.numeric(wosis$max_lon)

setDT(wosis)
setDT(ghcn.h)
clim.temp <- wosis[ghcn.h,on = .(min_lat <= LATITUDE, max_lat >= LATITUDE,min_lon <= LONGITUDE, max_lon >= LONGITUDE),nomatch = 0]

ghcn.h.stations<-as.vector(unique(clim.temp$GHCN_ID))

year<-as.vector(1973:2000)

stations<-unique(clim.temp$GHCN_ID)

sites<-ghcnh.inv[ghcnh.inv$V1 %in% ghcn.h.stations,]

colnames(sites)<-c("station_id","year","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

sites.1<-with(sites,subset(sites,jan>="670"&feb>="605"&mar>="670"&apr>="648"&may>="670"&jun>="648"&jul>="670"&aug>="670"&sep>="648"&oct>="670"&nov>="648"&dec>="670"))

sites.1 <- sites.1 %>%
  mutate(year = as.integer(year))

years<-1973:2023

sites.2 <- sites.1 %>% 
	filter(year %in% years) %>%
	distinct(station_id,year) %>%
	group_by(station_id) %>%
	filter(n_distinct(year) == length(years)) %>%
	ungroup()

soil<-read.csv("/disks/home/abigail/thesis-repository/lp.clim.csv")

soil.temp<-merge(soil,temp.max.3,all.x=FALSE,all.y=TRUE)

soil.temp$date<-as.Date(soil.temp$date)

soil.temp<-soil.temp %>% mutate(start.date = date %m-% years(1))

for(i in 1:nrow(soil.temp)){
x<-fread(file.path("NOAA_climate_data_1",paste0(soil.temp$station_id[[i]],".csv")))
x$DATE<-as.Date(x$DATE)
x$TMAX<-as.numeric(x$TMAX)
x$TMIN<-as.numeric(x$TMIN)
        time<-x$DATE<=soil.temp$date[i]&x$DATE>=soil.temp$start.date[i]
        soil.temp$avg.temp.max[i]<-with(subset(x,x$DATE<=soil.temp$date[i]&x$DATE>=soil.temp$start.date[i]),mean(TMAX,na.rm=TRUE))
	soil.temp$ex.high.temp.max[i]<-nrow(subset(x,time&x$TMAX>=soil.temp$ex.high.max[i]))
	soil.temp$high.temp.max[i]<-nrow(subset(x,time&x$TMAX>=soil.temp$high.max[i]))	
	soil.temp$ex.low.temp.max[i]<-nrow(subset(x,time&x$TMAX<=soil.temp$ex.low.max[i]))
	soil.temp$low.temp.max[i]<-nrow(subset(x,time&x$TMAX<=soil.temp$low.max[i]))
        soil.temp$avg.temp.min[i]<-with(subset(x,x$DATE<=soil.temp$date[i]&x$DATE>=soil.temp$start.date[i]),mean(TMIN,na.rm=TRUE))
        soil.temp$ex.high.temp.min[i]<-nrow(subset(x,time&x$TMIN>=soil.temp$ex.high.min[i]))
        soil.temp$high.temp.min[i]<-nrow(subset(x,time&x$TIN>=soil.temp$high.min[i]))
        soil.temp$ex.low.temp.min[i]<-nrow(subset(x,time&x$TMIN<=soil.temp$ex.low.min[i]))
        soil.temp$low.temp.min[i]<-nrow(subset(x,time&x$TMIN<=soil.temp$low.min[i]))

}


files<-file.path("NOAA_climate_data_1",clim.stations.1)

files.1<-files[file.exists(files)]

data_list<-setNames(lapply(files.1,fread),tools::file_path_sans_ext(basename(files.1)))

clim<-rbindlist(lapply(files.1,function(f){
dt<-fread(f)
data.table(
        station_id = basename(f),
        n_rows = nrow(dt),
        min.date=min(dt$DATE, na.rm = TRUE),
        max.date=max(dt$DATE, na.rm = TRUE))}))


files<-print0("GHCNh_",ghcn.h.stations,"_1973.psv")
files.1<-file.path("ghcn-hourly",ghcn.h.stations)
files.2<-files.1[file.exists(files.1)]
ghcn.h<-setNames(lapply(files.2,fread),tools::file_path_sans_ext(basename(files.1)))

#first make one big list with data from all years
#second: add int date, month, year, and hour columns to this data
#third: filter by years
#fourth: fill in all data with NAs
#fifth: remove all stations that are missing more than 10% of days
#sixth: remove all stations that do not have at least 17 hours recorded for each day
#seveth: merge all of these lists together to find the appropriate stations 

ghcn.h<-rbindlist(lapply(year,function(a){
files<-file.path("ghcn-hourly/","GHCNh_",ghcn.h.stations,"_",a,".psv",fsep="")
files.1<-files[file.exists(files)]
ghcn.h<-setNames(lapply(files.1,fread),
	tools::file_path_sans_ext(basename(files.1)))}))

ghcn.h.1<-map(ghcn.h,~x%>%
mutate(
	DATE = as.Date(DATE),
	YEAR = lubridate::year(DATE),
	MONTH = lubridate::month(DATE),
	DAY = lubridate::day(DATE)))

ghcn.h.2<-map(ghcn.h.1,~.x%>%
filter(YEAR>=1973,YEAR<=2023))

precip.2.3<-lapply(precip.2.2,function(a)
tidyr::complete(dplyr::mutate(a,DATE = as.Date(DATE)),DATE=seq(as.Date("1973-01-01"),as.Date("2023-12-31"),by="day")))

precip.2.4<-precip.2.3[sapply(precip.2.3,function(b) "PRCP" %in% names(b))]

precip.2.5<- lapply(precip.2.4,function(c){
       c$PRCP<-(as.numeric(c$PRCP)/10)*25.4
       c
})

map <-lapply(precip.2.5, function(d){aggregate(PRCP ~ YEAR*STATION, d, sum, na.rm = TRUE)})

map.1<-rbindlist(lapply(names(map),function(e){
x<-map[[e]]
data.table(station_id = basename(e),map = with(x,aggregate(PRCP~STATION,data=x,mean,na.rm=TRUE)))}))
