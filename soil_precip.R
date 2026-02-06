library(lubridate)
library(tidyverse)
library(data.table)

prcp<-read.csv("/disks/home/abigail/thesis-repository/precipitation.csv") 
soil<-read.csv("/disks/home/abigail/thesis-repository/lp.clim.csv")

soil.prcp<-merge(soil,prcp,all.x=FALSE,all.y=TRUE)

#soil.prcp.1<-with(soil.prcp,subset(soil.prcp,select=c("station_id","date","year","month","day")))

#soil.prcp.1$date<-as.Date(soil.prcp.1$date)

#soil.prcp.1<-soil.prcp.1 %>% mutate(start.date = date %m-% years(1))

#soil.prcp.2<-as.vector(soil.prcp.1$station_id)

#soil.prcp.2.1<-file.path(paste0(soil.prcp.2,".csv"))

#soil.prcp.2.2<-file.path("NOAA_climate_data_1",soil.prcp.2.1)

#soil.prcp.2.3<-setNames(lapply(soil.prcp.2.2,fread),tools::file_path_sans_ext(basename(soil.prcp.2.2)))

soil.prcp$date<-as.Date(soil.prcp$date)

soil.prcp<-soil.prcp %>% mutate(start.date = date %m-% years(1))

for(i in 1:nrow(soil.prcp)){
x<-fread(file.path("NOAA_climate_data_1",paste0(soil.prcp$station_id[[i]],".csv")))	
x$DATE<-as.Date(x$DATE)
x$PRCP<-as.numeric(x$PRCP)
	time<-x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]
	soil.prcp$avg.precip[i]<-with(subset(x,x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]),mean(PRCP,na.rm=TRUE))
        soil.prcp$mean.precip[i]<-with(subset(x,x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]&x$PRCP!=0),mean(PRCP,na.rm=TRUE))
	soil.prcp$events[i]<-nrow(subset(x,x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]&x$PRCP!=0&x$PRCP!="NA"))
	soil.prcp$ex.up.events[i]<-nrow(subset(x,x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]&x$PRCP>=soil.prcp$events.two.above&x$PRCP!="NA"))
	soil.prcp$ex.down.events[i]<-nrow(subset(x,x$DATE<=soil.prcp$date[i]&x$DATE>=soil.prcp$start.date[i]&PRCP<=soil.prcp$events.two.below&x$PRCP!="NA"))
}
