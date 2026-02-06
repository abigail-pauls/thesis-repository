#library(lubridate)
#library(tidyverse)
#library(data.table)

#prcp<-read.csv("/disks/home/abigail/thesis-repository/precipitation.csv") 
#soil<-read.csv("/disks/home/abigail/thesis-repository/lp.clim.csv")

#soil.prcp<-merge(soil,prcp,all.x=FALSE,all.y=TRUE)

#soil.prcp.1<-with(soil.prcp,subset(soil.prcp,select=c("station_id","date","year","month","day")))

#soil.prcp.1$date<-as.Date(soil.prcp.1$date)

#soil.prcp.1<-soil.prcp.1 %>% mutate(start.date = date %m-% years(1))

#clim.1<-with(clim,subset(clim,min.date<="1973-01-01"&max.date>="2023-01-01"))
#clim.1.1<-as.vector(clim.1$station_id)
#clim.1.2.files<-file.path("NOAA_climate_data_1",clim.1.1)
#clim.1.3.files<-setNames(lapply(clim.1.2.files,fread),tools::file_path_sans_ext(basename(clim.1.2.files)))

#clim.2<-map(clim.1.3.files,~.x %>%
#mutate(DATE = as.Date(DATE),YEAR = lubridate::year(DATE),MONTH = lubridate::month(DATE),DAY = lubridate::day(DATE)))

#clim.3<-map(clim.2, ~.x %>% filter(YEAR >= 1973, YEAR <= 2023))

#clim.4 <- lapply(clim.3, function(dt)
#tidyr::complete(dplyr::mutate(dt, DATE = as.Date(DATE)),DATE = seq(as.Date("1973-01-01"), as.Date("2023-12-31"), by = "day")))

soil.prcp.2<-as.vector(soil.prcp.1$station_id)

soil.prcp.2.1<-file.path(paste0(soil.prcp.2,".csv"))

soil.prcp.2.2<-file.path("NOAA_climate_data_1",soil.prcp.2.1)

soil.prcp.2.3<-setNames(lapply(soil.prcp.2.2,fread),tools::file_path_sans_ext(basename(soil.prcp.2.2)))


soil.prcp.1.1$station_id<-as.factor(soil.prcp.1$station_id)

for(i in 1:nrow(soil.prcp.1.1)){
	x<-soil.prcp.2.3[[i]]
	soil.prcp.2.3$avg.precip<-with(subset(x,X$DATE>=soil.prcp.1$start.date&x$DATE<=soil.prcp.1$date),mean(PRCP,na.rm=TRUE))
        soil.prcp.2.3$mean.precip<-with(subset(x,X$DATE>=soil.prcp.1$start.date&x$DATE<=soil.prcp.1$date&x$PRCP!=0),mean(PRCP,na.rm=TRUE))

}
