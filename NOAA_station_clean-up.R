library(tidyverse)

lp.clim<-read.csv("lp.clim.csv")

stations<-as.vector(unique(lp.clim$station_id))

clim.stations<-file.path(paste0(stations,".csv"))

clim.stations.1<-(clim.stations[! clim.stations %in% c("NA.csv")])

data_dir<-"/disks/home/abigail/thesis-repository/NOAA_climate_data_1"

climate_data<-list()

for(i in unique(clim.stations.1)){
x<-gsub("//.csv$","",i)
climate_data[[x]]<-read.csv(file.path(data_dir,i))}

#temp<-data.frame()

#for(i in seq_along(stations)){
#if((is.na(i$temperature)/nrows(i$temperature))<=0.1){
#temp[i]<-stations[i]}





































^G Get Help      ^O Write Out     ^W Where Is      ^K Cut Text      ^J Justify       ^C Cur Pos       M-U Undo         M-A Mark Text    M-] To Bracket   M-▲ Previous
^X Exit          ^R Read File     ^\ Replace       ^U Uncut Text    ^T To Spell      ^_ Go To Line    M-E Redo         M-6 Copy Text    M-W WhereIs Next M-▼ Next

