#library(data.table)
#library(tidyverse)
#library(readxl)

#ecosystem<-read.csv("/disks/home/abigail/thesis-repository/code/data/ecosystems.csv")
#ecosystem.codes<-read_excel("/disks/home/abigail/thesis-repository/code/data/ecosystem.codes.xlsx")

#ecosystem.1<-ecosystem[,c("station_id","X")]

#colnames(ecosystem.1)<-c("Code","station_id")

#eco<-merge(ecosystem.1,ecosystem.codes,all.x=TRUE)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

#coords<-data[,c("station_id","noaa.lon","noaa.lat")]

#coords<-unique(coords)

#ecosystem.2<-merge(ecosystem.1,coords,all.x=TRUE)

#ecosystem.3 <- ecosystem.2 %>%
#  group_by(station_id) %>%
#  filter(n() > 1) %>%       
#  filter(!duplicated(Code)) %>%
#  ungroup()

#ecosystem.3 <-ecosystem.2 %>%
  group_by(station_id) %>%
  slice_max(order_by = occurrence, n = 1) %>%
  ungroup()

#data.eco<-merge(data,eco,all.x=TRUE)

#data.eco<-merge(data.eco,map.1,all.x=TRUE)
#data.eco<-merge(data.eco,avg.tmax,all.x=TRUE)
#data.eco<-merge(data.eco,avg.tmin,all.x=TRUE)

#check<-data[,c("station_id","wosis.lon","wosis.lat")]

#check.1<-unique(check)
