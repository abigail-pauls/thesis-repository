library(tidyverse)
library(lubridate)
library(data.table)

data<-read.csv("/disks/home/abigail/thesis-repository/data/final.part.2.csv")

columns<-as.vector(names(data))

normality<-as.list()

for(i in columns){
	normality[i]<-shapiro.test(data[[i]])}

not.normal<-with(normality, subset(normality, p-value<=0.05))


