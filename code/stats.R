#library(tidyverse)
#library(lubridate)
#library(data.table)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/final.final.csv")

#data$continent<-as.factor(data$continent)
#data$country_name<-as.factor(data$country_name)
#data$region<-as.factor(data$region)
#data$station_id<-as.factor(data$station_id)

#data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
#	ifelse(upper_depth>=21&upper_depth<=40,"two",
#	ifelse(upper_depth>=41&upper_depth<=60,"three",
#	ifelse(upper_depth>=61&upper_depth<=80,"four",
#	ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

#data$soil.layer<-as.factor(data$soil.layer)

model.list<-c(16:28)
model.out<-data.frame(variable=character(),Fvalue=numeric(),pvalue=numeric())
for(i in model.list){
	one<-data[,c(i,which(names(data)=="soil.layer"))]
	two<-subset(one,!is.na(one[,1])&one$soil.layer!="six")
if(length(unique(two[,2]))>1){
	model<-aov(two[,1]~two$soil.layer,)
	three<-summary(model)[[1]]
	model.out<-rbind(model.out,data.frame(variable=names(data)[i],Fvalue=three$`F value`[1],pvalue=three$`Pr(>F)`[1]))}}
model.out<-as.data.frame(model.out)
names(model.out)<-c("variable","Fvalue","pvalue")

model.out$sig<-with(model.out,ifelse(pvalue<0.05,"yes","no"))

#soil<-c(

#check<-list()

#for(i in names(filter.tmin)){
 #       xhigh<-limits.tmin.id[[i]]$xhigh.tmin
  #      high<-limits.tmin.id[[i]]$high.tmin
   #     low<-limits.tmin.id[[i]]$low.tmin
    #    xlow<-limits.tmin.id[[i]]$xlow.tmin
#temp<-list()
#temp$tmin.avg <- with(filter.tmin[[i]],mean(TMIN,na.rm=TRUE))
#temp$tmin.xhigh <- with(filter.tmin[[i]],sum(TMIN>=xhigh))
#temp$tmin.high <- with(filter.tmin[[i]],sum(TMIN>=high))
#temp$tmin.low <- with(filter.tmin[[i]],sum(TMIN<=low))
#temp$tmin.xlow <- with(filter.tmin[[i]],sum(TMIN<=xlow))
#temp$tmin.xhigh.length <- norm(filter.tmin[[i]]$TMIN>=xhigh)$mean
#temp$tmin.high.length <- norm(filter.tmin[[i]]$TMIN>=high)$mean
#temp$tmin.low.length <- norm(filter.tmin[[i]]$TMIN<=low)$mean
#temp$tmin.xlow.length <- norm(filter.tmin[[i]]$TMIN<=xlow)$mean
#tmin.metric[[i]]<-temp}

#for(i in soil){
#	check$metric[i]<-[[i]]
#	check$
