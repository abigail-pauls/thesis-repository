library(tidyverse)
library(lubridate)
library(data.table)
library(lme4)
library(car)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/final.final.csv")

data<-final

data$continent<-as.factor(data$continent)
data$country_name<-as.factor(data$country_name)
data$region<-as.factor(data$region)
data$station_id<-as.factor(data$station_id)

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
	ifelse(upper_depth>=21&upper_depth<=40,"two",
	ifelse(upper_depth>=41&upper_depth<=60,"three",
	ifelse(upper_depth>=61&upper_depth<=80,"four",
	ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

#data$soil.layer<-as.factor(data$soil.layer)

#model.list<-c(16:28)
#model.out<-data.frame(variable=character(),Fvalue=numeric(),pvalue=numeric())
#for(i in model.list){
#	one<-data[,c(i,which(names(data)=="soil.layer"))]
#	two<-subset(one,!is.na(one[,1])&one$soil.layer!="six")
#if(length(unique(two[,2]))>1){
#	model<-aov(two[,1]~two$soil.layer,)
#	three<-summary(model)[[1]]
#	model.out<-rbind(model.out,data.frame(variable=names(data)[i],Fvalue=three$`F value`[1],pvalue=three$`Pr(>F)`[1]))}}
#model.out<-as.data.frame(model.out)
#names(model.out)<-c("variable","Fvalue","pvalue")

#model.out$sig<-with(model.out,ifelse(pvalue<0.05,"yes","no"))

data$decade<-with(data,ifelse(YEAR<=1983&YEAR>=1973,"seventies",
        ifelse(YEAR>1983&YEAR<=1993,"eighties",
        ifelse(YEAR>1993&YEAR<=2003,"ninties",
        ifelse(YEAR>2003&YEAR<=2013,"oughts",
        ifelse(YEAR>2013&YEAR<=2023,"tens","NA"))))))

data$decade<-as.factor(data$decade)

data.one<-subset(data,data$soil.layer=="one")
data.two<-subset(data,data$soil.layer=="two")
data.three<-subset(data,data$soil.layer=="three")
data.four<-subset(data,data$soil.layer=="four")
data.five<-subset(data,data$soil.layer=="five")
data.six<-subset(data,data$soil.layer=="six")

site<-unique(data.one$station_id)

orgc_n.xhigh.prcp<-data.frame(station_id=character(),estimate=numeric(),std.err=numeric(),
	t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(j in site){
	a<-data.one[which(data.one$station_id==j),]
	a<-na.omit(a[,c("orgc","n.prcp.xhigh")])
if(nrow(a)==0) next
if(nrow(a) < 3) next
if(length(unique(a$n.prcp.xhigh)) < 2) next
	model<-lm(orgc~n.prcp.xhigh,data=a)
	b<-summary(model)$coefficients
	c<-summary(model)
if(!"n.prcp.xhigh" %in% rownames(b)) next
	orgc_n.xhigh.prcp<-rbind(orgc_n.xhigh.prcp,data.frame(
		station_id=j,
		estimate=b[2,1],
		std.err=b[2,2],
		t.value=b[2,3],
		p.value=b[2,4],
		r2=c$r.squared,
		r2.adj=c$adj.r.squared))
}

#decade_orgc_n.prcp.xhigh<-data.frame(station_id=character(),estmate=numeric(),st.err=numeric(),t.value=numeric(),p.value=numeric())

#decades<-unique(data.one$decade)

#for(h in decades){
#	d<-data.one[which(data.one$decade==h),]}
#	d<-na.omit(d[,c("orgc","n.prcp.xhigh")])
#if(nrow(d)==0) next
#if(nrow(d)<3) next
#if(length(unique(d$n.prcp.xhigh))<2) next
#	model<-lm(orgc~n.prcp.xhigh+(1|station_id),data=d)
#	e<-summary(model)$varcor
#	test<-Anoova(model,type="II")
#	decade_orgc_n.prcp.xhigh<-rbind(decade_orgc_n.prcp.xhigh,data.frame(
#		station_id=h,
#		e$vcov,
#		test[,3]))}


