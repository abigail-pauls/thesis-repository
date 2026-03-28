library(data.table)
library(tidyverse)

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data[,c("X.2","X.1","X","HWSD2_SMU_ID","WRB2","cell_id","layer_id","organic_surface")]<-NULL

#map<-read.csv("/disks/home/abigail/thesis-repository/code/data/map.csv")
#avg.tmax<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmax.csv")
#avg.tmin<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmin.csv")

#map<-map[,c("station_id","map.PRCP")]
#colnames(map)<-c("station_id","map")

#avg.tmax<-avg.tmax[,c("station_id","avg.tmax")]
#colnames(avg.tmax)<-c("station_id","total.avg.tmax")

#avg.tmin<-avg.tmin[,c("station_id","avg.tmin")]
#colnames(avg.tmin)<-c("station_id","total.avg.tmin")

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
     ifelse(upper_depth>=21&upper_depth<=40,"two",
  ifelse(upper_depth>=41&upper_depth<=60,"three",
 ifelse(upper_depth>=61&upper_depth<=80,"four",
ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

data$soil.layer<-as.factor(data$soil.layer)

data.one<-subset(data,data$soil.layer=="one")

#site<-unique(data.one$station_id)

#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#water<-c("wg1500","wg0200","wg0033")
#water.sites<-list()

#k<-1

#for(i in water){
#	a<-substr(i,1,4)
#for(h in climate){
#	e<-substr(h,3,8)
 #      df<-data.frame(
 #     water=character(),climate=character(),station_id=character(),slope=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
#for(j in site){
   #   b<-data.one[data.one$station_id==j,c("station_id",i,h)]
    #   colnames(b)<-c("station_id","x","y")
     #  b$x<-as.numeric(b$x)
      # b$y<-as.numeric(b$y)
      # b<-b[is.finite(b$x)&is.finite(b$y),]
#if(nrow(b)==0) next
#if(nrow(b) < 3) next
 #       model<-lm(x~y,data=b)
  #      c<-summary(model)$coefficients
   #     d<-summary(model)
#if(nrow(c)<2|nrow(b)==0|nrow(b)<3) next
 #       df<-rbind(df,data.frame(
  #      water=i,climate=h,station_id=j,slope=c[2,1],p.value=c[2,4],r2=d$r.squared,r2.adj=d$adj.r.squared))}
#df$sig<-ifelse(df$p.value<0.05,"yes","no")
#water.sites[[k]]<-df
#colnames(water.sites[[k]])<-c(paste0("id.",a,sep=".",e),paste0("clim.",a,sep=".",e),"station_id",paste0("sl.",a,sep=".",e),
#                paste0("p.v.",a,sep=".",e),paste0("r2.",a,sep=".",e),paste0("r2.a.",a,sep=".",e),paste0("s.",a,sep=".",e))
#k<-k+1
#}}


#carbon<-c("orgc","totc","tceq")

#carbon.sites<-list()

#k<-1

#for(i in carbon){
#for(h in climate){
#	df<-data.frame(
 #      carbon=character(),climate=character(),station_id=character(),slope=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
#for(j in site){
 #     b<-data.one[data.one$station_id==j,c("station_id",i,h)]
  #   colnames(b)<-c("station_id","x","y")
   # b$x<-as.numeric(b$x)
   #b$y<-as.numeric(b$y)
  #b<-b[is.finite(b$x)&is.finite(b$y),]
#if(nrow(b)==0) next
#if(nrow(b) < 3) next
#	model<-lm(x~y,data=b)
#	c<-summary(model)$coefficients
#	d<-summary(model)
#if(nrow(c)<2|nrow(b)==0|nrow(b)<3) next
#	df<-rbind(df,data.frame(
#	carbon=i,climate=h,station_id=j,slope=c[2,1],p.value=c[2,4],r2=d$r.squared,r2.adj=d$adj.r.squared))}
#df$sig<-ifelse(df$p.value<0.05,"yes","no")
#carbon.sites[[k]]<-df
#colnames(carbon.sites[[k]])<-c(paste0("carbon.",i,sep=".",h),paste0("climate.",i,sep=".",h),"station_id",paste0("slope.",i,sep=".",h),
#		paste0("p.value.",i,sep=".",h),paste0("r2.",i,sep=".",h),paste0("r2.adj.",i,sep=".",h),paste0("sig.",i,sep=".",h))
#k<-k+1
#}}

#nutrient<-c("nitkjd","phetol","phetb1")

#nutrient.sites<-list()

#k<-1

#for(i in nutrient){
#for(h in climate){
 #       df<-data.frame(
  #      nutrient=character(),climate=character(),station_id=character(),slope=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
#for(j in site){
 #      b<-data.one[data.one$station_id==j,c("station_id",i,h)]
  #     colnames(b)<-c("station_id","x","y")
   #    b$x<-as.numeric(b$x)
    #   b$y<-as.numeric(b$y)
     #  b<-b[is.finite(b$x)&is.finite(b$y),]
#if(nrow(b)==0) next
#if(nrow(b) < 3) next
#        model<-lm(x~y,data=b)
#        c<-summary(model)$coefficients
#        d<-summary(model)
#if(nrow(c)<2|nrow(b)==0|nrow(b)<3) next
 #       df<-rbind(df,data.frame(
  #      nutrient=i,climate=h,station_id=j,slope=c[2,1],p.value=c[2,4],r2=d$r.squared,r2.adj=d$adj.r.squared))}
#df$sig<-ifelse(df$p.value<0.05,"yes","no")
#nutrient.sites[[k]]<-df
#colnames(nutrient.sites[[k]])<-c(paste0("nutrient.",i,sep=".",h),paste0("climate.",i,sep=".",h),"station_id",paste0("slope.",i,sep=".",h),
#                paste0("p.value.",i,sep=".",h),paste0("r2.",i,sep=".",h),paste0("r2.adj.",i,sep=".",h),paste0("sig.",i,sep=".",h))
#k<-k+1
#}}

#texture<-c("clay","silt","sand","ecec")

#texture.sites<-list()

#k<-1

#for(i in texture){
#for(h in climate){
#        df<-data.frame(
 #       texture=character(),climate=character(),station_id=character(),slope=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
#for(j in site){
#       b<-data.one[data.one$station_id==j,c("station_id",i,h)]
#       colnames(b)<-c("station_id","x","y")
#       b$x<-as.numeric(b$x)
#       b$y<-as.numeric(b$y)
#       b<-b[is.finite(b$x)&is.finite(b$y),]
#if(nrow(b)==0) next
#if(nrow(b) < 3) next
#        model<-lm(x~y,data=b)
#        c<-summary(model)$coefficients
#        d<-summary(model)
#if(nrow(c)<2|nrow(b)==0|nrow(b)<3) next
#        df<-rbind(df,data.frame(
#        texture=i,climate=h,station_id=j,slope=c[2,1],p.value=c[2,4],r2=d$r.squared,r2.adj=d$adj.r.squared))}
#df$sig<-ifelse(df$p.value<0.05,"yes","no")
#texture.sites[[k]]<-df
#colnames(texture.sites[[k]])<-c(paste0("texture.",i,sep=".",h),paste0("climate.",i,sep=".",h),"station_id",paste0("slope.",i,sep=".",h),
#                paste0("p.value.",i,sep=".",h),paste0("r2.",i,sep=".",h),paste0("r2.adj.",i,sep=".",h),paste0("sig.",i,sep=".",h))
#k<-k+1
#}}

##for(g in 1:27){
#       data<-merge(data,water.sites[[g]],all.x=TRUE)
#       data<-merge(data,carbon.sites[[g]],all.x=TRUE)
#       data<-merge(data,nutrient.sites[[g]],all.x=TRUE)}

#for(v in 1:36){
#	data<-merge(data,texture.sites[[v]],all.x=TRUE)}

#data.1<-data %>% filter(station_id %in% site)

#write.csv(data.1,"/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

#data.1<-data

#data.1<-merge(data.1,map,all.x=TRUE)
#data.1<-merge(data.1,avg.tmax,all.x=TRUE)
#data.1<-merge(data.1,avg.tmin,all.x=TRUE)

#data.2<-merge(data.1,biomes,all.x=TRUE)

#slope<-c("slope.wg1500.n.prcp.xhigh","slope.wg1500.n.dry.days","slope.wg1500.n.prcp.event","slope.wg1500.n.tmax.xhigh",
#	"slope.wg1500.n.tmin.xhigh","slope.wg1500.n.tmax.xlow","slope.wg1500.n.tmin.xlow","slope.wg1500.n.tmax.avg","slope.wg1500.n.tmin.avg",
#	"slope.wg0200.n.prcp.xhigh","slope.wg0200.n.dry.days","slope.wg0200.n.prcp.event","slope.wg0200.n.tmax.xhigh",
 #      "slope.wg0200.n.tmin.xhigh","slope.wg0200.n.tmax.xlow","slope.wg0200.n.tmin.xlow","slope.wg0200.n.tmax.avg","slope.wg0200.n.tmin.avg",
#"slope.wg0033.n.prcp.xhigh","slope.wg0033.n.dry.days","slope.wg0033.n.prcp.event","slope.wg0033.n.tmax.xhigh",
 #      "slope.wg0033.n.tmin.xhigh","slope.wg0033.n.tmax.xlow","slope.wg0033.n.tmin.xlow","slope.wg0033.n.tmax.avg","slope.wg0033.n.tmin.avg")

#property<-c("map","total.avg.tmax","total.avg.tmin","clay","sand","silt","ecec","bdwsod","tceq","orgc","cfgr","cfvo")

#water.slope<-data.frame(
 #      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(rr in slope){
#for(ww in property){
#	b<-data.1[,c(rr,ww)]
#	colnames(b)<-c("x","y")
#if(nrow(b)<3) next
#	model<-lm(x~y,data=b)
#	sum<-summary(model)
#	coef<-summary(model)$coefficients
#if(nrow(coef)<2) next
#	water.slope<-rbind(water.slope,data.frame(slope=rr,property=ww,slope.2=coef[2,1],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}	

#water.slope$sig<-ifelse(water.slope$p.value<0.05,"yes","no")

