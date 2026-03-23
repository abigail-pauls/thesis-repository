#library(tidyverse)
#library(data.table)
#library(lme4)
#library(car)	

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data$continent<-as.factor(data$continent)
data$country_name<-as.factor(data$country_name)
data$region<-as.factor(data$region)
data$station_id<-as.factor(data$station_id)

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
       ifelse(upper_depth>=21&upper_depth<=40,"two",
      ifelse(upper_depth>=41&upper_depth<=60,"three",
     ifelse(upper_depth>=61&upper_depth<=80,"four",
    ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

data$soil.layer<-as.factor(data$soil.layer)

data.one<-subset(data,data$soil.layer=="one")

#texture<-c("clay","silt","sand","bdwsod","bdfi33","cfgr","cfvo","orgm")

#initial.texture<-data.frame(
#		property=character(),
#		Df=numeric(),
#		Sum.sqr=numeric(),
#		Mean.sqr=numeric(),
#		F.value=numeric(),
#		p.value=numeric())

#for(i in texture){
#	df<-data.one[,c("station_id",i)]
#	df$station_id<-as.factor(df$station_id)
#	model<-aov(df[,2]~df$station_id,df)
#	s<-summary(model)[[1]]
#	initial.texture<-rbind(initial.texture,data.frame(
#		property=i,
#		Df=s$Df[1],
#		Sum.sqr=s$`Sum Sq`[1],
#		Mean.sqr=s$`Mean Sq`[1],
#		F.value=s$`F value`[1],
#		p.value=s$`Pr(>F)`[1]))}

#initial.texture<-as.data.frame(initial.texture)

#initial.texture$significant<-ifelse(initial.texture$p.value<=0.05,"yes","no")

water<-c("wg1500","wg0200","wg0033")
climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

initial.moisture<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
	t.value=numeric(),p.value=numeric())

for(h in water){
for(j in climate){
	df<-data.one[,c("station_id",h,j)]
	colnames(df)<-c("station_id","x","y")
	df$x<-as.numeric(df$x)
	df$y<-as.numeric(df$y)
	df<-na.omit(df)
	df<-df[is.finite(df$x)&is.finite(df$y),]
if(var(df$y)==0) next
	model<-lmer(x~y+(1|station_id),data=df)
	b<-as.data.frame(summary(model)$coefficients)
	a<-as.data.frame(summary(model)$varcor)
	test<-Anova(model,type="II")
if(nrow(a)<2|var(df$y)==0){
	initial.moisture<-rbind(initial.moisture,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
std.err="NA",t.value="NA",p.value="NA"))}
else{initial.moisture<-rbind(initial.moisture,data.frame(water=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
	std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}}

initial.moisture$significant<-ifelse(initial.moisture$p.value<0.05,"yes","no")

water.200<-c("n.prcp.high","n.prcp.xhigh.length")

temp.200<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
        t.value=numeric(),p.value=numeric())

for(j in water.200){
        df<-data.one[,c("station_id","wg0200",j)]
        colnames(df)<-c("station_id","x","y")
        df$x<-as.numeric(df$x)
        df$y<-as.numeric(df$y)
        df<-na.omit(df)
        df<-df[is.finite(df$x)&is.finite(df$y),]
if(var(df$y)==0) next
        model<-lmer(x~y+(1|station_id),data=df)
        b<-as.data.frame(summary(model)$coefficients)
        a<-as.data.frame(summary(model)$varcor)
        test<-Anova(model,type="II")
if(nrow(a)<2|var(df$y)==0){
        temp.200<-rbind(temp.200,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
std.err="NA",t.value="NA",p.value="NA"))}
else{temp.200<-rbind(temp.200,data.frame(water=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
        std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

temp.200$significant<-ifelse(temp.200$p.value<0.05,"yes","no")

water.33<-c("n.prcp.high","n.prcp.xhigh.length","n.tmax.high","n.tmax.xhigh.length")

temp.33<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
        t.value=numeric(),p.value=numeric())

for(j in water.33){
        df<-data.one[,c("station_id","wg0033",j)]
        colnames(df)<-c("station_id","x","y")
        df$x<-as.numeric(df$x)
        df$y<-as.numeric(df$y)
        df<-na.omit(df)
        df<-df[is.finite(df$x)&is.finite(df$y),]
if(var(df$y)==0) next
        model<-lmer(x~y+(1|station_id),data=df)
        b<-as.data.frame(summary(model)$coefficients)
        a<-as.data.frame(summary(model)$varcor)
        test<-Anova(model,type="II")
if(nrow(a)<2|var(df$y)==0){
        temp.33<-rbind(temp.33,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
	std.err="NA",t.value="NA",p.value="NA"))}
else{temp.33<-rbind(temp.33,data.frame(water=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
        std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

temp.33$significant<-ifelse(temp.33$p.value<0.05,"yes","no")

initial.moisture<-rbind(initial.moisture,temp.200,temp.33)

# water      climate significant
#wg1500 n.tmin.xhigh         yes
#	n.tmin.high
#	n.tmin.xhigh.length
#water.1500<-c("n.tmin.high","n.tmin.xhigh.length","n.tmin.high.length")

#temp.1500<-data.frame(water=character(),climate=character(),slope=numeric(),std.err=numeric(),t.value=numeric(),
#        p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(j in water.1500){
 #       df<-data.one[,c("wg1500",j)]
  #      df<-na.omit(df)
   #     df <- df[is.finite(df[,1])&is.finite(df[,2]),]
    #    colnames(df)<-c("x","y")
#if(nrow(df)==0) next
#if(nrow(df)<3) next
 #       model<-lm(x~y,data=df)
  #      a<-summary(model)$coefficients
   #     b<-summary(model)
#if(nrow(a)<2){
 #       temp.1500<-rbind(temp.1500,data.frame(
  #              water="wg1500",
   #             climate=j,
    #            slope="NA",
     #           std.err="NA",
      #          t.value="NA",
       #         p.value="NA",
        #        r2="NA",
         #       r2.adj="NA"))}
#else{temp.1500<-rbind(temp.1500,data.frame(
 #               water="wg1500",
  #              climate=j,
   #             slope=a[2,1],
    #            std.err=a[2,2],
     #           t.value=a[2,3],
      #          p.value=a[2,4],
       #         r2=b$r.squared,
        #        r2.adj=b$adj.r.squared))}}
#temp.1500$significant<-ifelse(temp.1500$p.value<0.05,"yes","no")

#wg0200 n.prcp.xhigh         yes
#	n.prcp.high
#	n.prcp.xhigh.length
#wg0200 n.prcp.event         yes
#	n.prcp.avg
#	n.prcp.mean
#wg0200 n.tmax.xhigh         yes
#	n.tmax.high
#	n.tmax.xhigh.length
#wg0200 n.tmax.xlow         yes
#	n.tmax.low
#	n.tax.xlow.length	
#wg0200 n.tmin.xhigh         yes
#	n.tmin.high
#	n.tmin.xhigh.length
#wg0200 n.tmin.xlow         yes
#	n.tmin.low
#	n.tmin.xlow.length
#water.200<-c("n.prcp.high","n.prcp.xhigh.length","n.prcp.avg","n.prcp.mean","n.tmax.high","n.tmax.xhigh.length",
#	"n.tmax.low","n.tmax.xlow.length","n.tmin.high","n.tmin.xhigh.length","n.tmin.low","n.tmin.xlow.length")

#water.200<-c("n.prcp.high","n.prcp.avg","n.prcp.mean","n.tmax.high","n.tmax.xhigh.length",
 #       "n.tmax.low","n.tmax.xlow.length","n.tmin.high","n.tmin.xhigh.length","n.tmin.low","n.tmin.xlow.length",
#	"n.tmax.high.length","n.tmax.low.length","n.tmin.high.length","n.tmin.low.length")

#temp.200<-data.frame(water=character(),climate=character(),slope=numeric(),std.err=numeric(),t.value=numeric(),
 #       p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(j in water.200){
 #       df<-data.one[,c("wg0200",j)]
  #      df<-na.omit(df)
   #     df <- df[is.finite(df[,1])&is.finite(df[,2]),]
    #    colnames(df)<-c("x","y")
#if(nrow(df)==0) next
#if(nrow(df)<3) next
 #       model<-lm(x~y,data=df)
  #      a<-summary(model)$coefficients
   #     b<-summary(model)
#if(nrow(a)<2){
 #       temp.200<-rbind(temp.200,data.frame(
  #              water="wg0200",
   #             climate=j,
    #            slope="NA",
     #           std.err="NA",
      #          t.value="NA",
       #         p.value="NA",
        #        r2="NA",
         #       r2.adj="NA"))}
#else{temp.200<-rbind(temp.200,data.frame(
 #               water="wg0200",
  #              climate=j,
   #             slope=a[2,1],
    #            std.err=a[2,2],
     #           t.value=a[2,3],
      #          p.value=a[2,4],
       #         r2=b$r.squared,
        #        r2.adj=b$adj.r.squared))}}
#temp.200$significant<-ifelse(temp.200$p.value<0.05,"yes","no")

#wg0033 n.tmax.xhigh         yes
#	n.tmax.high
#	n.tmax.xhigh.length
#wg0033 n.tmin.xhigh         yes
#	n.tmin.high
#	n.tmin.xhigh.length 
#water.33<-c("n.tmax.high","n.tmax.xhigh.length","n.tmin.high","n.tmin.xhigh.length","n.tmax.high.length","n.tmin.high.length")

#temp.33<-data.frame(water=character(),climate=character(),slope=numeric(),std.err=numeric(),t.value=numeric(),
 #       p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(j in water.33){
 #       df<-data.one[,c("wg0033",j)]
  #      df<-na.omit(df)
   #     df <- df[is.finite(df[,1])&is.finite(df[,2]),]
    #    colnames(df)<-c("x","y")
#if(nrow(df)==0) next
#if(nrow(df)<3) next
 #       model<-lm(x~y,data=df)
  #      a<-summary(model)$coefficients
   #     b<-summary(model)
#if(nrow(a)<2){
 #       temp.33<-rbind(temp.33,data.frame(
  #              water="wg0033",
   #             climate=j,
    #            slope="NA",
     #           std.err="NA",
      #          t.value="NA",
       #         p.value="NA",
        #        r2="NA",
         #       r2.adj="NA"))}
#else{temp.33<-rbind(temp.33,data.frame(
 #               water="wg0033",
  #              climate=j,
   #             slope=a[2,1],
    #            std.err=a[2,2],
     #           t.value=a[2,3],
      #          p.value=a[2,4],
       #         r2=b$r.squared,
        #        r2.adj=b$adj.r.squared))}}
#temp.33$significant<-ifelse(temp.33$p.value<0.05,"yes","no")

#initial.moisture<-rbind(initial.moisture,temp.1500,temp.200,temp.33)

# water             climate significant
#28 wg1500         n.tmin.high         yes
#29 wg1500 n.tmin.xhigh.length         yes
#30 wg0200         n.prcp.high         yes
#31 wg0200          n.prcp.avg         yes
#33 wg0200         n.tmax.high         yes
#34 wg0200 n.tmax.xhigh.length         yes
#35 wg0200          n.tmax.low         yes
#37 wg0200         n.tmin.high         yes
#38 wg0200 n.tmin.xhigh.length         yes
#39 wg0200          n.tmin.low         yes
#40 wg0200  n.tmin.xlow.length         yes
#41 wg0033         n.tmax.high         yes
#43 wg0033         n.tmin.high         yes

#write.csv(initial.moisture,"/disks/home/abigail/thesis-repository/code/data/initial.moisture.csv")
