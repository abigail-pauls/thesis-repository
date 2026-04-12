library(tidyverse)
library(data.table)
library(lme4)
library(car)	

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
#
#initial.texture<-as.data.frame(initial.texture)

#initial.texture$significant<-ifelse(initial.texture$p.value<=0.05,"yes","no")

#write.csv("/disks/home/abigail/thesis-repository/code/data/texture.anova.csv")

#water<-c("wg1500","wg0200","wg0033")
#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#initial.moisture<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#	t.value=numeric(),p.value=numeric())

#for(h in water){
#for(j in climate){
#	df<-data.one[,c("station_id",h,j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-na.omit(df)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	initial.moisture<-rbind(initial.moisture,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
#std.err="NA",t.value="NA",p.value="NA"))}
#else{initial.moisture<-rbind(initial.moisture,data.frame(water=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#	std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}}

#initial.moisture$significant<-ifelse(initial.moisture$p.value<0.05,"yes","no")

#water.200<-c("n.prcp.high","n.prcp.xhigh.length")

#temp.200<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#	t.value=numeric(),p.value=numeric())

#for(j in water.200){
#	df<-data.one[,c("station_id","wg0200",j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-na.omit(df)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	temp.200<-rbind(temp.200,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
#std.err="NA",t.value="NA",p.value="NA"))}
#else{temp.200<-rbind(temp.200,data.frame(water="wg0200",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#	std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.200$significant<-ifelse(temp.200$p.value<0.05,"yes","no")

#water.33<-c("n.prcp.high","n.prcp.xhigh.length","n.tmax.high","n.tmax.xhigh.length","n.prcp.high","n.dry.length","n.prcp.high.length","n.tmax.high.length")

#temp.33<-data.frame(water=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
 #      t.value=numeric(),p.value=numeric())

#for(j in water.33){
#	df<-data.one[,c("station_id","wg0033",j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-na.omit(df)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	temp.33<-rbind(temp.33,data.frame(water=h,climate=j,varPair="NA",varResid="NA",slope="NA",
#	std.err="NA",t.value="NA",p.value="NA"))}
#else{temp.33<-rbind(temp.33,data.frame(water="wg0033",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#	std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.33$significant<-ifelse(temp.33$p.value<0.05,"yes","no")

#initial.moisture<-rbind(initial.moisture,temp.200,temp.33)

#write.csv(initial.moisture,"/disks/home/abigail/thesis-repository/code/data/initial.moisture.csv")

##CARBON
#carbon<-c("orgc","totc","tceq")
#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#carbon.initial<-data.frame(carbon=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
 #     t.value=numeric(),p.value=numeric())

#for(h in carbon){
#for(j in climate){
 #      df<-data.one[,c("station_id",h,j)]
  #     colnames(df)<-c("station_id","x","y")
   #    df$x<-as.numeric(df$x)
    #   df$y<-as.numeric(df$y)
     #  df<-na.omit(df)
      # df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#       model<-lmer(x~y+(1|station_id),data=df)
#       b<-as.data.frame(summary(model)$coefficients)
#       a<-as.data.frame(summary(model)$varcor)
#       test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#       carbon.initial<-rbind(carbon.initial,data.frame(carbon=h,climate=j,varPair="NA",varResid="NA",slope="NA",
#		std.err="NA",t.value="NA",p.value="NA"))}
#else{carbon.initial<-rbind(carbon.initial,data.frame(carbon=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#       std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}}

#carbon.initial$significant<-ifelse(carbon.initial$p.value<0.05,"yes","no")

#orgc.initial<-c("n.tmax.high","n.tmax.xhigh.length","n.tmin.high","n.tmin.xhigh.length","n.tmax.high.length","n.tmin.high.length")

#temp.orgc<-data.frame(carbon=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#	t.value=numeric(),p.value=numeric())

#for(j in orgc.initial){
#	df<-data.one[,c("station_id","orgc",j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-na.omit(df)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	temp.orgc<-rbind(temp.orgc,data.frame(carbon=h,climate="orgc",varPair="NA",varResid="NA",slope="NA",
#std.err="NA",t.value="NA",p.value="NA"))}
#else{temp.orgc<-rbind(temp.orgc,data.frame(carbon="orgc",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#	std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.orgc$significant<-ifelse(temp.orgc$p.value<0.05,"yes","no")

#totc.initial<-c("n.dry.length","n.tmax.high","n.tmax.xhigh.length","n.tmin.high","n.tmin.xhigh.length","n.tmin.low",
#	"n.tmin.xlow.length","n.tmax.high.length","n.tmin.high.length","n.tmin.low.length")

#temp.totc<-data.frame(carbon=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#       t.value=numeric(),p.value=numeric())

#for(j in totc.initial){
#        df<-data.one[,c("station_id","totc",j)]
#        colnames(df)<-c("station_id","x","y")
#        df$x<-as.numeric(df$x)
#        df$y<-as.numeric(df$y)
#        df<-na.omit(df)
#        df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#        model<-lmer(x~y+(1|station_id),data=df)
#        b<-as.data.frame(summary(model)$coefficients)
#        a<-as.data.frame(summary(model)$varcor)
#        test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#        temp.totc<-rbind(temp.totc,data.frame(
#	carbon="totc",
#	climate=j,
#	varPair="NA",
#	varResid="NA",
#	slope="NA",
#	std.err="NA",
#	t.value="NA",
#	p.value="NA"))}
#else{temp.totc<-rbind(temp.totc,data.frame(carbon="totc",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
 #       std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.totc$significant<-ifelse(temp.totc$p.value<0.05,"yes","no")

#carbon.initial<-rbind(carbon.initial,temp.orgc,temp.totc)

#write.csv(carbon.initial,"/disks/home/abigail/thesis-repository/code/data/carbon.initial.csv")

#OTHER NUTRIENTS
#other<-c("nitkjd","phetol","phetm3","phetb1")
#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#nutrient.initial<-data.frame(carbon=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#       t.value=numeric(),p.value=numeric())

#for(h in other){
#for(j in climate){
#      df<-data.one[,c("station_id",h,j)]
#     colnames(df)<-c("station_id","x","y")
#    df$x<-as.numeric(df$x)
#   df$y<-as.numeric(df$y)
#  df<-na.omit(df)
# df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#      model<-lmer(x~y+(1|station_id),data=df)
#     b<-as.data.frame(summary(model)$coefficients)
#    a<-as.data.frame(summary(model)$varcor)
#   test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#      nutrient.initial<-rbind(nutrient.initial,data.frame(nutrient=h,climate=j,varPair="NA",varResid="NA",slope="NA",
#             std.err="NA",t.value="NA",p.value="NA"))}
#else{nutrient.initial<-rbind(nutrient.initial,data.frame(nutrient=h,climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#      std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}}

#nutrient.initial$significant<-ifelse(nutrient.initial$p.value<0.05,"yes","no")

#nitkjd.initial<-c("n.dry.length","n.prcp.avg","n.prcp.mean","n.tmin.low","n.tmin.xlow.length","n.tmin.low.length")

#temp.nitkjd<-data.frame(nutrient=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#      t.value=numeric(),p.value=numeric())

#for(j in nitkjd.initial){
#	df<-data.one[,c("station_id","nitkjd",j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-na.omit(df)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	temp.nitkjd<-rbind(temp.nitkjd,data.frame(
#	nutrient="nitkjd",
#	climate=j,
#	varPair="NA",
#	varResid="NA",
#	slope="NA",
#	std.err="NA",
#	t.value="NA",
#	p.value="NA"))}
#else{temp.nitkjd<-rbind(temp.nitkjd,data.frame(nutrient="nitkjd",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#     std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.nitkjd$significant<-ifelse(temp.nitkjd$p.value<0.05,"yes","no")

#phetol.initial<-c("n.tmax.high","n.tmax.xhigh.length","n.tmin.high","n.tmin.xhigh.length")

#temp.phetol<-data.frame(nutrient=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#     t.value=numeric(),p.value=numeric())

#for(j in phetol.initial){
#      df<-data.one[,c("station_id","phetol",j)]
#      colnames(df)<-c("station_id","x","y")
#     df$x<-as.numeric(df$x)
#   df$y<-as.numeric(df$y)
 #  df<-na.omit(df)
 # df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
 #      model<-lmer(x~y+(1|station_id),data=df)
#      b<-as.data.frame(summary(model)$coefficients)
#     a<-as.data.frame(summary(model)$varcor)
#    test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#       temp.phetol<-rbind(temp.phetol,data.frame(
#      nutrient="phetol",
#     climate=j,
#    varPair="NA",
#   varResid="NA",
#  slope="NA",
# std.err="NA",
#t.value="NA",
#p.value="NA"))}
#else{temp.phetol<-rbind(temp.phetol,data.frame(nutrient="phetol",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#      std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.phetol$significant<-ifelse(temp.phetol$p.value<0.05,"yes","no")

#phetb1.initial<-c("n.tmax.high","n.tmax.xhigh.length","n.tmax.low","n.tmax.xlow.length","n.tmin.high","n.tmin.xhigh.length",
#	"n.tmax.high.length","n.tmax.low.length","n.tmin.low.length")

#temp.phetb1<-data.frame(nutrient=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#      t.value=numeric(),p.value=numeric())

#for(j in phetb1.initial){
#       df<-data.one[,c("station_id","phetb1",j)]
#      colnames(df)<-c("station_id","x","y")
#     df$x<-as.numeric(df$x)
#    df$y<-as.numeric(df$y)
#   df<-na.omit(df)
# df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#       model<-lmer(x~y+(1|station_id),data=df)
#      b<-as.data.frame(summary(model)$coefficients)
#    a<-as.data.frame(summary(model)$varcor)
#    test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#      temp.phetol<-rbind(temp.phetb1,data.frame(
#      nutrient="phetb1",
#     climate=j,
#    varPair="NA",
#   varResid="NA",
#  slope="NA",
#std.err="NA",
# t.value="NA",
#p.value="NA"))}
#else{temp.phetb1<-rbind(temp.phetb1,data.frame(nutrient="phetb1",climate=j,varPair=a[1,"vcov"],varResid=a[2,"vcov"],slope=b[2,1],
#      std.err=b[2,2],t.value=b[2,3],p.value=test[1,3]))}}

#temp.phetb1$significant<-ifelse(temp.phetb1$p.value<0.05,"yes","no")

#nutrient.initial<-rbind(nutrient.initial,temp.nitkjd,temp.phetol,temp.phetb1)

#write.csv(nutrient.initial,"/disks/home/abigail/thesis-repository/code/data/nutrient.initis.csv")

#TEXTURE
#texture<-c("clay","silt","sand","ecec")

#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#texture.initial<-data.frame(texture=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
#       t.value=numeric(),p.value=numeric())

#for(h in texture){
#for(j in climate){

#	df<-data.one[,c("station_id",h,j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-df[is.finite(df$x)&is.finite(df$y),]

#if(var(df$y)==0) next

#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")

#if(nrow(a)<2|var(df$y)==0){

#	texture.initial<-rbind(texture.initial,data.frame(
#		texture=h,
#		climate=j,
#		varPair=NA,
#		varResid=NA,
#		slope=NA,
#		std.err=NA,
#		t.value=NA,
#		p.value=NA))}

#else{

#	texture.initial<-rbind(texture.initial,data.frame(
#		texture=h,
#		climate=j,
#		varPair=a[1,"vcov"],
#		varResid=a[2,"vcov"],
#		slope=b[2,1],
#		std.err=b[2,2],
#		t.value=b[2,3],
#		p.value=test[1,3]))}}}

#texture.initial$significant<-ifelse(texture.initial$p.value<0.05,"yes","no")

#clay<-c("n.tmax.low","n.tmax.xlow.length","n.tmin.high","n.tmin.xhigh.length","n.tmax.low.length","n.tmin.high.length")

#temp.clay<-data.frame(texture=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),st.err=numeric(),
#      t.value=numeric(),p.value=numeric())

#for(j in clay){
#	df<-data.one[,c("station_id","clay",j)]
#	colnames(df)<-c("station_id","x","y")
#	df$x<-as.numeric(df$x)
#	df$y<-as.numeric(df$y)
#	df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#	model<-lmer(x~y+(1|station_id),data=df)
#	b<-as.data.frame(summary(model)$coefficients)
#	a<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#	temp.clay<-rbind(temp.clay,data.frame(
#	texture="clay",
#	climate=j,
#	varPair="NA",
#	varResid="NA",
#	slope="NA",
#	std.err="NA",
#	t.value="NA",
#	p.value="NA"))}
#else{
#	temp.clay<-rbind(temp.clay,data.frame(
#	texture="clay",
#	climate=j,
#	varPair=a[1,"vcov"],
#	varResid=a[2,"vcov"],
#	slope=b[2,1],
#	std.err=b[2,2],
#	t.value=b[2,3],
#	p.value=test[1,3]))}}

#temp.clay$significant<-ifelse(temp.clay$p.value<0.05,"yes","no")

#silt<-c("n.dry.length","n.prcp.avg","n.prcp.mean","n.tmax.high","n.tmax.xhigh.length",
#	"n.tmax.low","n.tmax.xlow.length","n.tmin.high","n.tmin.xhigh.length","n.prcp.xhigh.length",
#	"n.tmax.low.length","n.tmin.high.length","n.prcp.high.length")

#temp.silt<-data.frame(texture=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),st.err=numeric(),
#      t.value=numeric(),p.value=numeric())

#for(j in silt){
#        df<-data.one[,c("station_id","silt",j)]
#        colnames(df)<-c("station_id","x","y")
#        df$x<-as.numeric(df$x)
#        df$y<-as.numeric(df$y)
#        df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#        model<-lmer(x~y+(1|station_id),data=df)
#        b<-as.data.frame(summary(model)$coefficients)
#        a<-as.data.frame(summary(model)$varcor)
#        test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#        temp.silt<-rbind(temp.silt,data.frame(
#        texture="silt",
#        climate=j,
#        varPair="NA",
#        varResid="NA",
#        slope="NA",
#        std.err="NA",
#        t.value="NA",
#        p.value="NA"))}
#else{
#        temp.silt<-rbind(temp.silt,data.frame(
#        texture="silt",
#        climate=j,
#        varPair=a[1,"vcov"],
#        varResid=a[2,"vcov"],
#	slope=b[2,1],
 #       std.err=b[2,2],
  #      t.value=b[2,3],
   #     p.value=test[1,3]))}}

#temp.silt$significant<-ifelse(temp.silt$p.value<0.05,"yes","no")

#sand<-c("n.tmax.high","n.tmax.xhigh.length","n.tmin.low","n.tmin.xlow.length")

#temp.sand<-data.frame(texture=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),st.err=numeric(),
#      t.value=numeric(),p.value=numeric())

#for(j in sand){
#        df<-data.one[,c("station_id","sand",j)]
#        colnames(df)<-c("station_id","x","y")
#        df$x<-as.numeric(df$x)
#        df$y<-as.numeric(df$y)
#        df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
#        model<-lmer(x~y+(1|station_id),data=df)
#        b<-as.data.frame(summary(model)$coefficients)
#        a<-as.data.frame(summary(model)$varcor)
#        test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
#        temp.sand<-rbind(temp.sand,data.frame(
#        texture="sand",
#        climate=j,
#        varPair="NA",
#        varResid="NA",
#        slope="NA",
##        std.err="NA",
#        t.value="NA",
#        p.value="NA"))}
#else{
#        temp.sand<-rbind(temp.sand,data.frame(
#        texture="sand",
#        climate=j,
#        varPair=a[1,"vcov"],
#        varResid=a[2,"vcov"],
#        slope=b[2,1],
#	std.err=b[2,2],
#        t.value=b[2,3],
#        p.value=test[1,3]))}}

#temp.sand$significant<-ifelse(temp.sand$p.value<0.05,"yes","no")

#ecec<-c("n.prcp.high","n.prcp.xhigh.length","n.prcp.high.length")

#temp.ecec<-data.frame(texture=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),st.err=numeric(),
 #     t.value=numeric(),p.value=numeric())

#for(j in ecec){
 #       df<-data.one[,c("station_id","ecec",j)]
  #      colnames(df)<-c("station_id","x","y")
   #     df$x<-as.numeric(df$x)
    #    df$y<-as.numeric(df$y)
     #   df<-df[is.finite(df$x)&is.finite(df$y),]
#if(var(df$y)==0) next
 #       model<-lmer(x~y+(1|station_id),data=df)
  #      b<-as.data.frame(summary(model)$coefficients)
   #     a<-as.data.frame(summary(model)$varcor)
    #    test<-Anova(model,type="II")
#if(nrow(a)<2|var(df$y)==0){
 #       temp.ecec<-rbind(temp.ecec,data.frame(
  #      texture="ecec",
   #     climate=j,
    #    varPair="NA",
     #   varResid="NA",
      #  slope="NA",
       # std.err="NA",
       # t.value="NA",
       # p.value="NA"))}
#else{
 #       temp.ecec<-rbind(temp.ecec,data.frame(
  #      texture="ecec",
   #     climate=j,
    #    varPair=a[1,"vcov"],
     #   varResid=a[2,"vcov"],
      #  slope=b[2,1],
#	std.err=b[2,2],
 #       t.value=b[2,3],
  #      p.value=test[1,3]))}}

#temp.ecec$significant<-ifelse(temp.ecec$p.value<0.05,"yes","no")

#texture.initial<-rbind(texture.initial,temp.clay, temp.silt, temp.sand, temp.ecec)
