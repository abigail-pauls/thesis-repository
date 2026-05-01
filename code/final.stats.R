library(tidyverse)
library(data.table)
library(emmeans)
library(lme4)

se <- function(x){
        x<-na.omit(x)
        sd(x)/sqrt(length(x))}

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.20260426.csv")

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
        ifelse(upper_depth>=21&upper_depth<=40,"two",
        ifelse(upper_depth>=41&upper_depth<=60,"three",
        ifelse(upper_depth>=61&upper_depth<=80,"four",
        ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

data$soil.layer<-as.factor(data$soil.layer)

data<-subset(data,data$soil.layer=="one")

map<-read.csv("/disks/home/abigail/thesis-repository/code/data/map.csv")
map<-map[,2:3]
names(map)<-c("station_id","map")

total.tmax.avg<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmax.csv")
total.tmax.avg<-total.tmax.avg[,2:3]
names(total.tmax.avg)<-c("station_id","total.avg.tmax")

total.tmin.avg<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmin.csv")
total.tmin.avg<-total.tmin.avg[,2:3]
names(total.tmin.avg)<-c("station_id","total.avg.tmin")

data<-merge(data,map,all.x=TRUE)
data<-merge(data,total.tmax.avg,all.x=TRUE)
data<-merge(data,total.tmin.avg,all.x=TRUE)

data<-data[data$biome!="tundra"&data$biome!="montane.grassland"&data$biome!="tropical.dry"&data$biome!="boreal"&data$biome!="tropical.coniferous",]
data$biome<-as.factor(data$biome)
data$biome<-droplevels(data$biome)

nitkjd.bio<-aov(nitkjd~biome,data)
orgc.bio<-aov(orgc~biome,data)

nitkjd.bio.t<-TukeyHSD(nitkjd.bio)
orgc.bio.t<-TukeyHSD(orgc.bio)

nitkjd.bio.l<-multcompLetters4(nitkjd.bio,nitkjd.bio.t)
orgc.bio.l<-multcompLetters4(orgc.bio,orgc.bio.t)

data$biome<-as.factor(data$biome)

nitkjd.mean<-aggregate(nitkjd~biome,data=data,mean,na.rm=TRUE)
orgc.mean<-aggregate(orgc~biome,data=data,mean,na.rm=TRUE)

nitkjd.se<-aggregate(nitkjd~biome,data=data,se)
orgc.se<-aggregate(orgc~biome,data=data,se)

soil<-c("sand","silt","clay","ecec","elco50","cfgr","bdwsod","bdfifm",
	"tceq","wg1500","phca")

means<-list()
std.err<-list()

for(s in soil){
	df<-data[,c(s,"biome")]
	colnames(df)<-c("soil","biome")
	df<-na.omit(df)
	df$biome<-droplevels(df$biome)
	means[[s]]<-aggregate(soil~biome,data=df,mean,na.rm=TRUE)
	std.err[[s]]<-aggregate(soil~biome,data=df,se)}

data<-data[data$biome!="boreal"&data$biome!="montane.grassland"&data$biome!="tropical.conifer"&data$biome!="tundra"&data$biome!="tropical.dry",]


climate<-c("n.prcp.xhigh","n.prcp.event","n.dry.days","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

n1.model<-list()
n1.test<-list()
o1.model<-list()
o1.test<-list()

for(i in climate){
        df<-data[,c("nitkjd",i,"station_id","biome")]
        colnames(df)<-c("nitkjd","climate","station_id","biome")
        df<-na.omit(df)
        df<-df[is.finite(df$nitkjd)&is.finite(df$climate),]
	df$climate<-scale(df$climate,center=TRUE,scale=TRUE)
        df$biome<-as.factor(df$biome)
        df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
        n1.model[[i]]<-lmer(nitkjd~climate+(1|biome)+(1|station_id),df)
        n1.test[[i]]<-Anova(n1.model[[i]])}

for(i in climate){
        df<-data[,c("orgc",i,"station_id","biome")]
        colnames(df)<-c("orgc","climate","station_id","biome")
        df<-na.omit(df)
        df<-df[is.finite(df$orgc)&is.finite(df$climate),]
        df$biome<-as.factor(df$biome)
        df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
        o1.model[[i]]<-lmer(orgc~climate+(1|biome)+(1|station_id),df)
        o1.test[[i]]<-Anova(o1.model[[i]])}


n2.model<-list()
n2.test<-list()
n2.emtrends<-list()
o2.model<-list()
o2.test<-list()
o2.emtrends<-list()

for(i in climate){
        df<-data[,c("nitkjd",i,"station_id","biome")]
        colnames(df)<-c("nitkjd","climate","station_id","biome")
        df<-na.omit(df)
        df<-df[is.finite(df$nitkjd)&is.finite(df$climate),]
	df$climate<-scale(df$climate,center=TRUE,scale=TRUE)
        df$biome<-as.factor(df$biome)
        df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
        n2.model[[i]]<-lmer(nitkjd~climate*biome+(1|station_id),df)
        n2.test[[i]]<-Anova(n2.model[[i]])
	n2.emtrends[[i]]<-emtrends(n2.model[[i]],~biome,var="climate")
}

for(i in climate){
        df<-data[,c("orgc",i,"station_id","biome")]
        colnames(df)<-c("orgc","climate","station_id","biome")
        df<-na.omit(df)
        df<-df[is.finite(df$orgc)&is.finite(df$climate),]
        df$biome<-as.factor(df$biome)
        df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
        o2.model[[i]]<-lmer(orgc~climate*biome+(1|station_id),df)
        o2.test[[i]]<-Anova(o2.model[[i]])
	o2.emtrends[[i]]<-emtrends(o2.model[[i]],~biome,var="climate")
}


nitkjd.lmer.model<-lmer(nitkjd~n.prcp.event+n.dry.days+n.prcp.xhigh+biome+
                        n.prcp.event:biome+n.dry.days:biome+n.prcp.xhigh:biome
                        +(1|station_id),subset(data,data$biome!="tundra"&data$biome!="montane.grassland"&data$biome!="tropical.coniferous"))

orgc.lmer.model<-lmer(orgc~n.prcp.event+n.dry.days+n.prcp.xhigh+biome+
                        n.prcp.event:biome+n.dry.days:biome+n.prcp.xhigh:biome
                        +(1|station_id),subset(data,data$biome!="tundra"&data$biome!="montane.grassland"&data$biome!="tropical.coniferous"))

data<-data[data$biome!="tundra"&data$biome!="montane.grassland"&data$biome!="tropical.coniferous"&data$biome!="boreal"&data$biome!="tropical.dry",]
data$biome<-as.factor(data$biome)
data$biome<-droplevels(data$biome)

climate<-c("n.prcp.xhigh","n.prcp.event","n.dry.days","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")
soil<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","cfgr","bdwsod","tceq","ecec","elco50")

nitkjd.models<-list()
orgc.models<-list()

for(c in climate){
for(s in soil){
        df<-data[,c("nitkjd",c,s,"biome")]
        colnames(df)<-c("nitkjd","climate","soil","biome")
        df<-na.omit(df)
df<-df[is.finite(df$nitkjd)&is.finite(df$climate)&is.finite(df$soil),]
       df$biome<-as.factor(df$biome)
df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
        model<-aov(nitkjd~soil*climate*biome,data=df)
        name<-paste0(c,sep=".",s)
nitkjd.models[[name]]<-summary(model)[[1]]}}

for(c in climate){
for(s in soil){
        df<-data[,c("orgc",c,s,"biome")]
        colnames(df)<-c("orgc","climate","soil","biome")
        df<-na.omit(df)
        df<-df[is.finite(df$nitkjd)&is.finite(df$climate)&is.finite(df$soil),]
        df$biome<-as.factor(df$biome)
df$biome<-droplevels(df$biome)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
        model<-aov(orgc~soil*climate*biome,data=df)
        name<-paste0(c,sep=".",s)
orgc.models[[name]]<-summary(model)[[1]]}}
