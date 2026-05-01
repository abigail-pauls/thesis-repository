library(multcompView)
library(lme4)
library(car)
library(multcomp)
library(tidyverse)
library(data.table)

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data$continent<-as.factor(data$continent)
data$country_name<-as.factor(data$country_name)
data$region<-as.factor(data$region)

map<-read.csv("/disks/home/abigail/thesis-repository/code/data/map.csv")
map<-map[,3:4]
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

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
        ifelse(upper_depth>=21&upper_depth<=40,"two",
        ifelse(upper_depth>=41&upper_depth<=60,"three",
        ifelse(upper_depth>=61&upper_depth<=80,"four",
        ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

data$soil.layer<-as.factor(data$soil.layer)

data.one<-subset(data,data$soil.layer=="one")

ecosystems<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.csv")

biome<-ecosystems[,c(1,4,6,7)]
colnames(biome)<-c("station_id","eco_name","biome_name","realm")

biome <- biome %>% dplyr::mutate(biome = NA_real_)

biome <- biome %>% dplyr::mutate(biome = NA_real_)

biome_map <- c(
  "Boreal Forests/Taiga" = "boreal",
  "Deserts & Xeric Shrublands" = "desert",
  "Mediterranean Forests, Woodlands & Scrub" = "mediterranean",
  "Montane Grasslands & Shrublands" = "montane.grassland",
  "Temperate Broadleaf & Mixed Forests" = "temperate.forest",
  "Temperate Conifer Forests" = "temperate.conifer",
  "Temperate Grasslands, Savannas & Shrublands" = "temperate.grassland",
  "Tropical & Subtropical Coniferous Forests" = "tropical.coniferous",
  "Tropical & Subtropical Dry Broadleaf Forests" = "tropical.dry",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "tropical.grassland",
  "Tropical & Subtropical Moist Broadleaf Forests" = "tropical.moist",
  "Tundra" = "tundra")

biome$biome <- biome_map[biome$biome_name]

data.one<-merge(data.one,biome,all.x=TRUE)

data.one<-data.one[,colSums(!is.na(data.one))> 0]

data.a<-with(data.one,data.one[biome=="desert"|biome=="mediterranean"|biome=="temperate.conifer"|biome=="temperate.forest"|
	biome=="tropical.grassland"|biome=="tropical.moist",])

var.contr<-function(model){
        raneff<-as.data.frame(VarCorr(model))
        ranvar<-raneff$vcov
        X<-model.matrix(model)
        beta<-fixef(model)
        fixvar<-var(as.vector(X%*%beta))
        total<-sum(ranvar)+fixvar
        rancontr<-ranvar/total
        fixcontr<-fixvar/total
        names(fixcontr)<-c("fixed")
        names(rancontr)<-c(raneff$grp)
        contr<-c(rancontr,fixcontr)
        varcontr<-data.frame(
                variance=c(ranvar,fixvar),proportion=contr,precent=contr*100)
        return(varcontr)}

nitkjd.models<-list()
nitkjd.tests<-list()
nitkjd.var<-list()

nitkjd.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","elco50","ecec",
        "bdwsod","bdfifm","cfgr","cfvo","wg1500")

for(k in nitkjd.property){
        df<-data.a[,c("station_id","biome","nitkjd",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","nitkjd",paste0(k))
       model<-lmer(nitkjd~df[,4]+(1|biome)+(1|station_id),df)
       nitkjd.models[[k]]<-summary(model)
       nitkjd.tests[[k]]<-Anova(model,type="II")
       nitkjd.var[[k]]<-var.contr(model)}

orgc.models<-list()
orgc.tests<-list()
orgc.var<-list()

orgc.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","totc","orgm","nitkjd","elco50","ecec","cecph7",
        "bdwsod","bdfifm","phetb1","phetm3","cfgr","cfvo","wg1500","wg0200","wg0033")

for(k in orgc.property){
        df<-data.a[,c("station_id","biome","orgc",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","orgc",paste0(k))
       model<-lmer(orgc~df[,4]+(1|biome)+(1|station_id),df)
       orgc.models[[k]]<-summary(model)
       orgc.tests[[k]]<-Anova(model,type="II")
       orgc.var[[k]]<-var.contr(model)}

