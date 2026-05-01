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

sand.models<-list()
sand.tests<-list()
sand.var<-list()

sand.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in sand.property){
	df<-data.a[,c("station_id","biome","sand",k)]
	colnames(df)<-c("station_id","biome","sand",paste0(k))
	model<-lmer(sand~df[,4]+(1|biome)+(1|station_id),df)
	sand.models[[k]]<-summary(model)
	sand.tests[[k]]<-Anova(model,type="II")
	sand.var[[k]]<-var.contr(model)}

silt.models<-list()
silt.tests<-list()
silt.var<-list()

silt.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in silt.property){
        df<-data.a[,c("station_id","biome","silt",k)]
        colnames(df)<-c("station_id","biome","silt",paste0(k))
        model<-lmer(silt~df[,4]+(1|biome)+(1|station_id),df)
        silt.models[[k]]<-summary(model)
        silt.tests[[k]]<-Anova(model,type="II")
        silt.var[[k]]<-var.contr(model)}

clay.models<-list()
clay.tests<-list()
clay.var<-list()

clay.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in clay.property){
        df<-data.a[,c("station_id","biome","clay",k)]
        colnames(df)<-c("station_id","biome","clay",paste0(k))
        model<-lmer(clay~df[,4]+(1|biome)+(1|station_id),df)
        clay.models[[k]]<-summary(model)
        clay.tests[[k]]<-Anova(model,type="II")
        clay.var[[k]]<-var.contr(model)}

tceq.models<-list()
tceq.tests<-list()
tceq.var<-list()

tceq.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in tceq.property){
        df<-data.a[,c("station_id","biome","tceq",k)]
        colnames(df)<-c("station_id","biome","tceq",paste0(k))
        model<-lmer(tceq~df[,4]+(1|biome)+(1|station_id),df)
       tceq.models[[k]]<-summary(model)
      tceq.tests[[k]]<-Anova(model,type="II")
     tceq.var[[k]]<-var.contr(model)}

orgm.models<-list()
orgm.tests<-list()
orgm.var<-list()

orgm.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in orgm.property){
        df<-data.a[,c("station_id","biome","orgm",k)]
	df<-na.omit(df)
if(nrow(df)<3) next
        colnames(df)<-c("station_id","biome","orgm",paste0(k))
        model<-lmer(orgm~df[,4]+(1|biome)+(1|station_id),df)
        orgm.models[[k]]<-summary(model)
        orgm.tests[[k]]<-Anova(model,type="II")
        orgm.var[[k]]<-var.contr(model)}

cfgr.models<-list()
cfgr.tests<-list()
cfgr.var<-list()

cfgr.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in cfgr.property){
        df1<-data.a[,c("station_id","biome","cfgr",k)]
	df1<-na.omit(df1)
if(nrow(df1)<3) next
        colnames(df1)<-c("station_id","biome","cfgr","prop")
        model<-lmer(cfgr~prop+(1|biome)+(1|station_id),df1)
        cfgr.models[[k]]<-summary(model)
        cfgr.tests[[k]]<-Anova(model,type="II")
        cfgr.var[[k]]<-var.contr(model)}

cfvo.models<-list()
cfvo.tests<-list()
cfvo.var<-list()

cfvo.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in cfvo.property){
        df<-data.a[,c("station_id","biome","cfvo",k)]
        colnames(df)<-c("station_id","biome","cfvo",paste0(k))
       model<-lmer(cfvo~df[,4]+(1|biome)+(1|station_id),df)
      cfvo.models[[k]]<-summary(model)
     cfvo.tests[[k]]<-Anova(model,type="II")
    cfvo.var[[k]]<-var.contr(model)}

phca.models<-list()
phca.tests<-list()
phca.var<-list()

phca.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in phca.property){
        df<-data.a[,c("station_id","biome","phca",k)]
        colnames(df)<-c("station_id","biome","phca",paste0(k))
       model<-lmer(phca~df[,4]+(1|biome)+(1|station_id),df)
      phca.models[[k]]<-summary(model)
     phca.tests[[k]]<-Anova(model,type="II")
    phca.var[[k]]<-var.contr(model)}

bdwsod.models<-list()
bdwsod.tests<-list()
bdwsod.var<-list()

bdwsod.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in bdwsod.property){
        df<-data.a[,c("station_id","biome","bdwsod",k)]
        colnames(df)<-c("station_id","biome","bdwsod",paste0(k))
       model<-lmer(bdwsod~df[,4]+(1|biome)+(1|station_id),df)
      bdwsod.models[[k]]<-summary(model)
     bdwsod.tests[[k]]<-Anova(model,type="II")
    bdwsod.var[[k]]<-var.contr(model)}

bdfifm.models<-list()
bdfifm.tests<-list()
bdfifm.var<-list()

bdfifm.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in bdfifm.property){
        df<-data.a[,c("station_id","biome","bdfifm",k)]
        colnames(df)<-c("station_id","biome","bdfifm",paste0(k))
       model<-lmer(bdfifm~df[,4]+(1|biome)+(1|station_id),df)
      bdfifm.models[[k]]<-summary(model)
     bdfifm.tests[[k]]<-Anova(model,type="II")
    bdfifm.var[[k]]<-var.contr(model)}

elco50.models<-list()
elco50.tests<-list()
elco50.var<-list()

elco50.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in elco50.property){
        df<-data.a[,c("station_id","biome","elco50",k)]
        colnames(df)<-c("station_id","biome","elco50",paste0(k))
       model<-lmer(elco50~df[,4]+(1|biome)+(1|station_id),df)
      elco50.models[[k]]<-summary(model)
     elco50.tests[[k]]<-Anova(model,type="II")
    elco50.var[[k]]<-var.contr(model)}

ecec.models<-list()
ecec.tests<-list()
ecec.var<-list()

ecec.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in ecec.property){
        df<-data.a[,c("station_id","biome","ecec",k)]
        colnames(df)<-c("station_id","biome","ecec",paste0(k))
       model<-lmer(ecec~df[,4]+(1|biome)+(1|station_id),df)
      ecec.models[[k]]<-summary(model)
     ecec.tests[[k]]<-Anova(model,type="II")
    ecec.var[[k]]<-var.contr(model)}


g1500.models<-list()
wg1500.tests<-list()
wg1500.var<-list()

wg1500.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

for(k in wg1500.property){
        df<-data.one[,c("station_id","biome","wg1500",k)]
	df<-na.omit(df)
if(nrow(df)<3) next
        colnames(df)<-c("station_id","biome","wg1500",paste0(k))
        model<-lmer(wg1500~df[,4]+(1|biome)+(1|station_id),df)
	wg1500.models[[k]]<-summary(model)
	wg1500.tests[[k]]<-Anova(model,type="II")
	wg1500.var[[k]]<-var.contr(model)}

wg1500.model<-lmer(wg1500~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
wg1500.test<-Anova(wg1500.model,type="II")
wg1500.var<-var.contr(wg1500.model)

wg0200.models<-list()
wg0200.tests<-list()
wg0200.var<-list()

wg0200.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

for(k in wg0200.property){
        df<-data.one[,c("station_id","biome","wg0200",k)]
	df<-na.omit(df)
if(nrow(df)<3) next
	colnames(df)<-c("station_id","biome","wg0200",paste0(k))
	model<-lmer(wg0200~df[,4]+(1|biome)+(1|station_id),df)
	wg0200.models[[k]]<-summary(model)
	wg0200.tests[[k]]<-Anova(model,type="II")
	wg0200.var[[k]]<-var.contr(model)}

wg0200.model<-lmer(wg0200~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
wg0200.test<-Anova(wg0200.model,type="II")
wg0200.var<-var.contr(wg0200.model)

wg0033.models<-list()
wg0033.tests<-list()
wg0033.var<-list()

wg0033.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

for(k in wg0033.property){
        df<-data.one[,c("station_id","biome","wg0033",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
	colnames(df)<-c("station_id","biome","wg0033",paste0(k))     
	model<-lmer(wg0033~df[,4]+(1|biome)+(1|station_id),df)
	wg0033.models[[k]]<-summary(model)
	wg0033.tests[[k]]<-Anova(model,type="II")
	wg0033.var[[k]]<-var.contr(model)}

wg0033.model<-lmer(wg0033~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
wg0033.test<-Anova(wg0033.model,type="II")
wg0033.var<-var.contr(wg0033.model)


nitkjd.models<-list()
nitkjd.tests<-list()
nitkjd.var<-list()

nitkjd.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt",
	"clay","phca","tceq","orgm","elco50","ecec","bdwsod","bdfifm","cfgr",
	"wg1500")

for(k in nitkjd.property){
        df<-data.one[,c("station_id","biome","nitkjd",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","nitkjd",paste0(k))
       model<-lmer(nitkjd~df[,4]+(1|biome)+(1|station_id),df)
       nitkjd.models[[k]]<-summary(model)
       nitkjd.tests[[k]]<-Anova(model,type="II")
       nitkjd.var[[k]]<-var.contr(model)}

nitkjd.model<-lmer(nitkjd~map*total.avg.tmax*silt*phca*elco50*cecph7*bdwsod*wg1500+(1|station_id)+(1|biome),data.one)
nitkjd.test<-Anova(nitkjd.model,type="II")
nitkjd.var<-var.contr(nitkjd.model)

orgc.models<-list()
orgc.tests<-list()
orgc.var<-list()

orgc.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay",
	"phca","tceq","orgm","elco50","ecec","bdwsod","bdfifm","cfgr",
	"wg1500")

for(k in orgc.property){
        df<-data.one[,c("station_id","biome","orgc",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","orgc",paste0(k))
       model<-lmer(orgc~df[,4]+(1|biome)+(1|station_id),df)
       orgc.models[[k]]<-summary(model)
       orgc.tests[[k]]<-Anova(model,type="II")
       orgc.var[[k]]<-var.contr(model)}

orgc.model<-lmer(orgc~total.avg.tmax*silt*phca*elco50*cecph7*bdwsod*bdfifm*wg1500+(1|station_id)+(1|biome),data.one)
orgc.test<-Anova(orgc.model,type="II")
orgc.var<-var.contr(orgc.model)

totc.models<-list()
totc.tests<-list()
totc.var<-list()

totc.property<-c("map","total.avg.tmax","total.avg.tmin","wg1500")

for(k in totc.property){
        df<-data.one[,c("station_id","biome","totc",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","totc",paste0(k))
       model<-lmer(totc~df[,4]+(1|biome)+(1|station_id),df)
       totc.models[[k]]<-summary(model)
       totc.tests[[k]]<-Anova(model,type="II")
       totc.var[[k]]<-var.contr(model)}

totc.model<-lmer(totc~map*total.avg.tmax*sand*silt*phca*cecph7*bdwsod*wg1500+(1|station_id)+(1|biome),data.one)
totc.test<-Anova(totc.model,type="II")
totc.var<-var.contr(totc.model)

phetb1.models<-list()
phetb1.tests<-list()
phetb1.var<-list()

phetb1.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","totc","orgc","orgm","nitkjd","elco50","ecec","cecph7",
        "bdwsod","bdfifm","cfgr","cfvo","wg1500","wg0200","wg0033")

for(k in phetb1.property){
        df<-data.one[,c("station_id","biome","phetb1",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","phetb1",paste0(k))
       model<-lmer(phetb1~df[,4]+(1|biome)+(1|station_id),df)
       phetb1.models[[k]]<-summary(model)
       phetb1.tests[[k]]<-Anova(model,type="II")
       phetb1.var[[k]]<-var.contr(model)}

phetb1.model<-lmer(phetb1~sand*silt*clay*cecph7*bdwsod*wg1500+(1|station_id)+(1|biome),data.one)
phetb1.test<-Anova(phetb1.model,type="II")
phetb1.var<-var.contr(phetb1.model)

phetm3.models<-list()
phetm3.tests<-list()
phetm3.var<-list()

phetm3.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","totc","orgc","orgm","nitkjd","elco50","ecec","cecph7",
        "bdwsod","bdfifm","cfgr","cfvo","wg1500","wg0200","wg0033")

for(k in phetm3.property){
        df<-data.one[,c("station_id","biome","phetm3",k)]
        df<-na.omit(df)
if(nrow(df)<3) next
       colnames(df)<-c("station_id","biome","phetm3",paste0(k))
       model<-lmer(phetm3~df[,4]+(1|biome)+(1|station_id),df)
       phetm3.models[[k]]<-summary(model)
       phetm3.tests[[k]]<-Anova(model,type="II")
       phetm3.var[[k]]<-var.contr(model)}


