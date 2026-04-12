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

attribute<-c("clay","silt","sand","tceq","cfgr","wg1500","wg0200","wg0033","nitkjd","phetol","phetb1","phetm3","orgc","totc","phca")

initial<-data.frame(
	soil=character(),
	property=character(),
	Df=numeric(),
	Sum.sqr=numeric(),
	Mean.sqr=numeric(),
	F.value=numeric(),
	p.value=numeric())

for(i in attribute){
	df<-data.one[,c("station_id","biome",i)]

	df$station_id<-as.factor(df$station_id)
	df$biome<-as.factor(df$biome)

	colnames(df)<-c("station_id","biome","soil")

	model<-aov(soil~biome+station_id,df)
	summary<-summary(model)[[1]]
	initial<-rbind(initial,data.frame(
		soil=rep(i,nrow(summary)),       
		property=rownames(summary),
		Df=summary$Df,
		Sum.sqr=summary$`Sum Sq`,
		Mean.sqr=summary$`Mean Sq`,
		F.value=summary$`F value`,
		p.value=summary$`Pr(>F)`))}

initial$sign<-ifelse(initial$p.value<0.05,"yes","no")

tukey.results<-data.frame(soil=character(),biomes=character(),p.adj=numeric())

for(h in attribute){
	df<-data.one[,c("biome",h)]
	df$biome<-as.factor(df$biome)
	colnames(df)<-c("biome","soil")
	model<-aov(soil~biome,data=df)
	test<-TukeyHSD(model)
	tukey<-as.data.frame(test$biome)
for(j in 1:nrow(tukey)){
if(tukey$`p adj`[[j]]<0.05){
	tukey.results<-rbind(tukey.results,data.frame(soil=h,biomes=rownames(tukey)[[j]],p.adj=tukey$`p adj`[[j]]))}}}

tukey.results<-tukey.results[order(tukey.results$biome),]

tukey.results$sign<-ifelse(tukey.results$p.adj<0.05,"yes","no")

biomes.list<-list()

for(k in attribute){
	df<-data.one[,c("biome",k)]
	df$biome<-as.factor(df$biome)
	colnames(df)<-c("biome","soil")
	model<-aov(soil~biome,df)
	test<-TukeyHSD(model)
	biomes.list[[k]]<-multcompLetters4(model,test)}

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

#sand.models<-list()
#sand.tests<-list()
#sand.var<-list()

#sand.property<-c("map","total.avg.tmax","total.avg.tmin","tceq","orgm","phca","cfgr","cfvo")

#for(k in sand.property){
#	df<-data.one[,c("station_id","biome","sand",k)]
#	colnames(df)<-c("station_id","biome","sand",paste0(k))
#	model<-lmer(sand~df[,4]+(1|biome)+(1|station_id),df)
#	sand.models[[k]]<-summary(model)
#	sand.tests[[k]]<-Anova(model,type="II")
#	sand.var[[k]]<-var.contr(model)}

#silt.models<-list()
#silt.tests<-list()
#silt.var<-list()

#silt.property<-c("map","total.avg.tmax","total.avg.tmin","tceq","orgm","phca","cfgr","cfvo")

#for(k in silt.property){
#        df<-data.one[,c("station_id","biome","silt",k)]
#        colnames(df)<-c("station_id","biome","silt",paste0(k))
#        model<-lmer(silt~df[,4]+(1|biome)+(1|station_id),df)
#        silt.models[[k]]<-summary(model)
#        silt.tests[[k]]<-Anova(model,type="II")
#        silt.var[[k]]<-var.contr(model)}

clay.models<-list()
clay.tests<-list()
clay.var<-list()

clay.property<-c("map","total.avg.tmax","total.avg.tmin","tceq","orgm","phca","cfgr","cfvo")

for(k in clay.property){
        df<-data.one[,c("station_id","biome","clay",k)]
        colnames(df)<-c("station_id","biome","clay",paste0(k))
        model<-lmer(clay~df[,4]+(1|biome)+(1|station_id),df)
        clay.models[[k]]<-summary(model)
        clay.tests[[k]]<-Anova(model,type="II")
        clay.var[[k]]<-var.contr(model)}

#tceq.models<-list()
#tceq.tests<-list()
#tceq.var<-list()

#tceq.property<-c("map","total.avg.tmax","total.avg.tmin","totc","orgc","orgm","phca","cfgr","cfvo","sand","silt","clay")

#for(k in tceq.property){
#        df<-data.one[,c("station_id","biome","tceq",k)]
#        colnames(df)<-c("station_id","biome","tceq",paste0(k))
#        model<-lmer(tceq~df[,4]+(1|biome)+(1|station_id),df)
#       tceq.models[[k]]<-summary(model)
#      tceq.tests[[k]]<-Anova(model,type="II")
#     tceq.var[[k]]<-var.contr(model)}

#orgm.models<-list()
#orgm.tests<-list()
#orgm.var<-list()

#orgm.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","cfgr","cfvo")

#for(k in orgm.property){
#        df<-data.one[,c("station_id","biome","orgm",k)]
#	df<-na.omit(df)
#if(nrow(df)<3) next
 #       colnames(df)<-c("station_id","biome","orgm",paste0(k))
  #      model<-lmer(orgm~df[,4]+(1|biome)+(1|station_id),df)
   #     orgm.models[[k]]<-summary(model)
    #    orgm.tests[[k]]<-Anova(model,type="II")
     #   orgm.var[[k]]<-var.contr(model)}

#cfgr.models<-list()
#cfgr.tests<-list()
#cfgr.var<-list()

#cfgr.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq")

#for(k in cfgr.property){
#        df1<-df[,c("station_id","biome","cfgr",k)]
#	df1<-na.omit(df1)
#if(nrow(df1)<3) next
#        colnames(df1)<-c("station_id","biome","cfgr","prop")
#        model<-lmer(cfgr~prop+(1|biome)+(1|station_id),df1)
#        cfgr.models[[k]]<-summary(model)
#        cfgr.tests[[k]]<-Anova(model,type="II")
#        cfgr.var[[k]]<-var.contr(model)}

#cfvo.models<-list()
#cfvo.tests<-list()
#cfvo.var<-list()

#cfvo.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq")

#for(k in cfvo.property){
 #       df<-data.one[,c("station_id","biome","cfvo",k)]
  #      colnames(df)<-c("station_id","biome","cfvo",paste0(k))
   #     model<-lmer(cfvo~df[,4]+(1|biome)+(1|station_id),df)
    #    cfvo.models[[k]]<-summary(model)
     #   cfvo.tests[[k]]<-Anova(model,type="II")
      #  cfvo.var[[k]]<-var.contr(model)}

#wg1500.models<-list()
#wg1500.tests<-list()
#wg1500.var<-list()

#wg1500.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

#for(k in wg1500.property){
#        df<-data.one[,c("station_id","biome","wg1500",k)]
#	df<-na.omit(df)
#if(nrow(df)<3) next
#        colnames(df)<-c("station_id","biome","wg1500",paste0(k))
#        model<-lmer(wg1500~df[,4]+(1|biome)+(1|station_id),df)
#	wg1500.models[[k]]<-summary(model)
#	wg1500.tests[[k]]<-Anova(model,type="II")
#	wg1500.var[[k]]<-var.contr(model)}

#wg1500.model<-lmer(wg1500~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
#wg1500.test<-Anova(wg1500.model,type="II")
#wg1500.var<-var.contr(wg1500.model)

#wg0200.models<-list()
#wg0200.tests<-list()
#wg0200.var<-list()

#wg0200.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

#for(k in wg0200.property){
#        df<-data.one[,c("station_id","biome","wg0200",k)]
#	df<-na.omit(df)
#if(nrow(df)<3) next
#	colnames(df)<-c("station_id","biome","wg0200",paste0(k))
#	model<-lmer(wg0200~df[,4]+(1|biome)+(1|station_id),df)
#	wg0200.models[[k]]<-summary(model)
#	wg0200.tests[[k]]<-Anova(model,type="II")
#	wg0200.var[[k]]<-var.contr(model)}

#wg0200.model<-lmer(wg0200~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
#wg0200.test<-Anova(wg0200.model,type="II")
#wg0200.var<-var.contr(wg0200.model)

#wg0033.models<-list()
#wg0033.tests<-list()
#wg0033.var<-list()

#wg0033.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgm","orgc","elco50","ecec","bdwsod","bdfifm")

#for(k in wg0033.property){
#        df<-data.one[,c("station_id","biome","wg0033",k)]
#        df<-na.omit(df)
#if(nrow(df)<3) next
#	colnames(df)<-c("station_id","biome","wg0033",paste0(k))     
#	model<-lmer(wg0033~df[,4]+(1|biome)+(1|station_id),df)
#	wg0033.models[[k]]<-summary(model)
#	wg0033.tests[[k]]<-Anova(model,type="II")
#	wg0033.var[[k]]<-var.contr(model)}

#wg0033.model<-lmer(wg0033~sand*silt*clay*orgc*ecec*bdwsod+(1|station_id)+(1|biome),data.one)
#wg0033.test<-Anova(wg0033.model,type="II")
#wg0033.var<-var.contr(wg0033.model)


nitkjd.models<-list()
nitkjd.tests<-list()
nitkjd.var<-list()

nitkjd.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","totc","orgm","orgc","elco50","ecec","cecph7",
	"bdwsod","bdfifm","phetb1","phetm3","cfgr","cfvo","wg1500","wg0200","wg0033")

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

orgc.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","totc","orgm","nitkjd","elco50","ecec","cecph7",
        "bdwsod","bdfifm","phetb1","phetm3","cfgr","cfvo","wg1500","wg0200","wg0033")

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

totc.property<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","tceq","orgc","orgm","nitkjd","elco50","ecec","cecph7",
        "bdwsod","bdfifm","phetb1","phetm3","cfgr","cfvo","wg1500","wg0200","wg0033")

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


#for(i in properties){
#	df<-data.one[,c("nitkjd","station_id","biome",i)]
#	df<-na.omit(df)
#if(nrow(df)<3) next
#	colnames(df)<-c("nitkjd","station_id","biome","climate")
#	model<-lmer(nitkjd~climate+(1|biome)+(1|station_id),df)
#	test<-Anova(mode,type="II")
#	N.biomes<-rbind(N.biomes,data.frame(
#		biome=i,
#		p.value=test[,3],
#
#biomes<-c("boreal","desert","mediterranean","montane.grassland","temperate.forest","temperate.conifer",
#		"temperate.grassland","tropical.coniferous","tropical.dry","tropical.grassland","tropical.moist",
#		"tundra")

#climate<-c("n.prcp.xhigh","n.dry.days","n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

#df<-data.one[!is.na(data.one$nitkjd),]

#   nitkjd.biomes<-data.frame(
 #               biome=character(),
  #              climate=character(),
   #             estimate=numeric(),
    #            p.value=numeric())
		
#for(j in biomes){
#	df1<-subset(df,df$biome==j)
#for(h in climate){
#	df2<-df1[,c("station_id","nitkjd",h)]
#	df2<-na.omit(df2)
#if(nrow(df1)<3) next
#	colnames(df2)<-c("station_id","nitkjd","climate")
#	model<-lmer(nitkjd~climate+(1|station_id),df2)
#	a<-as.data.frame(summary(model)$coefficients)
#	test<-Anova(model,type="II")
#	nitkjd.biomes<-rbind(nitkjd.biomes,data.frame(
#		biome=j,
#		climate=h,
#		estimate=a[2,3],
#		p.value=test[1,3]))}}
#nitkjd.biomes$sign<-ifelse(nitkjd.biomes$p.value<0.05,"yes","no")

#df<-data.one[!is.na(data.one$orgc),]

#orgc.biomes<-data.frame(
#	biome=character(),
#	climate=character(),
#	estimate=numeric(),
#	p.value=numeric())

#for(j in biomes){
#        df1<-subset(df,df$biome==j)
#for(h in climate){
#        df2<-df1[,c("station_id","orgc",h)]
#        df2<-na.omit(df2)
#if(nrow(df1)<3) next
#        colnames(df2)<-c("station_id","orgc","climate")
#        model<-lmer(orgc~climate+(1|station_id),df2)
#        a<-as.data.frame(summary(model)$coefficients)
#        test<-Anova(model,type="II")
#        orgc.biomes<-rbind(orgc.biomes,data.frame(
#                biome=j,
#                climate=h,
#                estimate=a[2,3],
#                p.value=test[1,3]))}}
#orgc.biomes$sign<-ifelse(nitkjd.biomes$p.value<0.05,"yes","no")

#df<-data.one[!is.na(data.one$totc),]

#totc.biomes<-data.frame(
 #       biome=character(),
  #      climate=character(),
   #     estimate=numeric(),
    #    p.value=numeric())

#for(j in biomes){
#        df1<-subset(df,df$biome==j)
#for(h in climate){
#        df2<-df1[,c("station_id","totc",h)]
#        df2<-na.omit(df2)
#if(nrow(df1)<3) next
#        colnames(df2)<-c("station_id","totc","climate")
#        model<-lmer(totc~climate+(1|station_id),df2)
#        a<-as.data.frame(summary(model)$coefficients)
#        test<-Anova(model,type="II")
#        totc.biomes<-rbind(totc.biomes,data.frame(
#                biome=j,
#                climate=h,
#                estimate=a[2,3],
#                p.value=test[1,3]))}}
#totc.biomes$sign<-ifelse(totc.biomes$p.value<0.05,"yes","no")

#df<-data.one[!is.na(data.one$phetol),]

#phetol.biomes<-data.frame(
#        biome=character(),
#        climate=character(),
#        estimate=numeric(),
#        p.value=numeric())

#for(j in biomes){
#        df1<-subset(df,df$biome==j)
#for(h in climate){
#        df2<-df1[,c("station_id","phetol",h)]
#        df2<-na.omit(df2)
#if(nrow(df1)<3) next
#        colnames(df2)<-c("station_id","phetol","climate")
#        model<-lmer(phetol~climate+(1|station_id),df2)
#        a<-as.data.frame(summary(model)$coefficients)
#        test<-Anova(model,type="II")
#        phetol.biomes<-rbind(phetol.biomes,data.frame(
#                biome=j,
##                climate=h,
#                estimate=a[2,3],
#                p.value=test[1,3]))}}
#phetol.biomes$sign<-ifelse(phetol.biomes$p.value<0.05,"yes","no")

