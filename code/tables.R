library(tidyverse)
library(data.table)
library(lme4)
library(car)
library(multcompView)

se <- function(x) sd(na.omit(x))/sqrt(length(na.omit(x)))
avg <- function(x) mean(na.omit(x))

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

nutrients<-c("nitkjd","orgc","phetb1")
climate<-c("n.prcp.xhigh","n.prcp.event","n.dry.days","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

data.one$station_id<-as.character(data.one$station_id)

site<-unique(data.one$station_id)

slopes<-list()

for(i in nutrients){
for(h in climate){
temp<-data.frame(nutrient=character(),climate=character(),varPair=numeric(),varResid=numeric(),slope=numeric(),std.err=numeric(),
        t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
for(j in site){
	df<-na.omit(data.one[data.one$station_id==j,c("station_id",i,h)])
	colnames(df)<-c("station_id","nutrient","climate")
	df$nutrient<-as.numeric(df$nutrient)
	df$climate<-as.numeric(df$climate)
if(nrow(df) < 3) next
if(length(unique(df$climate)) < 2) next
	model<-lm(nutrient~climate,data=df)
	coeff<-summary(model)$coefficients
	summ<-summary(model)
temp<-rbind(temp,data.frame(
	station_id=j,
	nutrient=i,
	climate=h,
	slope=coeff[2,1],
	std.err=coeff[2,2],
	t.value=coeff[2,3],
	p.value=coeff[2,4],
	r2=summ$r.squared,
	r2.adj=summ$adj.r.squared))}
	temp$sig<-ifelse(temp$r2.adj<0.05,"yes","no")
	temp.1<-temp[,c("station_id","slope")]
colnames(temp.1)<-c("station_id",paste0(i,sep=".",h))
name<-paste0(i,sep=".",h)
slopes[[name]]<-temp.1
}}

temp.slopes<-data.frame()

for(f in 1:length(slopes)){
	temp.slopes<-merge(temp.slopes,slopes[[f]],all.y=TRUE)}

data<-merge(data.one,temp.slopes,all.x=TRUE)

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

data<-merge(data,biome,all.x=TRUE)

data<-data[,colSums(!is.na(data))> 0]

slopes<-c("nitkjd.n.prcp.event","nitkjd.n.dry.days","nitkjd.n.tmax.xhigh","nitkjd.n.tmax.xlow","nitkjd.n.tmax.avg",
	"nitkjd.n.tmin.xhigh","nitkjd.n.tmin.xlow","nitkjd.n.tmin.avg","orgc.n.prcp.xhigh","orgc.n.prcp.event","orgc.n.dry.days",
	"orgc.n.tmax.xhigh","orgc.n.tmax.xlow","orgc.n.tmax.avg","orgc.n.tmin.xhigh","orgc.n.tmin.xlow","orgc.n.tmin.avg",
	"phetb1.n.prcp.xhigh","phetb1.n.prcp.event","phetb1.n.dry.days","phetb1.n.tmax.xhigh","phetb1.n.tmax.xlow","phetb1.n.tmax.avg",
	"phetb1.n.tmin.xhigh","phetb1.n.tmin.xlow","phetb1.n.tmin.avg")

biomes.results<-data.frame(
	name=character(),
	Df=numeric(),
	f.value=numeric(),
	p.value=numeric())

tukey.results<-list()

sig.results<-list()

for(ss in slopes){
	df<-data[,c(ss,"biome")]
	colnames(df)<-c("slope","biome")
	df<-na.omit(df)
	df$biome<-as.factor(df$biome)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
	model<-aov(slope~biome,data=df)
	stats<-summary(model)[[1]]
	biomes.results<-rbind(biomes.results,data.frame(
		name=ss,
		Df=stats$`Df`[1],
		f.value=stats$`F value`[1],
		p.value=stats$`Pr(>F)`[1]))
	post.hoc<-TukeyHSD(model)
	tukey<-as.data.frame(post.hoc$biome)
	tukey$sig<-ifelse(tukey$`p adj`<0.05,"yes","no")
	tukey.results[[ss]]<-tukey
	sig.results[[ss]]<-multcompLetters4(model,post.hoc)}
biomes.results$signif<-ifelse(biomes.results$p.value<0.05,"yes","no")

tukey<-data.frame(
	name=character(),
	p.value=numeric(),
	sig=numeric())

for(l in 1:length(tukey.results)){
	df<-as.data.frame(tukey.results[[l]])
	df<-df[,c("p adj","sig")]
for(w in 1:nrow(df)){
	tukey<-rbind(tukey,data.frame(
		name=paste0(names(tukey.results)[[l]],sep="-",rownames(df)[[w]]),
		p.value=df[w,"p adj"],
		sig=df[w,"sig"]))
}}

write.csv(tukey,"/disks/home/abigail/thesis-repository/code/data/tukey.results.csv")
write.csv(biomes.results,"/disks/home/abigail/thesis-repository/code/data/biomes.results.csv")

avg<-list()
err<-list()

for(s in 1:length(slopes)){
	slope.1<-slopes[[s]]
	mean<-mean(data[,c(slope.1)],na.rm=TRUE)
	std.err<-se(data[,c(slope.1)])
	avg[[s]]<-as.data.frame(mean)
	err[[s]]<-as.data.frame(std.err)}

stats<-data.frame(
	slope=character(),
	mean=numeric(),
	std.err=numeric())

for(s in 1:length(slopes)){
	stats<-rbind(stats,data.frame(
		slope=slopes[[s]],
		mean=avg[[s]],
		std.err=err[[s]]))}

write.csv(stats,"/disks/home/abigail/thesis-repository/code/data/mean.se.csv")
