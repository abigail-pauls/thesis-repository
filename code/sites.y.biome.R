library(multcompView)
library(tidyverse)
library(data.table)
library(lme4)
library(car)

#write.csv(data.one,"/disks/home/abigail/thesis-repository/code/data/site.regression.csv")

sites.biomes<-data.frame(climate=character(),Df=numeric(),p.value=numeric())
tukey.results<-data.frame(biome=character(),p.adj=numeric(),sign=character())
biome.list<-list()

list<-colnames(data.one[,115:283])

for(n in list){
	dframe<-data.one[,c(n,"biome")]
	dframe<-na.omit(dframe)
	dframe$biome<-as.factor(dframe$biome)
	colnames(dframe)<-c("climate","biome")
if(nrow(dframe)<3) next
if(length(levels(dframe$biome))<2) next
	model<-aov(climate~biome,dframe)
	stats<-summary(model)[[1]]
	sites.biomes<-rbind(sites.biomes,data.frame(
		climate=n,
		Df=stats$Df[1],
		p.value=stats$`Pr(>F)`[1]))
	tukey<-TukeyHSD(model)
biome.list[[n]]<-multcompLetters4(model,tukey)
	tukey<-as.data.frame(tukey$biome)
	tukey.results<-rbind(tukey.results,data.frame(
		biome=rownames(tukey),
		p.adj=tukey$`p adj`,
		sign=ifelse(tukey$`p adj`<0.05,"yes","no")))}

sites.biomes$sign<-ifelse(sites.biomes$p.value<0.05,"yes","no")

tukey.results<-tukey.results[order(tukey.results$biome),]

