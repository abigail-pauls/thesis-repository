library(multcomp)
#library(tidyverse)
#library(data.table)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

#data.one<-subset(data,data$soil.layer=="one")

#ecosystems<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.csv")

#biome<-ecosystems[,c(1,4,6,7)]
#colnames(biome)<-c("station_id","eco_name","biome_name","realm")

#biome <- biome %>% dplyr::mutate(biome = NA_real_)

#biome_map <- c(
#  "Boreal Forests/Taiga" = "boreal",
#  "Deserts & Xeric Shrublands" = "desert",
#  "Mediterranean Forests, Woodlands & Scrub" = "mediterranean",
#  "Montane Grasslands & Shrublands" = "montane.grassland",
#  "Temperate Broadleaf & Mixed Forests" = "temperate.forest",
#  "Temperate Conifer Forests" = "temperate.conifer",
#  "Temperate Grasslands, Savannas & Shrublands" = "temperate.grassland",
#  "Tropical & Subtropical Coniferous Forests" = "tropical.coniferous",
#  "Tropical & Subtropical Dry Broadleaf Forests" = "tropical.dry",
#  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "tropical.grassland",
#  "Tropical & Subtropical Moist Broadleaf Forests" = "tropical.moist",
#  "Tundra" = "tundra"
#)

#biome$biome <- biome_map[biome$biome_name]

#data.one<-merge(data.one,biome,all.x=TRUE)

#data.one<-data.one[,colSums(!is.na(data.one))> 0]

#soil.phys<-data.one[,c("station_id","biome","sand","silt","clay","tceq","wg1500","wg0200","wg0033")]

#model<-aov(sand~biome+station_id,data=soil.phys)

#attribute<-c("clay","silt","sand","tceq","cfgr","wg1500","wg0200","wg0033")

#initial<-data.frame(
#	soil=character(),
#	property=character(),
#	Df=numeric(),
#	Sum.sqr=numeric(),
#	Mean.sqr=numeric(),
#	F.value=numeric(),
#	p.value=numeric())

#for(i in attribute){
#       df<-data.one[,c("station_id","biome",i)]
#       df$station_id<-as.factor(df$station_id)
#	df$biome<-as.factor(df$biome)
#	colnames(df)<-c("station_id","biome","soil")
#	model<-aov(soil~biome+station_id,df)
#	s<-summary(model)[[1]]
#	initial<-rbind(initial,data.frame(
#		soil=rep(i,nrow(s)),       
#		property=rownames(s),
#		Df=s$Df[1],
#		Sum.sqr=s$`Sum Sq`[1],
#		Mean.sqr=s$`Mean Sq`[1],
#		F.value=s$`F value`[1],
#		p.value=s$`Pr(>F)`[1]))}

#tukey.results<-data.frame(
#	soil=character(),
#	biomes=character(),
#	p.adj=numeric())

#for(i in attribute){
#	df<-data.one[,c("biome",i)]
#	df$biome<-as.factor(df$biome)
#	colnames(df)<-c("biome","soil")
#	model<-aov(soil~biome,df)
#	test<-TukeyHSD(model)
#	tukey<-as.data.frame(test$biome)
#for(j in 1:nrow(tukey)){
#if(tukey$`p adj`[[j]]<0.05){
#	tukey.results<-rbind(tukey.results,data.frame(
#		soil=i,
#		biomes=rownames(tukey)[[j]],
#		p.adj=tukey$`p adj`[[j]]))}}}

#tukey.results<-tukey.results[order(tukey.results$biome),]

#biomes.list<-list()

#for(i in attribute){
 #      df<-data.one[,c("biome",i)]
  #     df$biome<-as.factor(df$biome)
   #    colnames(df)<-c("biome","soil")
    #   model<-aov(soil~biome,df)
     #  test<-TukeyHSD(model)
#	biomes.list[[i]]<-multcompLetters4(model,test)}


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
		variance=c(ranvar,fixvar),
		proportion=contr,
		precent=contr*100)
	return(varcontr)}

#fix.contr<-function(model){
#	X<-model.matrix(model)
#	beta<-fixef(model)
#	fixvar<-var(as.vector(X%*%beta))
#	total<-sum(ranvar)+sum(fixvar)
#	rancontr<-ranvar/total
#	fixcontr<-fixvar/total	
#	df<-data.frame(
#		effect=c(rownames(raneff),"fixed.effects"),
#		proportion=c(rancontr,fixcontr),
#		precent=c(rancontr*100,fixcontr*100))}
