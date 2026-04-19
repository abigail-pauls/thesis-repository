#library(tidyverse)
#library(data.table)
#library(lme4)
#library(car)
#library(multcompView)
#library(emmeans)
#library(multcomp)
#library(agricolae)

se <- function(x){
	x<-na.omit(x)
	sd(x)/sqrt(length(x))}

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

#data$continent<-as.factor(data$continent)
#data$country_name<-as.factor(data$country_name)
#data$region<-as.factor(data$region)
#data$station_id<-as.factor(data$station_id)

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

nutrients<-c("nitkjd","orgc")
climate<-c("n.prcp.xhigh","n.prcp.event","n.dry.days","n.tmax.xhigh","n.tmax.xlow","n.tmax.avg","n.tmin.xhigh","n.tmin.xlow","n.tmin.avg")

data$station_id<-as.character(data$station_id)

#ecosystems<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.csv")

#biome<-ecosystems[,c(1,4,6,7)]
#colnames(biome)<-c("station_id","eco_name","biome_name","realm")

#biome <- biome %>% dplyr::mutate(biome = NA_real_)

#biome_map <- c(
#"Boreal Forests/Taiga" = "boreal",
#"Deserts & Xeric Shrublands" = "desert",
#"Mediterranean Forests, Woodlands & Scrub" = "mediterranean",
#"Montane Grasslands & Shrublands" = "montane.grassland",
#"Temperate Broadleaf & Mixed Forests" = "temperate.forest",
#"Temperate Conifer Forests" = "temperate.conifer",
#"Temperate Grasslands, Savannas & Shrublands" = "temperate.grassland",
#"Tropical & Subtropical Coniferous Forests" = "tropical.coniferous",
#"Tropical & Subtropical Dry Broadleaf Forests" = "tropical.dry",
#"Tropical & Subtropical Grasslands, Savannas & Shrublands" = "tropical.grassland",
#"Tropical & Subtropical Moist Broadleaf Forests" = "tropical.moist",
#"Tundra" = "tundra")

#biome$biome <- biome_map[biome$biome_name]

#data<-merge(data,biome,all.x=TRUE)

#data<-data[,colSums(!is.na(data))> 0]

data<-with(data,data[biome=="desert"|biome=="mediterranean"|biome=="temperate.conifer"|biome=="temperate.forest"|
	biome=="tropical.grassland"|biome=="tropical.moist",])

site<-unique(data$station_id)

slopes<-list()

for(i in nutrients){
for(h in climate){
	temp<-data.frame(nutrient=character(),climate=character(),varPair=numeric(),varResid=numeric(),
	slope=numeric(),std.err=numeric(),t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
for(j in site){
	df<-na.omit(data[data$station_id==j,c("station_id",i,h)])
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

temp.slopes<-slopes[[1]]

for(f in 1:length(slopes)){
	temp.slopes<-merge(temp.slopes,slopes[[f]],all=TRUE)}

data<-merge(data,temp.slopes,all.x=TRUE)

write.csv(data,"/disks/home/abigail/thesis-repository/code/data/data.w.slopes.csv")

#slopes<-c("nitkjd.n.prcp.event","nitkjd.n.dry.days","nitkjd.n.tmax.xhigh","nitkjd.n.tmax.xlow","nitkjd.n.tmax.avg",
#	"nitkjd.n.tmin.xhigh","nitkjd.n.tmin.xlow","nitkjd.n.tmin.avg","orgc.n.prcp.xhigh","orgc.n.prcp.event","orgc.n.dry.days",
#	"orgc.n.tmax.xhigh","orgc.n.tmax.xlow","orgc.n.tmax.avg","orgc.n.tmin.xhigh","orgc.n.tmin.xlow","orgc.n.tmin.avg")

#biomes.results<-data.frame(
#	name=character(),
#	Df=numeric(),
#	f.value=numeric(),
#	p.value=numeric())

#tukey.results<-list()

#sig.results<-list()

#for(ss in slopes){
#	df<-data[,c(ss,"biome")]
#	colnames(df)<-c("slope","biome")
#	df<-na.omit(df)
#	df$biome<-as.factor(df$biome)
#if(nrow(df)<3) next
#if(length(levels(df$biome))<2)next
#	model<-aov(slope~biome,data=df)
#	stats<-summary(model)[[1]]
#	biomes.results<-rbind(biomes.results,data.frame(
#		name=ss,
#		Df=stats$`Df`[1],
#		f.value=stats$`F value`[1],
#		p.value=stats$`Pr(>F)`[1]))
#	post.hoc<-TukeyHSD(model)
#	tukey<-as.data.frame(post.hoc$biome)
#	tukey$sig<-ifelse(tukey$`p adj`<0.05,"yes","no")
#	tukey.results[[ss]]<-tukey
#	sig.results[[ss]]<-multcompLetters4(model,post.hoc)}
#biomes.results$signif<-ifelse(biomes.results$p.value<0.05,"yes","no")

#tukey<-data.frame(
#	name=character(),
#	p.value=numeric(),
#	sig=numeric())

#for(l in 1:length(tukey.results)){
#	df<-as.data.frame(tukey.results[[l]])
#	df<-df[,c("p adj","sig")]
#for(w in 1:nrow(df)){
#	tukey<-rbind(tukey,data.frame(
#		name=paste0(names(tukey.results)[[l]],sep="-",rownames(df)[[w]]),
#		p.value=df[w,"p adj"],
#		sig=df[w,"sig"]))
#}}

#write.csv(tukey,"/disks/home/abigail/thesis-repository/code/data/tukey.results.csv")
#write.csv(biomes.results,"/disks/home/abigail/thesis-repository/code/data/biomes.results.csv")

#nitkjd.bio<-aov(nitkjd~biome,data)
#orgc.bio<-aov(orgc~biome,data)

#nitkjd.bio.t<-TukeyHSD(nitkjd.bio)
#orgc.bio.t<-TukeyHSD(orgc.bio)

#nitkjd.bio.l<-multcompLetters4(nitkjd.bio,nitkjd.bio.t)
#orgc.bio.l<-multcompLetters4(orgc.bio,orgc.bio.t)

#data$biome<-as.factor(data$biome)

#nitkjd.mean<-aggregate(nitkjd~biome,data=data,mean,na.rm=TRUE)
#orgc.mean<-aggregate(orgc~biome,data=data,mean,na.rm=TRUE)

#nitkjd.se<-aggregate(nitkjd~biome,data=data,se)
#orgc.se<-aggregate(orgc~biome,data=data,se)

#avg<-list()
#err<-list()

#for(s in 1:length(slopes)){
#	slope.1<-slopes[[s]]
#	mean<-mean(data[,c(slope.1)],na.rm=TRUE)
#	std.err<-se(data[,c(slope.1)])
#	avg[[s]]<-as.data.frame(mean)
#	err[[s]]<-as.data.frame(std.err)}

#stats<-data.frame(
#	slope=character(),
#	mean=numeric(),
#	std.err=numeric())

#for(s in 1:length(slopes)){
#	stats<-rbind(stats,data.frame(
#		slope=slopes[[s]],
#		mean=avg[[s]],
#		std.err=err[[s]]))}

#write.csv(stats,"/disks/home/abigail/thesis-repository/code/data/mean.se.csv")

#biome.mean.se<-list()

#for(s in 1:length(slopes)){
#	col<-slopes[[s]]
#	df<-data[,c(col,"biome")]
#	colnames(df)<-c("slopes","biome")
#	mean.avg<-aggregate(slopes~biome,df,FUN=avg)
#	std.err<-aggregate(slopes~biome,df,FUN=se)
#	std.err<-as.data.frame(std.err)
#	colnames(std.err)<-c("biome","std.err")
#	df.1<-merge(as.data.frame(mean.avg),std.err,all=TRUE)
#	biome.mean.se[[col]]<-df.1}

#keep<-data

slopes.n<-c("nitkjd.n.prcp.event","nitkjd.n.dry.days","nitkjd.n.tmax.xhigh","nitkjd.n.tmax.xlow","nitkjd.n.tmax.avg",
       "nitkjd.n.tmin.xhigh","nitkjd.n.tmin.xlow","nitkjd.n.tmin.avg")

#data <- data %>%
 # mutate(across(all_of(slopes.n), ~{
  #  x <- as.numeric(.)
   # q1 <- quantile(x, 0.25, na.rm = TRUE)
   # q3 <- quantile(x, 0.75, na.rm = TRUE)
   # iqr <- IQR(x, na.rm = TRUE)
   # low <- q1 - 1.5 * iqr
   # high <- q3 + 1.5 * iqr
   # replace(x, x <= low | x >= high, NA)
  #}))

soil.n<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","ecec","elco50")

nitkjd.lm<-list()
nitkjd.emtrends<-list()
nitkjd.pairs<-list()
nitkjd.letters<-list()

for(ss in slopes.n){
for(cc in soil.n){
	df<-data[,c(ss,cc,"biome")]
	colnames(df)<-c("slope","soil","biome")
	df<-na.omit(df)
if(nrow(df)<3) next
if(length(unique(df$biome))<2) next
	model<-lm(slope~soil*biome,df)
name<-paste0(ss,sep=".",cc)
nitkjd.lm[[name]]<-summary(model)
	slopes<-emtrends(model,~biome,var="soil")	
nitkjd.emtrends[[name]]<-slopes	
	pw<-pairs(slopes,adjust="tukey")
	pwdf<-as.data.frame(pw)
nitkjd.pairs[[name]]<-pwdf
pval<-pwdf$p.value
names(pval)<-pwdf$contrast
valid <- !is.na(pval) & !is.na(names(pval))
	pval <- pval[valid]
	names(pval) <- gsub(" - ", "-", names(pval))
	valid_names <- grepl("^[^-]+-[^-]+$", names(pval))
	pval <- pval[valid_names]
letters<-multcompLetters(pval)
nitkjd.letters[[name]] <- letters$Letters
}}

slopes.c<-c("orgc.n.prcp.xhigh","orgc.n.prcp.event","orgc.n.dry.days","orgc.n.tmax.xhigh","orgc.n.tmax.xlow",
      "orgc.n.tmax.avg","orgc.n.tmin.xhigh","orgc.n.tmin.xlow","orgc.n.tmin.avg")

#data <- data %>%
 # mutate(across(all_of(slopes.c), ~{
  #  x <- as.numeric(.)
   # q1 <- quantile(x, 0.25, na.rm = TRUE)
   # q3 <- quantile(x, 0.75, na.rm = TRUE)
   # iqr <- IQR(x, na.rm = TRUE)
   # low <- q1 - 1.5 * iqr
   # high <- q3 + 1.5 * iqr
   # replace(x, x <= low | x >= high, NA)
 # }))

soil.c<-c("map","total.avg.tmax","total.avg.tmin","sand","silt","clay","phca","ecec","cfgr")

orgc.lm<-list()
orgc.emtrends<-list()
orgc.pairs<-list()
orgc.letters<-list()

for(ss in slopes.c){
for(cc in soil.c){
       df<-data[,c(ss,cc,"biome")]
      colnames(df)<-c("slope","soil","biome")
     df<-na.omit(df)
if(nrow(df)<3) next
if(length(unique(df$biome))<2) next
        model<-lm(slope~soil*biome,df)
name<-paste0(ss,sep=".",cc)
orgc.lm[[name]]<-summary(model)
	slopes<-emtrends(model,~biome,var="soil")
orgc.emtrends[[name]]<-slopes
pw<-pairs(slopes,adjust="tukey")
       pwdf<-as.data.frame(pw)
orgc.pairs[[name]]<-pwdf
pval<-pwdf$p.value
names(pval)<-pwdf$contrast
valid <- !is.na(pval) & !is.na(names(pval))
       pval <- pval[valid]
     names(pval) <- gsub(" - ", "-", names(pval))
    valid_names <- grepl("^[^-]+-[^-]+$", names(pval))
    pval <- pval[valid_names]
letters<-multcompLetters(pval)
orgc.letters[[name]] <- letters$Letters
}}

