
#library(data.table)
#library(tidyverse)

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

setDT(data.one)

slope.w<-c("wg1500.n.prcp.xhigh.length.est","wg1500.n.dry.days.est","wg1500.n.dry.length.est","wg1500.n.tmax.xhigh.est",
"wg1500.n.tmax.xhigh.length.est","wg1500.n.tmax.high.est","wg1500.n.tmax.high.length.est","wg0200.n.prcp.xhigh.est",
"wg0200.n.prcp.xhigh.length.est","wg0200.n.dry.days.est","wg0200.n.dry.length.est","wg0200.n.tmax.xhigh.est",
"wg0033.n.prcp.xhigh.est","wg0033.n.prcp.xhigh.length.est","wg0033.n.dry.days.est","wg0033.n.dry.length.est",
"wg0033.n.tmax.xhigh.est","wg0033.n.tmax.xhigh.length.est","wg0033.n.tmax.high.est","wg0033.n.tmax.high.length.est")

predictors.w<- c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt",
                    "sand","phetol","phetm3","phetb1","phca","elco50","cecph7","bdwsod",
                    "bdfi33","map","total.avg.tmax","total.avg.tmin")

water.slope<-data.frame(slope=character(),predictor=character(),estimate=numeric(),std.err=numeric(),t.value=numeric(),
			p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope.w){
for(pp in predictors.w){
	df<-data.one[,c(m,pp),with=FALSE]
	df<-na.omit(df)
	colnames(df)<-c("slope","property")
	df<-subset(df,is.finite(df$slope)&is.finite(df$property))
if(nrow(df)<3) next
	model<-lm(slope~property,data=df)
	sum<-summary(model)
	coef<-coef(summary(model))	
if(nrow(coef)<2) next
	water.slope<-rbind(water.slope,data.frame(slope=m,predictor=pp,estimate=coef[2,1],std.err=coef[2,2],
				t.value=coef[2,3],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}
water.slope$significant<-ifelse(water.slope$p.value<0.05,"yes","no")

write.csv(water.slope,"/disks/home/abigail/thesis-repository/code/data/water.slope.csv")


slope.c<-c("orgc.n.dry.days.est","orgc.n.tmax.xhigh.est","orgc.n.tmax.xhigh.length.est","orgc.n.tmax.high.est",
	"orgc.n.tmin.high.est","orgc.n.tmin.high.length.est","orgc.n.tmin.xhigh.est","orgc.n.tmin.xhigh.length.est",
	"orgc.n.tmin.xlow.est","orgc.n.tmin.low.est","orgc.n.tmin.avg.est","totc.n.dry.days.est","totc.n.tmax.xhigh.est",
	"totc.n.tmax.xhigh.length.est","totc.n.tmax.high.est","totc.n.tmin.high.est","totc.n.tmin.high.length.est","totc.n.tmin.xhigh.est",
	"totc.n.tmin.xhigh.length.est","totc.n.tmin.xlow.est","totc.n.tmin.low.est","totc.n.tmin.avg.est","tceq.n.dry.days.est",
	"tceq.n.tmax.xhigh.est","tceq.n.tmax.xhigh.length.est","tceq.n.tmax.high.est","tceq.n.tmin.high.est","tceq.n.tmin.high.length.est",
	"tceq.n.tmin.xhigh.est","tceq.n.tmin.xhigh.length.est","tceq.n.tmin.xlow.est","tceq.n.tmin.low.est","tceq.n.tmin.avg.est") 

predictors.c<-c("orgm","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phetol","phetm3","phetb1","phca","elco50",
		"cecph7","bdwsod","bdfi33","wg1500","wg0200","wg0033","map","total.avg.tmax","total.avg.tmin")

carbon.slope<-data.frame(slope=character(),predictor=character(),estimate=numeric(),std.err=numeric(),t.value=numeric(),
                        p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope.c){
for(pp in predictors.c){
        df<-data.one[,c(m,pp),with=FALSE]
        df<-na.omit(df)
        colnames(df)<-c("slope","property")
        df<-subset(df,is.finite(df$slope)&is.finite(df$property))
if(nrow(df)<3) next
        model<-lm(slope~property,data=df)
        sum<-summary(model)
        coef<-coef(summary(model))
if(nrow(coef)<2) next
        carbon.slope<-rbind(carbon.slope,data.frame(slope=m,predictor=pp,estimate=coef[2,1],std.err=coef[2,2],
                                t.value=coef[2,3],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}
carbon.slope$significant<-ifelse(carbon.slope$p.value<0.05,"yes","no")

write.csv(carbon.slope,"/disks/home/abigail/thesis-repository/code/data/carbon.slope.csv")


slope.n<-c("nitkjd.n.dry.days.est","nitkjd.n.prcp.event.est","nitkjd.n.prcp.mean.est","nitkjd.n.tmax.xhigh.est","nitkjd.n.tmax.xhigh.length.est",
"nitkjd.n.tmax.xlow.est","nitkjd.n.tmax.xlow.length.est","nitkjd.n.tmax.avg.est","nitkjd.n.tmin.xhigh.est","nitkjd.n.tmin.xhigh.length.est","nitkjd.n.tmin.xlow.est",
"nitkjd.n.tmin.low.est","nitkjd.n.tmin.avg.est","phetol.n.dry.days.est","phetol.n.tmax.avg.est","phetb1.n.dry.days.est","phetb1.n.prcp.event.est",
"phetb1.n.prcp.mean.est","phetb1.n.tmax.xhigh.est","phetb1.n.tmax.xhigh.length.est","phetb1.n.tmax.xlow.est","phetb1.n.tmax.xlow.length.est","phetb1.n.tmax.avg.est",
"phetb1.n.tmin.xhigh.est","phetb1.n.tmin.xhigh.length.est","phetb1.n.tmin.xlow.est","phetb1.n.tmin.low.est","phetb1.n.tmin.avg.est")

predictors.n<- c("tceq","orgm","orgc","totc","ecec","cfgr","cfvo","clay","silt","sand","phca","elco50","cecph7","bdwsod",
                    "bdfi33","wg1500","wg0200","wg0033","map","total.avg.tmax","total.avg.tmin")

nutrient.slope<-data.frame(slope=character(),predictor=character(),estimate=numeric(),std.err=numeric(),t.value=numeric(),
                        p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope.n){
for(pp in predictors.n){
        df<-data.one[,c(m,pp),with=FALSE]
        df<-na.omit(df)
        colnames(df)<-c("slope","property")
        df<-subset(df,is.finite(df$slope)&is.finite(df$property))
if(nrow(df)<3) next
        model<-lm(slope~property,data=df)
        sum<-summary(model)
        coef<-coef(summary(model))
if(nrow(coef)<2) next
        nutrient.slope<-rbind(nutrient.slope,data.frame(slope=m,predictor=pp,estimate=coef[2,1],std.err=coef[2,2],
                                t.value=coef[2,3],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}
nutrient.slope$significant<-ifelse(nutrient.slope$p.value<0.05,"yes","no")

write.csv(nutrient.slope,"/disks/home/abigail/thesis-repository/code/data/nutrient.slope.csv")


slope.t<-c("clay.n.prcp.xhigh.est","clay.n.prcp.xhigh.length.est","clay.n.prcp.high.est","clay.n.dry.days.est","clay.n.dry.length.est",
"clay.n.prcp.event.est","clay.n.tmax.xhigh.est","clay.n.tmax.xlow.est","clay.n.tmax.xlow.length.est","clay.n.tmax.low.est","clay.n.tmin.xhigh.est",
"clay.n.tmin.xhigh.length.est","clay.n.tmin.high.est","clay.n.tmin.high.length.est","clay.n.tmin.xlow.est","silt.n.prcp.xhigh.est","silt.n.prcp.xhigh.length.est",
"silt.n.prcp.high.est","silt.n.dry.days.est","silt.n.dry.length.est","silt.n.prcp.event.est",
"silt.n.tmax.xhigh.est","silt.n.tmax.xlow.est","silt.n.tmax.xlow.length.est","silt.n.tmax.low.est","silt.n.tmin.xhigh.est","silt.n.tmin.xhigh.length.est",
"silt.n.tmin.high.est","silt.n.tmin.high.length.est","silt.n.tmin.xlow.est","sand.n.prcp.xhigh.est","sand.n.prcp.xhigh.length.est","sand.n.prcp.high.est",
"sand.n.dry.days.est","sand.n.dry.length.est","sand.n.prcp.event.est","sand.n.tmax.xhigh.est","sand.n.tmax.xlow.est","sand.n.tmax.xlow.length.est",
"sand.n.tmax.low.est","sand.n.tmin.xhigh.est","sand.n.tmin.xhigh.length.est","sand.n.tmin.high.est",
"sand.n.tmin.high.length.est","sand.n.tmin.xlow.est","ecec.n.prcp.xhigh.est","ecec.n.prcp.xhigh.length.est","ecec.n.prcp.high.est","ecec.n.dry.days.est",
"ecec.n.dry.length.est","ecec.n.prcp.event.est","ecec.n.tmax.xhigh.est","ecec.n.tmax.xlow.est","ecec.n.tmax.low.est","ecec.n.tmin.xhigh.est",
"ecec.n.tmin.xhigh.length.est","ecec.n.tmin.high.est","ecec.n.tmin.high.length.est","cecph7.n.prcp.xhigh.est",
"cecph7.n.prcp.xhigh.length.est","cecph7.n.prcp.high.est","cecph7.n.dry.days.est","cecph7.n.dry.length.est","cecph7.n.prcp.event.est","cecph7.n.tmax.xhigh.est",
"cecph7.n.tmax.xlow.est","cecph7.n.tmax.xlow.length.est","cecph7.n.tmax.low.est","cecph7.n.tmin.xhigh.est","cecph7.n.tmin.xhigh.length.est","cecph7.n.tmin.high.est",
"cecph7.n.tmin.high.length.est","cecph7.n.tmin.xlow.est","elco50.n.prcp.xhigh.est","elco50.n.prcp.xhigh.length.est","elco50.n.prcp.high.est","elco50.n.dry.days.est",
"elco50.n.prcp.xhigh.est","elco50.n.prcp.xhigh.length.est","elco50.n.prcp.high.est","elco50.n.dry.days.est","elco50.n.dry.length.est","elco50.n.prcp.event.est",
"elco50.n.tmax.xhigh.est","elco50.n.tmax.xlow.est","elco50.n.tmax.xlow.length.est","elco50.n.tmax.low.est","elco50.n.tmin.xhigh.est","elco50.n.tmin.xhigh.length.est",
"elco50.n.tmin.high.est","elco50.n.tmin.high.length.est","elco50.n.tmin.xlow.est")

predictors.t<- c("tceq","orgm","orgc","totc","nitkjd","cfgr","cfvo","phetol","phetm3","phetb1","phca","elco50","cecph7","bdwsod",
                    "bdfi33","wg1500","wg0200","wg0033","map","total.avg.tmax","total.avg.tmin")

texture.slope<-data.frame(slope=character(),predictor=character(),estimate=numeric(),std.err=numeric(),t.value=numeric(),
                        p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope.t){
for(pp in predictors.t){
        df<-data.one[,c(m,pp),with=FALSE]
        df<-na.omit(df)
        colnames(df)<-c("slope","property")
        df<-subset(df,is.finite(df$slope)&is.finite(df$property))
if(nrow(df)<3) next
        model<-lm(slope~property,data=df)
        sum<-summary(model)
        coef<-coef(summary(model))
if(nrow(coef)<2) next
        texture.slope<-rbind(texture.slope,data.frame(slope=m,predictor=pp,estimate=coef[2,1],std.err=coef[2,2],
                                t.value=coef[2,3],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}
texture.slope$significant<-ifelse(texture.slope$p.value<0.05,"yes","no")

write.csv(texture.slope,"/disks/home/abigail/thesis-repository/code/data/texture.slope.csv")


data.one$biome <- as.factor(data.one$biome)

slope <- c(slope.w, slope.c, slope.n, slope.t)

anova <- data.frame(
	slope = character(),
	df = numeric(),
	sum.sqr = numeric(),
	mean.sqr = numeric(),
	f.value = numeric(),
	p.value = numeric())

for(s in slope){
	df <- data.one[, c(s, "biome"), with = FALSE]
	colnames(df) <- c("slope", "biome")
	df <- na.omit(df)
	df$slope <- as.numeric(as.character(df$slope))
	df$biome <- droplevels(as.factor(df$biome))
if(nrow(df) == 0) next
if(length(levels(df$biome)) < 2) next
if(length(unique(df$slope)) < 2) next
	model <- aov(slope ~ biome, data = df)
	a <- summary(model)[[1]]
anova <- rbind(anova, data.frame(
	slope = s,
	df = a$Df[1],
	sum.sqr = a$`Sum Sq`[1],
	mean.sqr = a$`Mean Sq`[1],
	f.value = a$`F value`[1],
	p.value = a$`Pr(>F)`[1]))}

anova$significant<-ifelse(anova$p.value<0.05,"yes","no")

write.csv(anova,"/disks/home/abigail/thesis-repository/code/data/anova.biome.csv")
