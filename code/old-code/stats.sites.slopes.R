library(data.table)
library(tidyverse)

data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

data[,c("X.2","X.1","X","HWSD2_SMU_ID","WRB2","cell_id","layer_id","organic_surface")]<-NULL

map<-read.csv("/disks/home/abigail/thesis-repository/code/data/map.csv")
avg.tmax<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmax.csv")
avg.tmin<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmin.csv")#

map<-map[,c("station_id","map.PRCP")]
colnames(map)<-c("station_id","map")

avg.tmax<-avg.tmax[,c("station_id","avg.tmax")]
colnames(avg.tmax)<-c("station_id","total.avg.tmax")

avg.tmin<-avg.tmin[,c("station_id","avg.tmin")]
colnames(avg.tmin)<-c("station_id","total.avg.tmin")

data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
	ifelse(upper_depth>=21&upper_depth<=40,"two",
	ifelse(upper_depth>=41&upper_depth<=60,"three",
	ifelse(upper_depth>=61&upper_depth<=80,"four",
	ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

data$soil.layer<-as.factor(data$soil.layer)

data.one<-subset(data,data$soil.layer=="one")

valid_var <- function(x) {
x <- x[!is.na(x)]
length(unique(x)) > 1}

site<-unique(data.one$station_id)

water<-c("wg1500","wg0200","wg0033")
water.sites<-list()

for(i in water){
result<-data.frame(station_id=character(),climate=character(),estimate=numeric(),std.err=numeric(),
               t.value=numeric(),p.value=numeric())
for(j in site){
        df<-data.one[data.one$station_id==j,c("station_id",i,"n.prcp.xhigh","n.prcp.xhigh.length","n.dry.days","n.dry.length",
                        "n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high","n.tmax.high.length")]
        colnames(df)<-c("station_id","metric","n.prcp.xhigh","n.prcp.xhigh.length","n.dry.days","n.dry.length",
                        "n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high","n.tmax.high.length")
        df<-df[!is.na(df$metric),]
if(nrow(df) < 3) next
	predictors <- names(df)[-c(1,2)]
	valid <- predictors[sapply(df[predictors], valid_var)]
if(length(valid) == 0) next
	formula_str <- paste("metric ~", paste(valid, collapse = "+"))
	model <- lm(as.formula(formula_str), data = df)
	coef<-as.data.frame(coef(summary(model)))
if(nrow(coef)==0) next
result<-rbind(result,data.frame(station_id=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
		std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
result$significant<-ifelse(result$p.value<0.05,"yes","no")
result<-result[result$climate!="(Intercept)",]
water.sites[[i]]<-result}


carbon<-c("orgc","totc","tceq")
carbon.sites<-list()

for(i in carbon){
result<-data.frame(station_id=character(),climate=character(),estimate=numeric(),std.err=numeric(),
               t.value=numeric(),p.value=numeric())
for(j in site){
        df<-data.one[data.one$station_id==j,c("station_id",i,"n.dry.days","n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high",
		"n.tmin.high","n.tmin.high.length","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")]
        colnames(df)<-c("station_id","metric","n.dry.days","n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high",
                "n.tmin.high","n.tmin.high.length","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")
        df<-df[!is.na(df$metric),]
if(nrow(df) < 3) next
        predictors <- names(df)[-c(1,2)]
        valid <- predictors[sapply(df[predictors], valid_var)]
if(length(valid) == 0) next
        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        coef<-as.data.frame(coef(summary(model)))
if(nrow(coef)==0) next
        result<-rbind(result,data.frame(station_id=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
result$significant<-ifelse(result$p.value<0.05,"yes","no")
result<-result[result$climate!="(Intercept)",]
carbon.sites[[i]]<-result}

nutrient<-c("nitkjd","phetol","phetb1")
nutrient.sites<-list()

for(i in nutrient){
result<-data.frame(station_id=character(),climate=character(),estimate=numeric(),std.err=numeric(),
               t.value=numeric(),p.value=numeric())
for(j in site){
        df<-data.one[data.one$station_id==j,c("station_id",i,"n.dry.days","n.prcp.event","n.prcp.mean","n.tmax.xhigh","n.tmax.xhigh.length",
                "n.tmax.xlow","n.tmax.xlow.length","n.tmax.avg","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")]
        colnames(df)<-c("station_id","metric","n.dry.days","n.prcp.event","n.prcp.mean","n.tmax.xhigh","n.tmax.xhigh.length",
                "n.tmax.xlow","n.tmax.xlow.length","n.tmax.avg","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")
        df<-df[!is.na(df$metric),]
if(nrow(df) < 3) next
        predictors <- names(df)[-c(1,2)]
        valid <- predictors[sapply(df[predictors], valid_var)]
if(length(valid) == 0) next
        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        coef<-as.data.frame(coef(summary(model)))
if(nrow(coef)==0) next
        result<-rbind(result,data.frame(station_id=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
result$significant<-ifelse(result$p.value<0.05,"yes","no")
result<-result[result$climate!="(Intercept)",]
nutrient.sites[[i]]<-result}


texture<-c("clay","silt","sand","ecec","cecph7","elco50")
texture.sites<-list()

for(i in texture){
result<-data.frame(station_id=character(),climate=character(),estimate=numeric(),std.err=numeric(),
               t.value=numeric(),p.value=numeric())
for(j in site){
        df<-data.one[data.one$station_id==j,c("station_id",i,"n.prcp.xhigh","n.prcp.xhigh.length","n.prcp.high","n.dry.days","n.dry.length",
		"n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.xlow.length","n.tmax.low","n.tmin.xhigh","n.tmin.xhigh.length",
		"n.tmin.high","n.tmin.high.length","n.tmin.xlow")]
        colnames(df)<-c("station_id","metric","n.prcp.xhigh","n.prcp.xhigh.length","n.prcp.high","n.dry.days","n.dry.length",
                "n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.xlow.length","n.tmax.low","n.tmin.xhigh","n.tmin.xhigh.length",
                "n.tmin.high","n.tmin.high.length","n.tmin.xlow")
        df<-df[!is.na(df$metric),]
if(nrow(df) < 3) next
        predictors <- names(df)[-c(1,2)]
        valid <- predictors[sapply(df[predictors], valid_var)]
if(length(valid) == 0) next
        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
        model <- lm(as.formula(formula_str), data = df)
        coef<-as.data.frame(coef(summary(model)))
if(nrow(coef)==0) next
        result<-rbind(result,data.frame(station_id=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
result$significant<-ifelse(result$p.value<0.05,"yes","no")
result<-result[result$climate!="(Intercept)",]
texture.sites[[i]]<-result}

water.final<-list()

climate.w<-c("n.prcp.xhigh","n.prcp.xhigh.length","n.dry.days","n.dry.length","n.tmax.xhigh",
		"n.tmax.xhigh.length","n.tmax.high","n.tmax.high.length")

for(i in water){
	df<-water.sites[[i]]
for(c in climate.w){
	name<-paste0(i,sep=".",c)
	sub<-df[df$climate==c,]
	sub<-sub[is.finite(sub$std.err),]
	water.final[[name]]<-sub
	colnames(water.final[[name]])<-c("station_id","climate",paste0(i,sep=".",c,sep=".","est"),"std.err","t.value","p.value")
}}

carbon.final<-list()

climate.c<-c("n.dry.days","n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high",
                "n.tmin.high","n.tmin.high.length","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")

for(i in carbon){
        df<-carbon.sites[[i]]
for(c in climate.c){
        name<-paste0(i,sep=".",c)
        sub<-df[df$climate==c,]
        sub<-sub[is.finite(sub$std.err),]
        carbon.final[[name]]<-sub
        colnames(carbon.final[[name]])<-c("station_id","climate",paste0(i,sep=".",c,sep=".","est"),"std.err","t.value","p.value")
}}

nutrient.final<-list()

climate.n<-c("n.dry.days","n.prcp.event","n.prcp.mean","n.tmax.xhigh","n.tmax.xhigh.length",
                "n.tmax.xlow","n.tmax.xlow.length","n.tmax.avg","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")

for(i in nutrient){
        df<-nutrient.sites[[i]]
for(c in climate.n){
        name<-paste0(i,sep=".",c)
        sub<-df[df$climate==c,]
        sub<-sub[is.finite(sub$std.err),]
        nutrient.final[[name]]<-sub
        colnames(nutrient.final[[name]])<-c("station_id","climate",paste0(i,sep=".",c,sep=".","est"),"std.err","t.value","p.value")
}}

texture.final<-list()

climate.t<-c("n.prcp.xhigh","n.prcp.xhigh.length","n.prcp.high","n.dry.days","n.dry.length",
                "n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.xlow.length","n.tmax.low","n.tmin.xhigh","n.tmin.xhigh.length",
                "n.tmin.high","n.tmin.high.length","n.tmin.xlow")

for(i in texture){
        df<-texture.sites[[i]]
for(c in climate.t){
        name<-paste0(i,sep=".",c)
        sub<-df[df$climate==c,]
        sub<-sub[is.finite(sub$std.err),]
        texture.final[[name]]<-sub
        colnames(texture.final[[name]])<-c("station_id","climate",paste0(i,sep=".",c,sep=".","est"),"std.err","t.value","p.value")
}}

sites.data<-c("water.final","carbon.final","nutrient.final","texture.final")

for(sd in sites.data){
	df<-get(sd)
for(i in names(df)){
	frame<-df[[i]]
	frame<-frame[,c(1,3)]
	data.one<-merge(data.one,frame,by=c("station_id"),all.x=TRUE)}}

data.one<-merge(data.one,map,all.x=TRUE)
data.one<-merge(data.one,avg.tmax,all.x=TRUE)
data.one<-merge(data.one,avg.tmin,all.x=TRUE)

write.csv(data.one,"/disks/home/abigail/thesis-repository/code/data/data.stats.csv")


data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

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
  "Tundra" = "tundra"
)

biome$biome <- biome_map[biome$biome_name]

data.one<-merge(data.one,biome,all.x=TRUE)

data.one<-data.one[,colSums(!is.na(data.one))> 0]

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

setDT(data.one)

valid_var <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(FALSE)

  if (is.factor(x)) {
    return(nlevels(droplevels(x)) > 1)
  } else {
    return(length(unique(x)) > 1)
  }
}

slope.w<-c("wg1500.n.prcp.xhigh.length.est","wg1500.n.dry.days.est","wg1500.n.dry.length.est","wg1500.n.tmax.xhigh.est",
"wg1500.n.tmax.xhigh.length.est","wg1500.n.tmax.high.est","wg1500.n.tmax.high.length.est","wg0200.n.prcp.xhigh.est",
"wg0200.n.prcp.xhigh.length.est","wg0200.n.dry.days.est","wg0200.n.dry.length.est","wg0200.n.tmax.xhigh.est",
"wg0033.n.prcp.xhigh.est","wg0033.n.prcp.xhigh.length.est","wg0033.n.dry.days.est","wg0033.n.dry.length.est",
"wg0033.n.tmax.xhigh.est","wg0033.n.tmax.xhigh.length.est","wg0033.n.tmax.high.est","wg0033.n.tmax.high.length.est")


predictors <- c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt",
                    "sand","phetol","phetm3","phetb1","phca","elco50","cecph7","bdwsod",
                    "bdfi33","wg0200","wg0033","map","total.avg.tmax","total.avg.tmin","biome")

valid_var <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) > 1
}

water.slope<-data.frame()

for(m in slope.w){
        valid <- predictors[sapply(data.one[,predictors,with=FALSE], valid_var)]
if (length(valid) == 0) next
        cols<-c(m,valid)
        df<-data.one[,cols,with=FALSE]
        names(df)[names(df)==m]<-"slope"
        df<-subset(df,!is.na(df$slope))
        df<-droplevels(df)
if(nrow(df)<3) next
        valid1<-valid[sapply(df[,valid,with=FALSE],valid_var)]
if(length(valid1)==0) next
        formula <- as.formula(paste("slope ~", paste(valid1, collapse = "+")))
        model <-try(lm(formula, data = df),silent=TRUE)
if (inherits(model, "try-error")) next
        sum<-summary(model)
       coef<-coef(summary(model))
if(nrow(coef)<2) next
       water.slope<-rbind(water.slope,data.frame(slope=rep(m,nrow(coef)),property=rownames(coef),slope.2=coef[,1],
                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}
water.slope$sig<-ifelse(water.slope$p.value<0.05,"yes","no")

property.n<- c("map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

nutrient.slope<-data.frame(
      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope){
for(p in property.n){
       df<-data[,c(m,"map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")]
       colnames(df)<-c("slope","map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")
if(nrow(df)<3) next
if(nrow(df)==0)next
       model<-lm(slope~bdfi33+bdwsod+tceq+orgm+orgc+totc+nitkjd+cecph7+ecec+elco50+cfgr+cfvo+clay+
        silt+sand+phca+phetb1+phetm3+phetol+map+total.tmax.avg+total.tmin.avg,data=df)
       sum<-summary(model)
       coef<-summary(model)$coefficients
if(nrow(coef)<2) next
       nutrient.slope<-rbind(nutrient.slope,data.frame(slope=rep(m,nrow(coef)),property=namerows(coef),slope.2=coef[,1],
                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}}
nutrient.slope$sig<-ifelse(nutrient.slope$p.value<0.05,"yes","no")


property.t<- c("map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

texture.slope<-data.frame(
      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope){
for(p in property.t){
       df<-data[,c(m,"map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")]
       colnames(df)<-c("slope","map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")
if(nrow(df)<3) next
if(nrow(df)==0)next
       model<-lm(slope~bdfi33+bdwsod+tceq+orgm+orgc+totc+nitkjd+cecph7+ecec+elco50+cfgr+cfvo+clay+
        silt+sand+phca+phetb1+phetm3+phetol+map+total.tmax.avg+total.tmin.avg,data=df)
       sum<-summary(model)
       coef<-summary(model)$coefficients
if(nrow(coef)<2) next
       texture.slope<-rbind(texture.slope,data.frame(slope=rep(m,nrow(coef)),property=namerows(coef),slope.2=coef[,1],
                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}}
texture.slope$sig<-ifelse(texture.slope$p.value<0.05,"yes","no")

