#library(data.table)
#library(tidyverse)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.final.csv")

#data[,c("X.2","X.1","X","HWSD2_SMU_ID","WRB2","cell_id","layer_id","organic_surface")]<-NULL

#map<-read.csv("/disks/home/abigail/thesis-repository/code/data/map.csv")
#avg.tmax<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmax.csv")
#avg.tmin<-read.csv("/disks/home/abigail/thesis-repository/code/data/avg.tmin.csv")

#map<-map[,c("station_id","map.PRCP")]
#colnames(map)<-c("station_id","map")

#avg.tmax<-avg.tmax[,c("station_id","avg.tmax")]
#colnames(avg.tmax)<-c("station_id","total.avg.tmax")

#avg.tmin<-avg.tmin[,c("station_id","avg.tmin")]
#colnames(avg.tmin)<-c("station_id","total.avg.tmin")

#data$soil.layer<-with(data,ifelse(upper_depth<=20,"one",
#	ifelse(upper_depth>=21&upper_depth<=40,"two",
#	ifelse(upper_depth>=41&upper_depth<=60,"three",
#	ifelse(upper_depth>=61&upper_depth<=80,"four",
#	ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

#data$soil.layer<-as.factor(data$soil.layer)

#data.one<-subset(data,data$soil.layer=="one")

#valid_var <- function(x) {
#  x <- x[!is.na(x)]
#  length(unique(x)) > 1
#}

#site<-unique(data.one$station_id)

#water<-c("wg1500","wg0200","wg0033")
#water.sites<-list()

#for(i in water){
#result<-data.frame(site=character(),climate=character(),estimate=numeric(),std.err=numeric(),
#               t.value=numeric(),p.value=numeric())
#for(j in site){
#        df<-data.one[data.one$station_id==j,c("station_id",i,"n.prcp.xhigh","n.prcp.xhigh.length","n.dry.days","n.dry.length",
#                        "n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high","n.tmax.high.length")]
#        colnames(df)<-c("station_id","metric","n.prcp.xhigh","n.prcp.xhigh.length","n.dry.days","n.dry.length",
#                        "n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high","n.tmax.high.length")
#        df<-df[!is.na(df$metric),]
#if(nrow(df) < 3) next
#	predictors <- names(df)[-c(1,2)]
#	valid <- predictors[sapply(df[predictors], valid_var)]
#if(length(valid) == 0) next
#	formula_str <- paste("metric ~", paste(valid, collapse = "+"))
#	model <- lm(as.formula(formula_str), data = df)
#	coef<-as.data.frame(coef(summary(model)))
#if(nrow(coef)==0) next
#	result<-rbind(result,data.frame(site=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
#		std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
#result$significant<-ifelse(result$p.value<0.05,"yes","no")
#result<-result[result$climate!="(Intercept)",]
#water.sites[[i]]<-result}
#colnames(water.sites[[i]])<-c("station_id",paste0(i,sep=".","climate"),paste0(i,sep=".","estimate"),
#		paste0(i,sep=".","std.err"),paste0(i,sep=".","t.value"),paste0(i,sep=".","p.value"),paste0(i,sep=".","sig"))


#carbon<-c("orgc","totc","tceq")
#carbon.sites<-list()

#for(i in carbon){
#result<-data.frame(site=character(),climate=character(),estimate=numeric(),std.err=numeric(),
#               t.value=numeric(),p.value=numeric())
#for(j in site){
#        df<-data.one[data.one$station_id==j,c("station_id",i,"n.dry.days","n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high",
#		"n.tmin.high","n.tmin.high.length","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")]
#        colnames(df)<-c("station_id","metric","n.dry.days","n.tmax.xhigh","n.tmax.xhigh.length","n.tmax.high",
#                "n.tmin.high","n.tmin.high.length","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")
#        df<-df[!is.na(df$metric),]
#if(nrow(df) < 3) next
#        predictors <- names(df)[-c(1,2)]
#        valid <- predictors[sapply(df[predictors], valid_var)]
#if(length(valid) == 0) next
#        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
#        model <- lm(as.formula(formula_str), data = df)
#        coef<-as.data.frame(coef(summary(model)))
#if(nrow(coef)==0) next
#        result<-rbind(result,data.frame(site=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
#                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
#result$significant<-ifelse(result$p.value<0.05,"yes","no")
#result<-result[result$climate!="(Intercept)",]
#carbon.sites[[i]]<-result
#colnames(carbon.sites[[i]])<-c("station_id",paste0(i,sep=".","climate"),paste0(i,sep=".","estimate"),
#                paste0(i,sep=".","std.err"),paste0(i,sep=".","t.value"),paste0(i,sep=".","p.value"),paste0(i,sep=".","sig"))}

#nutrient<-c("nitkjd","phetol","phetb1")
#nutrient.sites<-list()

#for(i in nutrient){
#result<-data.frame(site=character(),climate=character(),estimate=numeric(),std.err=numeric(),
#               t.value=numeric(),p.value=numeric())
#for(j in site){
#        df<-data.one[data.one$station_id==j,c("station_id",i,"n.dry.days","n.prcp.event","n.prcp.mean","n.tmax.xhigh","n.tmax.xhigh.length",
#                "n.tmax.xlow","n.tmax.xlow.length","n.tmax.avg","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")]
#        colnames(df)<-c("station_id","metric","n.dry.days","n.prcp.event","n.prcp.mean","n.tmax.xhigh","n.tmax.xhigh.length",
#                "n.tmax.xlow","n.tmax.xlow.length","n.tmax.avg","n.tmin.xhigh","n.tmin.xhigh.length","n.tmin.xlow","n.tmin.low","n.tmin.avg")
#        df<-df[!is.na(df$metric),]
#if(nrow(df) < 3) next
#        predictors <- names(df)[-c(1,2)]
#        valid <- predictors[sapply(df[predictors], valid_var)]
#if(length(valid) == 0) next
#        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
#        model <- lm(as.formula(formula_str), data = df)
#        coef<-as.data.frame(coef(summary(model)))
#if(nrow(coef)==0) next
#        result<-rbind(result,data.frame(site=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
#                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
#result$significant<-ifelse(result$p.value<0.05,"yes","no")
#result<-result[result$climate!="(Intercept)",]
#nutrient.sites[[i]]<-result
#colnames(nutrient.sites[[i]])<-c("station_id",paste0(i,sep=".","climate"),paste0(i,sep=".","estimate"),
#                paste0(i,sep=".","std.err"),paste0(i,sep=".","t.value"),paste0(i,sep=".","p.value"),paste0(i,sep=".","sig"))}


#texture<-c("clay","silt","sand","ecec","cecph7","elco50")
#texture.sites<-list()

#for(i in texture){
#result<-data.frame(site=character(),climate=character(),estimate=numeric(),std.err=numeric(),
#               t.value=numeric(),p.value=numeric())
#for(j in site){
#        df<-data.one[data.one$station_id==j,c("station_id",i,"n.prcp.xhigh","n.prcp.xhigh.length","n.prcp.high","n.dry.days","n.dry.length",#
#		"n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.xlow.length","n.tmax.low","n.tmin.xhigh","n.tmin.xhigh.length",
#		"n.tmin.high","n.tmin.high.length","n.tmin.xlow")]
#        colnames(df)<-c("station_id","metric","n.prcp.xhigh","n.prcp.xhigh.length","n.prcp.high","n.dry.days","n.dry.length",
#                "n.prcp.event","n.tmax.xhigh","n.tmax.xlow","n.tmax.xlow.length","n.tmax.low","n.tmin.xhigh","n.tmin.xhigh.length",
#                "n.tmin.high","n.tmin.high.length","n.tmin.xlow")
#        df<-df[!is.na(df$metric),]
#if(nrow(df) < 3) next
#        predictors <- names(df)[-c(1,2)]
#        valid <- predictors[sapply(df[predictors], valid_var)]
#if(length(valid) == 0) next
#        formula_str <- paste("metric ~", paste(valid, collapse = "+"))
#        model <- lm(as.formula(formula_str), data = df)
#        coef<-as.data.frame(coef(summary(model)))
#if(nrow(coef)==0) next
#        result<-rbind(result,data.frame(site=rep(j,nrow(coef)),climate=rownames(coef),estimate=coef$Estimate,
#                std.err=coef$`Std. Error`,t.value=coef$`t value`,p.value=coef$`Pr(>|t|)`))}
#result$significant<-ifelse(result$p.value<0.05,"yes","no")
#result<-result[result$climate!="(Intercept)",]
#texture.sites[[i]]<-result
#colnames(texture.sites[[i]])<-c("station_id",paste0(i,sep=".","climate"),paste0(i,sep=".","estimate"),
#                paste0(i,sep=".","std.err"),paste0(i,sep=".","t.value"),paste0(i,sep=".","p.value"),paste0(i,sep=".","sig"))}

for(g in 1:3){
	df1<-water.sites[[g]]
	df1<-df1[,c(1:3)]
	data<-merge(data,df1,all.x=TRUE)}
#	df2<-carbon.sites[[g]]
#	df2<-df2[,c(1:3)]
#	data<-merge(data,df2,all.x=TRUE)
#	df3<-nutrient.sites[[g]]
#	df3<-df3[,c(1:3)]
#	data<-merge(data,df3,all.x=TRUE)}

#for(v in 1:6){
#	df4<-texture.sites[[v]]
#	df4<-df4[,c(1:3)]
#	data<-merge(data,df4,all.x=TRUE)}

#data<-data %>% filter(station_id %in% site)

#data<-merge(data,map,all.x=TRUE)
#data<-merge(data,avg.tmax,all.x=TRUE)
#data<-merge(data,avg.tmin,all.x=TRUE)

#write.csv(data,"/disks/home/abigail/thesis-repsitory/code/data/data.stats.csv")

#data<-merge(data,biomes,all.x=TRUE)

