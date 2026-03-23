library(tidyverse)
library(lubridate)
library(data.table)
library(lme4)
library(car)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.csv")

#data<-final

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

#model.list<-c(16:28)
#model.out<-data.frame(variable=character(),Fvalue=numeric(),pvalue=numeric())
#for(i in model.list){
#	one<-data[,c(i,which(names(data)=="soil.layer"))]
#	two<-subset(one,!is.na(one[,1])&one$soil.layer!="six")
#if(length(unique(two[,2]))>1){
#	model<-aov(two[,1]~two$soil.layer,)
#	three<-summary(model)[[1]]
#	model.out<-rbind(model.out,data.frame(variable=names(data)[i],Fvalue=three$`F value`[1],pvalue=three$`Pr(>F)`[1]))}}
#model.out<-as.data.frame(model.out)
#names(model.out)<-c("variable","Fvalue","pvalue")

#model.out$sig<-with(model.out,ifelse(pvalue<0.05,"yes","no"))

#data$decade<-with(data,ifelse(YEAR<=1983&YEAR>=1973,"seventies",
 #  ifelse(YEAR>1983&YEAR<=1993,"eighties",
#ifelse(YEAR>1993&YEAR<=2003,"ninties",
#ifelse(YEAR>2003&YEAR<=2013,"oughts",
#ifelse(YEAR>2013&YEAR<=2023,"tens","NA"))))))

#decade<-as.factor(data$decade)

data.one<-subset(data,data$soil.layer=="one")
#data.two<-subset(data,data$soil.layer=="two")
#data.three<-subset(data,data$soil.layer=="three")
#data.four<-subset(data,data$soil.layer=="four")
#data.five<-subset(data,data$soil.layer=="five")
#data.six<-subset(data,data$soil.layer=="six")

#data.one$station_id<-as.character(data.one$station_id)

#site<-unique(data.one$station_id)

#orgc.xhigh.prcp<-data.frame(station_id=character(),slope=numeric(),std.err=numeric(),
#	t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())
#
#for(j in site){
#	a<-subset(data.one,data.one$station_id==j)
#	a<-na.omit(a[,c("orgc","n.prcp.xhigh")])
#if(nrow(a)==0) next
#if(nrow(a) < 3) next
#if(length(unique(a$n.prcp.xhigh)) < 2) next
#	model<-lm(orgc~n.prcp.xhigh,data=a)
#	b<-summary(model)$coefficients
#	c<-summary(model)
#if(!"n.prcp.xhigh" %in% rownames(b)) next
#	orgc.xhigh.prcp<-rbind(orgc.xhigh.prcp,data.frame(
#		station_id=j,
#		slope=b[2,1],
#		std.err=b[2,2],
#		t.value=b[2,3],
#		p.value=b[2,4],
#		r2=c$r.squared,
#		r2.adj=c$adj.r.squared))}

#orgc.xhigh.prcp$sig<-ifelse(orgc.xhigh.prcp$r2<0.05,"yes","no")

#write.csv(orgc.xhigh.prcp,"/disks/home/abigail/thesis-repository/code/data/orgc.xhigh.prcp.csv")

#decade_orgc_n.prcp.xhigh<-data.frame(decade=character(),intercept=numeric(),n.xhigh.prcp=numeric(),p.value=numeric())

#decades<-as.vector(unique(data.one$decade))

#for(h in decades){
#	d<-data.one[which(data.one$decade==h),]
#	d<-na.omit(d[,c("station_id","orgc","n.prcp.xhigh")])
#if(nrow(d)==0) next
#if(nrow(d)<3) next
#if(length(unique(d$n.prcp.xhigh))<2) next
#	model<-lmer(orgc~n.prcp.xhigh+(1|station_id),data=d)
#	test<-Anova(model,type="II")
#	decade_orgc_n.prcp.xhigh<-rbind(decade_orgc_n.prcp.xhigh,data.frame(
#		decade=h,
#		intercept=re$vcov[2,1],
#		n.xhigh.prcp=re$vcov[2,2],
#		p.value=test[,3]))}

#soil.attributes<-c(17:56)
#climate.attributes<-c(93:120)

#output<-data.frame(soil.attribute=character(),estimate=numeric(),t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(a in soil.attributes){
#	soil<-paste0(names(data.one)[[a]])
#	model<-lm(soil~n.prcp.xhigh,data=data.one)
#	b<-summary(model)
#	c<-b$coefficients
#	output<-rbind(output,data.frame(
#		soil.attribute=names(data.one)[[a]],
#		estimate=c[2,1],
#		t.value=c[2,3],
#		p.value=c[2,4],
#		r2=b$r.squared,
#		r2.adj=c$adj.r.squared))}

#output<-data.frame(climate.attribute=character(),soil.attribute=character(),varStation=numeric(),varResid=numeric(),p.value=numeric())

#soil.attributes<-c(17:56)
#climate.attributes<-c(93:120)

#for(aa in climate.attributes){
#for(ii in soil.attributes){
#	climate<-paste0(names(data.one)[aa])
#	soil<-paste0(names(data.one)[ii])
#	df<-data.one[,c("station_id",climate,soil)]
#	df<-na.omit(df) 
#if (nrow(df) < 3) next 
#if (length(unique(df[[soil]])) < 2) next 
#if (length(unique(df$station_id)) < 2){
#	model<-lm(as.formula(paste(soil, "~", climate)),data = df)
#}
#else{
#	model <- lmer(as.formula(paste(soil, "~", climate, "+ (1 | station_id)")),data = df)}
#	re<-as.data.frame(summary(model)$varcor)
#	test<-Anova(model,type="II")
#	output<-rbind(output,data.frame(
#		climate.attribute=climate,
#		soil.attribute=soil,
#		varStation=re[1,4],
#		varResid=re[2,4],
#		p.value=test[1,3]))}}

#library(dplyr)
#library(purrr)
#library(tidyr)
#library(lme4)
#library(car)

#soil.attributes <- 17:57
#climate.attributes <- 93:120

# Create all combinations of variables
#var_grid <- expand_grid(
#  climate = names(data.one)[climate.attributes],
#  soil = names(data.one)[soil.attributes]
#)

#results <- var_grid %>%
 # mutate(
  #  model_output = map2(climate, soil, ~{
#
 #     df <- data.one %>%
  #      select(station_id, all_of(.x), all_of(.y)) %>%
   #     drop_na()

      # Basic checks
    #  if (nrow(df) < 3) return(NULL)
     # if (length(unique(df[[.y]])) < 2) return(NULL)
      #if (length(unique(df$station_id)) < 2) return(NULL)

      # Fit model safely
     # model <- tryCatch(
      #  lmer(
       #   as.formula(paste(.y, "~", .x, "+ (1 | station_id)")),
        #  data = df
        #),
        #error = function(e) return(NULL)
      #)

#      if (is.null(model)) return(NULL)

      # Skip singular fits
 #     if (isSingular(model)) return(NULL)

      # Extract variance components
  #    vc <- as.data.frame(VarCorr(model))

      # Extract p-value
   #   test <- tryCatch(
    #    Anova(model, type = "II"),
     #   error = function(e) return(NULL)
      #)

#      if (is.null(test)) return(NULL)

#      tibble(
 #       varStation = vc$vcov[vc$grp == "station_id"],
  #      varResid = vc$vcov[vc$grp == "Residual"],
   #     p.value = test[1, "Pr(>Chisq)"]
    #  )
    #})
  #) %>%
  #unnest(model_output) %>%
  #rename(
   # climate.attribute = climate,
    #soil.attribute = soil
 # )


# "n.prcp.avg"
# [94] "n.prcp.mean"         "n.prcp.events"       "n.dry.days"
# [97] "n.prcp.xhigh"        "n.prcp.high"         "n.dry.days.length"
#[100] "n.prcp.high.length"  "n.prcp.xhigh.length" "n.tmax.avg"
#[103] "n.tmax.xhigh"        "n.tmax.high"         "n.tmax.low"
#[106] "n.tmax.xlow"         "n.tmax.xhigh.length" "n.tmax.high.length"
#[109] "n.tmax.low.length"   "n.tmax.xlow.length"  "n.tmin.avg"
#[112] "n.tmin.xhigh"        "n.tmin.high"         "n.tmin.low"
#[115] "n.tmin.xlow"         "n.tmin.xhigh.length" "n.tmin.high.length"
#[118] "n.tmin.low.length"   "n.tmin.xlow.length"  "n.prcp.event"

#for(a in soil.attributes){
 #      soil<-paste0(names(data.one)[[a]])
  #     model<-lm(soil~n.prcp.xhigh,data=data.one)
   #    b<-summary(model)
    #   c<-b$coefficients
     #  output<-rbind(output,data.frame(
      #         soil.attribute=names(data.one)[[a]],
       #        estimate=c[2,1],
        #       t.value=c[2,3],
         #      p.value=c[2,4],
          #     r2=b$r.squared,
           #    r2.adj=c$adj.r.squared))}

