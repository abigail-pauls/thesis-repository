#site<-unique(data.one$station_id)

#water<-c("wg1500","wg0200","wg0033")

#water.sites<-data.frame(water=character(),climate=character(),station_id=character(),slope=numeric(),std.err=numeric(),t.value=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(i in water){
#for(h in climate){
#for(j in site){
#	b<-data.one[data.one$station_id==j,c("station_id",i,h)]
#	colnames(b)<-c("station_id","x","y")
#	b$x<-as.numeric(b$x)
#	b$y<-as.numeric(b$y)
#	b <- b[is.finite(b$x) & is.finite(b$y), ]
#if(nrow(b)==0) next
#if(nrow(b) < 3) next
 #      model<-lm(x~y,data=b)
  #     c<-summary(model)$coefficients
   #    d<-summary(model)
    #   if(nrow(c)<2|nrow(b)==0|nrow(b)<3){
#		water.sites<-rbind(water.sites,data.frame(water=i,climate=h,station_id=j,slope="NA",std.err="NA",t.value="NA",
#			p.value="NA",r2="NA",r2.adj="NA"))}
#else{water.sites<-rbind(water.sites,data.frame(water=i,climate=h,station_id=j,slope=c[2,1],std.err=c[2,2],t.value=c[2,3],
#			p.value=c[2,4],r2=d$r.squared,r2.adj=d$adj.r.squared))}}}}

#water.sites$sig<-ifelse(water.sites$p.value<0.05,"yes","no")

#wg1500.prcp.xhigh<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.prcp.xhigh"))
#wg1500.prcp.dry.days<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.dry.days"))
#wg1500.prcp.event<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.prcp.event"))
#wg1500.tmax.xhigh<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmax.xhigh"))
#wg1500.tmax.xlow<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmax.xlow"))
#wg1500.tmax.avg<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmax.avg"))
#wg1500.tmin.xhigh<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmin.xhigh"))
#wg1500.tmin.xlow<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmin.xlow"))
#wg1500.tmin.avg<-with(water.sites,subset(water.sites,water=="wg1500"&climate=="n.tmin.avg"))
#wg0200.prcp.xhigh<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.prcp.xhigh"))
#wg0200.prcp.dry.days<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.dry.days"))
#wg0200.prcp.event<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.prcp.event"))
#wg0200.tmax.xhigh<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmax.xhigh"))
#wg0200.tmax.xlow<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmax.xlow"))
#wg0200.tmax.avg<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmax.avg"))
#wg0200.tmin.xhigh<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmin.xhigh"))
#wg0200.tmin.xlow<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmin.xlow"))
#wg0200.tmin.avg<-with(water.sites,subset(water.sites,water=="wg0200"&climate=="n.tmin.avg"))
#wg0033.prcp.xhigh<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.prcp.xhigh"))
#wg0033.prcp.dry.days<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.dry.days"))
#wg0033.prcp.event<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.prcp.event"))
#wg0033.tmax.xhigh<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmax.xhigh"))
#wg0033.tmax.xlow<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmax.xlow"))
#wg0033.tmax.avg<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmax.avg"))
#wg0033.tmin.xhigh<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmin.xhigh"))
#wg0033.tmin.xlow<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmin.xlow"))
#wg0033.tmin.avg<-with(water.sites,subset(water.sites,water=="wg0033"&climate=="n.tmin.avg"))

#colnames(wg1500.prcp.xhigh)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#	"r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.prcp.dry.days)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.prcp.event)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmax.xhigh)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmax.xlow)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmax.avg)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmin.xhigh)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmin.xlow)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg1500.tmin.avg)<-c("water.wg1500","climate.wg1500","station_id","slope.wg1500","std.err.wg1500","t.value.wg1500","p.value.wg1500",
#        "r2.wg1500","adj.r2.wg1500","sig.wg1500")
#colnames(wg0200.prcp.xhigh)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.prcp.dry.days)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.prcp.event)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmax.xhigh)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmax.xlow)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmax.avg)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmin.xhigh)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmin.xlow)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0200.tmin.avg)<-c("water.wg200","climate.wg200","station_id","slope.wg200","std.err.wg200","t.value.wg200","p.value.wg200",
#        "r2.wg200","adj.r2.wg200","sig.wg200")
#colnames(wg0033.prcp.xhigh)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.prcp.dry.days)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.prcp.event)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmax.xhigh)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmax.xlow)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmax.avg)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmin.xhigh)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmin.xlow)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")
#colnames(wg0033.tmin.avg)<-c("water.wg33","climate.wg33","station_id","slope.wg33","std.err.wg33","t.value.wg33","p.value.wg33",
#        "r2.wg33","adj.r2.wg33","sig.wg33")

#one<-data.one[,c(2,4,18:59)]

#two<-merge(one,wg1500.prcp.xhigh,all.x=TRUE)
#two<-merge(two,wg1500.prcp.dry.days,all.x=TRUE)
#two<-merge(two,wg1500.prcp.event,all.x=TRUE)
#two<-merge(two,wg1500.tmax.xhigh,all.x=TRUE)
#two<-merge(two,wg1500.tmax.xlow,all.x=TRUE)
#two<-merge(two,wg1500.tmax.avg,all.x=TRUE)
#two<-merge(two,wg1500.tmin.xhigh,all.x=TRUE)
#two<-merge(two,wg1500.tmin.xlow,all.x=TRUE)
#two<-merge(two,wg1500.tmin.avg,all.x=TRUE)
#two<-merge(two,wg0200.prcp.xhigh,all.x=TRUE)
#two<-merge(two,wg0200.prcp.dry.days,all.x=TRUE)
#two<-merge(two,wg0200.prcp.event,all.x=TRUE)
#two<-merge(two,wg0200.tmax.xhigh,all.x=TRUE)
#two<-merge(two,wg0200.tmax.xlow,all.x=TRUE)
#two<-merge(two,wg0200.tmax.avg,all.x=TRUE)
#two<-merge(two,wg0200.tmin.xhigh,all.x=TRUE)
#two<-merge(two,wg0200.tmin.xlow,all.x=TRUE)
#two<-merge(two,wg0200.tmin.avg,all.x=TRUE)
#two<-merge(two,wg0033.prcp.xhigh,all.x=TRUE)
#two<-merge(two,wg0033.prcp.dry.days,all.x=TRUE)
#two<-merge(two,wg0033.prcp.event,all.x=TRUE)
#two<-merge(two,wg0033.tmax.xhigh,all.x=TRUE)
#two<-merge(two,wg0033.tmax.xlow,all.x=TRUE)
#two<-merge(two,wg0033.tmax.avg,all.x=TRUE)
#two<-merge(two,wg0033.tmin.xhigh,all.x=TRUE)
#two<-merge(two,wg0033.tmin.xlow,all.x=TRUE)
#two<-merge(two,wg0033.tmin.avg,all.x=TRUE)

#three<-with(two,subset(two,!is.na(slope.wg1500)|!is.na(slope.wg200)|!is.na(slope.wg33)))

model<-lm(slope.wg1500~clay,data=three)

