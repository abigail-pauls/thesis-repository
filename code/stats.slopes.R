data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

#slope<-c("slope.wg1500.n.prcp.xhigh","slope.wg1500.n.dry.days","slope.wg1500.n.prcp.event","slope.wg1500.n.tmax.xhigh",
#       "slope.wg1500.n.tmin.xhigh","slope.wg1500.n.tmax.xlow","slope.wg1500.n.tmin.xlow","slope.wg1500.n.tmax.avg","slope.wg1500.n.tmin.avg")
 #      "slope.wg0200.n.prcp.xhigh","slope.wg0200.n.dry.days","slope.wg0200.n.prcp.event","slope.wg0200.n.tmax.xhigh",
  #    "slope.wg0200.n.tmin.xhigh","slope.wg0200.n.tmax.xlow","slope.wg0200.n.tmin.xlow","slope.wg0200.n.tmax.avg","slope.wg0200.n.tmin.avg",
#"slope.wg0033.n.prcp.xhigh","slope.wg0033.n.dry.days","slope.wg0033.n.prcp.event","slope.wg0033.n.tmax.xhigh",
 #     "slope.wg0033.n.tmin.xhigh","slope.wg0033.n.tmax.xlow","slope.wg0033.n.tmin.xlow","slope.wg0033.n.tmax.avg","slope.wg0033.n.tmin.avg")

#property<-c("map","total.avg.tmax","total.avg.tmin","clay","sand","silt","ecec","bdwsod","tceq","orgc","cfgr","cfvo")

#water.slope<-data.frame(
#      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(rr in slope){
#for(ww in property){
#       b<-data.1[,c(rr,ww)]
#       colnames(b)<-c("x","y")
#if(nrow(b)<3) next
#       model<-lm(x~y,data=b)
#       sum<-summary(model)
#       coef<-summary(model)$coefficients
#if(nrow(coef)<2) next
#       water.slope<-rbind(water.slope,data.frame(slope=rr,property=ww,slope.2=coef[2,1],p.value=coef[2,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}

#water.slope$sig<-ifelse(water.slope$p.value<0.05,"yes","no")

