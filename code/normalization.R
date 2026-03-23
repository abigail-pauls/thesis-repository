library(tidyverse)
library(data.table)

final<-read.csv("/disks/home/abigail/thesis-repository/code/data/final.csv")

norm.prcp<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.prcp.csv")
norm.prcp.1<-norm.prcp[,-c(1)]

norm.tmax<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.tmax.csv") 
norm.tmax.1<-norm.tmax[,-c(1)] 
names(norm.tmax.1)[names(norm.tmax.1)=="tmax.avg"]<-"tmax.avg.u"

norm.tmin<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.tmin.csv")
norm.tmin.1<-norm.tmin[,-c(1)]
names(norm.tmin.1)[names(norm.tmin.1)=="tmin.avg"]<-"tmin.avg.u"

final<-merge(final,norm.prcp.1,all.x=TRUE)
final<-merge(final,norm.tmax.1,all.x=TRUE)
final<-merge(final,norm.tmin.1,all.x=TRUE)

check<-final

final$n.prcp.avg<-((final$prcp.avg-final$prcp.avg.u)/final$prcp.avg.sd)

final$n.prcp.mean<-((final$prcp.mean-final$prcp.mean.u)/final$prcp.mean.sd)

final$n.prcp.event<-((final$prcp.events-final$prcp.event.u)/final$prcp.event.sd)

final$n.dry.days<-((final$dry.days-final$dry.days.u)/final$dry.days.u)

final$n.prcp.xhigh<-((final$prcp.xhigh-final$prcp.xhigh.u)/final$prcp.xhigh.sd)

final$n.prcp.high<-((final$prcp.high-final$prcp.high.u)/final$prcp.xhigh.sd)

final$n.tmax.avg<-((final$tmax.avg-final$tmax.avg)/final$tmax.sd)

final$n.tmax.xhigh<-((final$tmax.xhigh-final$tmax.xhigh.u)/final$tmax.xhigh.sd)

final$n.tmax.high<-((final$tmax.high-final$tmax.high.u)/final$tmax.high.sd)

final$n.tmax.xlow<-((final$tmax.xlow-final$tmax.xlow.u)/final$tmax.xlow.sd)

final$n.tmax.low<-((final$tmax.low-final$tmax.low.u)/final$tmax.low.sd)

final$n.tmin.avg<-((final$tmin.avg-final$tmin.avg)/final$tmin.sd)

final$n.tmin.xhigh<-((final$tmin.xhigh-final$tmin.xhigh.u)/final$tmin.xhigh.sd)

final$n.tmin.high<-((final$tmin.high-final$tmin.high.u)/final$tmin.high.sd)

final$n.tmin.xlow<-((final$tmin.xlow-final$tmin.xlow.u)/final$tmin.xlow.sd)

final$n.tmin.low<-((final$tmin.low-final$tmin.low.u)/final$tmin.low.sd)

final$n.dry.length<-((final$dry.days.length-final$dry.days.length.u)/final$dry.days.length.sd)

final$n.prcp.xhigh.length<-(final$prcp.xhigh.length-final$prcp.xhigh.length.u)/final$prcp.xhigh.length.sd

final$n.prcp.high.length<-(final$prcp.high.length-final$prcp.high.length.u)/final$prcp.high.length.sd

final$n.tmax.xhigh.length<-((final$tmax.xhigh.length-final$tmax.xhigh.length.u)/final$tmax.xhigh.length.sd)

final$n.tmax.high.length<-((final$tmax.high.length-final$tmax.high.length.u)/final$tmax.high.length.sd)

final$n.tmax.xlow.length<-((final$tmax.xlow.length-final$tmax.xlow.length.u)/final$tmax.xlow.length.sd)

final$n.tmax.low.length<-((final$tmax.low.length-final$tmax.low.length.u)/final$tmax.low.length.sd)

final$n.tmin.xhigh.length<-((final$tmin.xhigh.length-final$tmin.xhigh.length.u)/final$tmin.xhigh.length.sd)

final$n.tmin.high.length<-((final$tmin.high.length-final$tmin.high.length.u)/final$tmin.high.length.sd)

final$n.tmin.xlow.length<-((final$tmin.xlow.length-final$tmin.xlow.length.u)/final$tmin.xlow.length.sd)

final$n.tmin.low.length<-((final$tmin.low.length-final$tmin.low.length.u)/final$tmin.low.length.sd)

write.csv(final,"/disks/home/abigail/thesis-repository/code/data/data.csv")

final<-final[,-c(92:153)]

write.csv(final,"/disks/home/abigail/thesis-repository/code/data/data.final.csv")
