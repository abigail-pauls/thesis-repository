final<-final %>% dplyr::mutate(n.prcp.avg = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.mean = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.events = NA_real_)
final<-final %>% dplyr::mutate(n.dry.days = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.xhigh = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.high = NA_real_)
final<-final %>% dplyr::mutate(n.dry.days.length = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.high.length = NA_real_)
final<-final %>% dplyr::mutate(n.prcp.xhigh.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.avg = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.xhigh = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.high = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.low = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.xlow = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.xhigh.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.high.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.low.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmax.xlow.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.avg = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.xhigh = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.high = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.low = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.xlow = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.xhigh.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.high.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.low.length = NA_real_)
final<-final %>% dplyr::mutate(n.tmin.xlow.length = NA_real_)

for(i in 1:nrow(final)){
a<-final[i,"station_id"]
	final$n.prcp.avg[i]<-(final$prcp.avg[i]-norm.prcp[station_id==a,"prcp.avg.u"])/norm.prcp[station_id==a,"prcp.avg.sd"]
	final$n.prcp.mean[i]<-(final$prcp.mean[i]-norm.prcp[station_id==a,"prcp.mean.u"])/norm.prcp[station_id==a,"prcp.mean.sd"]
	final$n.prcp.event[i]<-(final$prcp.event[i]-norm.prcp[station_id==a,"prcp.event.u"])/norm.prcp[station_id==a,"prcp.event.sd"]
	final$n.dry.days[i]<-(final$dry.days[i]-norm.prcp[station_id==a,"dry.days.u"])/norm.prcp[station_id==a,"dry.days.u"]
	final$n.prcp.xhigh[i]<-(final$prcp.xhigh[i]-norm.prcp[station_id==a,"prcp.xhigh.u"])/norm.prcp[station_id==a,"prcp.xhigh.sd"]
	final$n.prcp.high[i]<-(final$prcp.high[i]-norm.prcp[station_id==a,"prcp.high.u"])/norm.prcp[station_id==a,"prcp.xhigh.sd"]
	final$n.tmax.avg[i]<-(final$tmax.avg[i]-norm.tmax[station_id==a,"tmax.avg"])/norm.tmax[station_id,"tmax.sd"]
	final$n.tmax.xhigh[i]<-(final$tmax.xhigh[i]-norm.tmax[station_id==a,"tmax.xhigh.u"])/norm.tmax[station_id==a,"tmax.xhigh.sd"]
	final$n.tmax.high[i]<-(final$tmax.high[i]-norm.tmax[station_id==a,"tmax.high.u"])/norm.tmax[station_id==a,"tmax.high.sd"]
	final$n.tmax.xlow[i]<-(final$tmax.xlow[i]-norm.tmax[station_id==a,"tmax.xlow.u"])/norm.tmax[station_id==a,"tmax.xlow.sd"]
	final$n.tmax.low[i]<-(final$tmax.low[i]-norm.tmax[station_id==a,"tmax.low.u"])/norm.tmax[station_id==a,"tmax.low.sd"]
	final$n.tmin.avg[i]<-(final$tmin.avg[i]-norm.tmin[station_id==a,"tmin.avg"])/norm.tmin[station_id==a,"tmin.sd"]
	final$n.tmin.xhigh[i]<-(final$tmin.xhigh[i]-norm.tmin[station_id==a,"tmin.xhigh.u"])/norm.tmin[station_id==a,"tmin.xhigh.sd"]
	final$n.tmin.high[i]<-(final$tmin.high[i]-norm.tmin[station_id==a,"tmin.high.u"])/norm.tmin[station_id==a,"tmin.high.sd"]
	final$n.tmin.xlow[i]<-(final$tmin.xlow[i]-norm.tmin[station_id==a,"tmin.xlow.u"])/norm.tmin[station_id==a,"tmin.xlow.sd"]
	final$n.tmin.low[i]<-(final$tmin.low[i]-norm.tmin[station_id==a,"tmin.low.u"])/norm.tmin[station_id==a,"tmin.low.sd"]
	final$n.dry.length[i]<-(final$dry.days.length[i]-norm.prcp[station_id==a,"dry.days.length.u"])/norm.prcp[station_id==a,"dry.days.length.sd"]
	final$n.tmax.xhigh.length[i]<-(final$tmax.xhigh.length[i]-norm.tmax[station_id==a,"tmax.xhigh.length.u"])/norm.tmax[station_id==a,"tmax.xhigh.length.sd"]
	final$n.tmax.high.length[i]<-(final$tmax.high.length[i]-norm.tmax[station_id==a,"tmax.high.length.u"])/norm.tmax[station_id==a."tmax.high.length.sd"]
	final$n.tmax.xlow.length[i]<-(final$tmax.xlow.length[i]-norm.tmax[station_id==a,"tmax.xlow.length.u"])/norm.tmax[station_id==a,"tmax.xlow.length.sd"]
	final$n.tmax.low.length[i]<-(final$tmax.low.length[i]-norm.tmax[station_id==a,"tmax.low.length.u"])/norm.tmax[station_id==a,"tmax.low.length.sd"]
	final$n.tmin.xhigh.length[i]<-(final$tmin.xhigh.length[i]-norm.tmin[station_id==a,"tmin.xhigh.length.u"])/norm.tmin[station_id==a,"tmin.xhigh.length.sd"]
	final$n.tmin.high.length[i]<-(final$tmin.high.length[i]-norm.tmin[station_id==a,"tmin.high.length.u"])/norm.tmin[station_id==a,"tmin.high.length.sd"]
	final$n.tmin.xlow.length[i]<-(final$tmin.xlow.length[i]-norm.tmin[station_id==a,"tmin.xlow.length.u"])/norm.tmin[station_id==a,"tmin.xlow.length.sd"]
	final$n.tmin.low.length[i]<-(final$tmin.low.length[i]-norm.tmin[station_id==a,"tmin.low.length.u"])/norm.tmin[station_id==a."tmin.low.length.sd"]
}

