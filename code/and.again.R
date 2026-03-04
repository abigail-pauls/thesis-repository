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
       final$n.prcp.avg[i]<-(final$prcp.avg[i]-norm.prcp$prcp.avg[i])/norm.prcp$prcp.avg.sd[i]
       final$n.prcp.mean[i]<-(final$prcp.mean[i]-norm.prcp$prcp.mean[i])/norm.prcp$prcp.mean.sd[i]
       final$n.prcp.event[i]<-(final$prcp.event[i]-norm.prcp$prcp.event.u[i])/norm.prcp$prcp.event.sd[i]
       final$n.dry.days[i]<-(final$dry.days[i]-norm.prcp$dry.days.u[i])/norm.prcp$dry.days.u[i]
       final$n.prcp.xhigh[i]<-(final$prcp.xhigh[i]-norm.ptcp$prcp.xhigh.u[i])/norm.prcp$prcp.xhigh.sd[i]
       final$n.prcp.high[i]<-(final$prcp.high[i]-norm.prcp$prcp.high.u[i])/norm.prcp$prcp.xhigh.sd[i]
       final$n.tmax.avg[i]<-(final$tmax.avg[i]-norm.tmax$tmax.avg[i])/norm.tmax$tmax.avg.sd[i]
       final$n.tmax.xhigh[i]<-(final$tmax.xhigh[i]-norm.tmax$tmax.xhigh.u[i])/norm.tmax$tmax.xhigh.sd[i]
       final$n.tmax.high[i]<-(final$tmax.high[i]-norm.tmax$tmax.high.u[i])/norm.tmax$tmax.high.sd[i]
       final$n.tmax.xlow[i]<-(final$tmax.xlow[i]-norm.tmax$tmax.xlow.u[i])/norm.tmax$tmax.xlow.sd[i]
       final$n.tmax.low[i]<-(final$tmax.low[i]-norm.tmax$tmax.low.u[i])/norm.tmax$tmax.low.sd[i]
       final$n.tmin.avg[i]<-(final$tmin.avg[i]-norm.tmin$tmin.avg[i])/norm.tmin$tmin.avg.sd[i]
       final$n.tmin.xhigh[i]<-(final$tmin.xhigh[i]-norm.tmin$tmin.xhigh.u[i])/norm.tmin$tmin.xhigh.sd[i]
       final$n.tmin.high[i]<-(final$tmin.high[i]-norm.tmin$tmin.high.u[i])/norm.tmin$tmin.high.sd[i]
       final$n.tmin.xlow[i]<-(final$tmin.xlow[i]-norm.tmin$tmin.xlow.u[i])/norm.tmin$tmin.xlow.sd[i]
       final$n.tmin.low[i]<-(final$tmin.low[i]-norm.tmin$tmin.low.u[i])/norm.tmin$tmin.low.sd[i]
       final$n.dry.length[i]<-(final$avg.dry.days[i]-norm.prcp$dry.length.u[i])/norm.prcp$dry.length.sd[i]
       final$n.tmax.xhigh.length[i]<-(final$tmax.avg.xhigh[i]-norm.tmax$tmax.xhigh.length.u[i])/norm.tmax$tmax.xhigh.length.sd[i]
       final$n.tmax.high.length[i]<-(final$tmax.avg.high[i]-norm.tmax$tmax.high.length.u[i])/norm.tmax$tmax.high.length.sd[i]
       final$n.tmax.xlow.length[i]<-(final$tmax.avg.xlow[i]-norm.tmax$tmax.xlow.length.u[i])/norm.tmax$tmax.xlow.length.sd[i]
       final$n.tmax.low.length[i]<-(final$tmax.avg.low[i]-norm.tmax$tmax.low.length.u[i])/norm.tmax$tmax.low.length.sd[i]
       final$n.tmin.xhigh.length[i]<-(final$tmin.avg.xhigh[i]-norm.tmin$tmin.xhigh.length.u[i])/norm.tmin$tmin.xhigh.length.sd[i]
	final$n.tmin.high.length[i]<-(final$tmin.avg.high[i]-norm.tmin$tmin.high.length.u[i])/norm.tmin$tmin.high.length.sd[i]
	final$n.tmin.xlow.length[i]<-(final$tmin.avg.xlow[i]-norm.tmin$tmin.xlow.length.u[i])/norm.tmin$tmin.xlow.length.sd[i]
	final$n.tmin.low.length[i]<-(final$tmin.avg.low[i]-norm.tmin$tmin.low.length.u[i])/norm.tmin$tmin.low.length.sd[i]
}

