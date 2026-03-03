library(data.table)

soil<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.final.csv")

soil$date<-as.Date(soil$date,format="%Y-%m-%d")

soil$YEAR  <- format(soil$date,"%Y")
soil$MONTH <- format(soil$date,"%m")
soil$DAY   <- format(soil$date,"%d")

soil<-soil %>% filter(YEAR >= 1973, YEAR <= 2023)

soil<-soil %>% mutate(start.date = date %m-% years(1))

soil[,c("tceq","orgm","orgc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500",
	"max_lat","max_lon","licence","station_name","year","month","month.1","month.2","day","day.1","day.2","day.3","extra","layer_name")]<-NULL

names(soil)[names(soil)=="tceq_avg"]<-"tceq"
names(soil)[names(soil)=="orgm_avg"]<-"orgm"
names(soil)[names(soil)=="orgc_avg"]<-"orgc"
names(soil)[names(soil)=="nitkjd_avg"]<-"nitkjd"
names(soil)[names(soil)=="ecec_avg"]<-"ecec"
names(soil)[names(soil)=="cfgr_avg"]<-"cfgr"
names(soil)[names(soil)=="cfvo_avg"]<-"cfvo"
names(soil)[names(soil)=="clay_avg"]<-"clay"
names(soil)[names(soil)=="silt_avg"]<-"silt"
names(soil)[names(soil)=="sand_avg"]<-"sand"
names(soil)[names(soil)=="phaq_avg"]<-"phaq"
names(soil)[names(soil)=="phetol_avg"]<-"phetol"
names(soil)[names(soil)=="wg1500_avg"]<-"wg1500"
names(soil)[names(soil)=="min_lon"]<-"noaa.lon"
names(soil)[names(soil)=="min_lat"]<-"noaa.lat"
names(soil)[names(soil)=="latitude"]<-"wosis.lat"
names(soil)[names(soil)=="longitude"]<-"wosis.lon"
names(soil)[names(soil)=="elevation"]<-"noaa.elev"

#soil<-soil %>% dplyr::mutate(prcp.avg = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.mean = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.events = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh.length = NA_real_)

#stations<-soil$station_id %in% prcp$station_id

#for(i in stations){
	

#soil[,row_id := .I]
#
#
 #       y.2<-PRCP==0
  #      y.3<-PRCP>=xhigh
   #     y.4<-PRCP>=high

#list(
 #       station_id = first(station_id),
  #      prcp.avg = mean(PRCP,na.rm=TRUE),
   #     prcp.mean = mean(PRCP[y.1],na.rm=TRUE),
    #    prcp.events= y.1 %>% tally(),
     #   dry.days = y.2 %>% tally(),
      #  prcp.xhigh = y.3 %>% tally(),
       # prcp.high = y.4 %>% tally(),
        #dry.days.length = norm(PRCP==0)$mean,
        #prcp.high.length = norm(PRCP>=xhigh)$mean,
        #prcp.xhigh.length = norm(PRCP>=high)$mean)},
#by="row_id"]

#prcp.1<-data.table(

#setDT(soil)
#setDT(prcp)
#setDT(limits.prcp)

#soil[,row_id := .I]

#prcp<-rbindlist(precip.2.5,idcol="station_id",fill=TRUE)
	
#one<-soil[,c("station_id","date","start.date","row_id")]
#two<-limits.prcp[,c("station_id","xhigh.prcp","high.prcp")]

#three<-merge(one,two,by="station_id",all.x=TRUE,all.y=FALSE)

#four<-subset(three,!is.na(three$xhigh.prcp)|!is.na(three$high.prcp))

#five<-subset(prcp,prcp$PRCP!=0)
#six<-subset(prcp,prcp$PRCP==0)
#seven<-subset(prcp,prcp$PRCP>=four$prcp.xhigh)
#eight<-subset(prcp,prcp$PRCP>=four$prcp.high)

#prcp.1<-map(precip.2.5, ~.x %>% filter(PRCP!=0))
#prcp.2<-map(precip.2.5, ~.x %>% filter(PRCP==0))
#prcp.3<-map(precip.2.5, ~.x %>% filter(PRCP>=four$prcp.xhigh))
#prcp.4<-map(precip.2.5, ~.x %>% filter(PRCP>=four$prcp.high))

#prcp.5<-lapply(four$station_id,function(i){
#	start<-four$start.date[i]
#	end<-four$date[i]
#	x<-precip.2.5[i]
#	x.5<-prcp.1[i]
#	x.6<-prcp.2[i]
#	x.7<-prcp.3[i]
#	x.8<-prcp.4[i]
#data.table(
#	station_id = i,
#	prcp.avg = mean(x$PRCP,na.rm=TRUE),
#	prcp.mean = mean(x.5$PRCP,na.rm=TRUE),
#	prcp.events= nrow(x.5),
#	dry.days = nrow(x.6),
#	prcp.xhigh = nrow(x.7),
#	prcp.high = nrow(x.8),
#	dry.days.length = norm(x.6)$mean,
#	prcp.high.length = norm(x.8)$mean,
#	prcp.xhigh.length = norm(x.7)$mean)})

#setDT(soil)
#setDT(prcp)
#setDT(limits.prcp)

#soil[,row_id := .I]

#soil<-soil %>% dplyr::mutate(prcp.avg = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.mean = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.events = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high = NA_real_)
#soil<-soil %>% dplyr::mutate(dry.days.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.high.length = NA_real_)
#soil<-soil %>% dplyr::mutate(prcp.xhigh.length = NA_real_)

#soil[, `:=`(
#  prcp.avg = NA_real_,
#  prcp.mean = NA_real_,
#  prcp.events = NA_integer_,
#  dry.days = NA_integer_,
#  prcp.xhigh = NA_integer_,
#  prcp.high = NA_integer_,
#  dry.days.length = NA_real_,
#  prcp.high.length = NA_real_,
#  prcp.xhigh.length = NA_real_
#)]

#norm<-function(y){
#        r<-rle(y)
#        lengths <- r$lengths[r$values]
#        list(
#                mean = if (length(lengths)) mean(lengths,na.rm=TRUE) else NA_real_,
#                sd = if (length(lengths)) sd(lengths,na.rm=TRUE) else NA_real_)}


#one<-merge(soil,limits.prcp[,.(station_id,xhigh=xhigh.prcp,high=high.prcp)],by="station_id",all.x=TRUE)

#prcp.1<-map(precip.2.5, ~.x %>% filter(PRCP!=0))
#prcp.2<-map(precip.2.5, ~.x %>% filter(PRCP==0))
#prcp.3<-map(precip.2.5, ~.x %>% filter(PRCP>=one$xhigh))
#prcp.4<-map(precip.2.5, ~.x %>% filter(PRCP>=one$high))

#setkey(prcp.1,STATION,DATE)
#setkey(prcp.2,STATION,DATE)
#setkey(prcp.3,STATION,DATE)
#setkey(prcp.4,STATION,DATE)

#for(i in seq_along(nrow(one))){
#	station_id<-one$station_id[i]
#	start.date<-one$start.date[i]
#	date<-one$date[i]
#	xhigh<-one$xhigh[i]
#	high<-one$high[i]

#z<-precip.2.5[station_id]
#setDT(z)

#if(nrow(z)==0) next

#z<-z[DATE>=start.date&DATE<=date]

#z.1<-z[PRCP!=0]
#z.2<-z[PRCP==0]
#z.3<-z[PRCP>=xhigh]
#z.4<-z[PRCP>=high]
#
#	soil$prcp.avg[i]<-mean(z$PRCP,na.rm=TRUE)
#	soil$prcp.mean[i]<-mean(z$PRCP[z$PRCP!=0],na.rm=TRUE)
#	soil$prcp.events[i]<-nrow(z.1)
#	soil$dry.days[i]<-nrow(z.2)
#	soil$prcp.xhigh[i]<-nrow(z.3)
#	soil$prcp.high[i]<-nrow(z.4)
#	soil$dry.days.length[i]<-norm(z.2$PRCP)$mean
#	soil$prcp.high.length[i]<-norm(z.3$PRCP)$mean
#	soil$prcp.xhigh.length[i]<-norm(z.4$PRCP)$mean}

setDT(soil)
setDT(prcp)
setDT(limits.prcp)

prcp[, DATE := as.Date(DATE)]
soil[, start.date := as.Date(start.date)]
soil[, end.date := as.Date(date)]

setkey(prcp, station_id, DATE)

soil <- merge(
  soil,
  limits.prcp[, .(station_id,
                  xhigh = xhigh.prcp,
                  high  = high.prcp)],
  by = "station_id",
  all.x = TRUE
)

soil[, `:=`(
  prcp.avg = NA_real_,
  prcp.mean = NA_real_,
  prcp.events = NA_integer_,
  dry.days = NA_integer_,
  prcp.xhigh = NA_integer_,
  prcp.high = NA_integer_,
  dry.days.length = NA_real_,
  prcp.high.length = NA_real_,
  prcp.xhigh.length = NA_real_
)]

mean_spell <- function(cond) {

  # Remove NA first (critical)
  cond <- cond[!is.na(cond)]

  if (length(cond) == 0L) return(NA_real_)

  r <- rle(cond)

  lengths <- r$lengths[r$values == TRUE]

  if (length(lengths) == 0L) {
    NA_real_
  } else {
    mean(lengths)
  }
}

for (i in seq_len(nrow(soil))) {

  st <- soil$station_id[i]
  sd <- soil$start.date[i]
  ed <- soil$end.date[i]
  xh <- soil$xhigh[i]
  hi <- soil$high[i]

  z <- prcp[station_id == st & DATE >= sd & DATE <= ed]

  if (nrow(z) == 0L) next

  pr <- z$PRCP

  soil$prcp.avg[i]   <- mean(pr, na.rm = TRUE)
  soil$prcp.mean[i]  <- mean(pr[pr != 0], na.rm = TRUE)

  soil$prcp.events[i] <- sum(pr != 0)
  soil$dry.days[i]    <- sum(pr == 0)
  soil$prcp.xhigh[i]  <- sum(pr >= xh)
  soil$prcp.high[i]   <- sum(pr >= hi)

  soil$dry.days.length[i]   <- mean_spell(pr == 0)
  soil$prcp.high.length[i]  <- mean_spell(pr >= hi)
  soil$prcp.xhigh.length[i] <- mean_spell(pr >= xh)
}
