#soil<-read.csv("/disks/home/abigail/thesis-repository/code/data/soil.final.csv")

#soil$date<-as.Date(soil$date,format="%Y-%m-%d")

#soil$YEAR  <- format(soil$date,"%Y")
#soil$MONTH <- format(soil$date,"%m")
#soil$DAY   <- format(soil$date,"%d")

#soil<-soil %>% filter(YEAR >= 1973, YEAR <= 2023)

#soil<-soil %>% mutate(start.date = date %m-% years(1))

#soil[,c("tceq","orgm","orgc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500",
 #      "max_lat","max_lon","licence","station_name","year","month","month.1","month.2","day","day.1","day.2","day.3","extra","layer_name")]<-NULL

#names(soil)[names(soil)=="tceq_avg"]<-"tceq"
#names(soil)[names(soil)=="orgm_avg"]<-"orgm"
#names(soil)[names(soil)=="orgc_avg"]<-"orgc"
#names(soil)[names(soil)=="nitkjd_avg"]<-"nitkjd"
#names(soil)[names(soil)=="ecec_avg"]<-"ecec"
#names(soil)[names(soil)=="cfgr_avg"]<-"cfgr"
#names(soil)[names(soil)=="cfvo_avg"]<-"cfvo"
#names(soil)[names(soil)=="clay_avg"]<-"clay"
#names(soil)[names(soil)=="silt_avg"]<-"silt"
#names(soil)[names(soil)=="sand_avg"]<-"sand"
#names(soil)[names(soil)=="phaq_avg"]<-"phaq"
#names(soil)[names(soil)=="phetol_avg"]<-"phetol"
#names(soil)[names(soil)=="wg1500_avg"]<-"wg1500"
#names(soil)[names(soil)=="min_lon"]<-"noaa.lon"
#names(soil)[names(soil)=="min_lat"]<-"noaa.lat"
#names(soil)[names(soil)=="latitude"]<-"wosis.lat"
#names(soil)[names(soil)=="longitude"]<-"wosis.lon"
#names(soil)[names(soil)=="elevation"]<-"noaa.elev"

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
	

#setDT(prcp)
#setDT(soil)
#setDT(limits.prcp)

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

prcp<-rbindlist(precip.2.5,idcol="station_id",fill=TRUE)
	
one<-soil[,c("station_id","date","start.date","row_id")]
two<-limits.prcp[,c("station_id","xhigh.prcp","high.prcp")]

three<-merge(one,two,by="station_id",all.x=TRUE,all.y=FALSE)

four<-subset(three,!is.na(three$xhigh.prcp)|!is.na(three$high.prcp))

five<-subset(prcp,prcp$PRCP!=0)
six<-subset(prcp,prcp$PRCP==0)
seven<-subset(prcp,prcp$PRCP>=four$prcp.xhigh)
eight<-subset(prcp,prcp$PRCP>=four$prcp.high)

prcp.1<-lapply(four$station_id,function(i){
	start<-four$start.date[i]
	end<-four$date[i]
	x<-prcp[station_id==i]
	x.5<-five[station_id==i,DATE>=start&DATE<=end]
	x.6<-six[station_id==i,DATE>=start&DATE<=end]
	x.7<-seven[station_id==i,DATE>=start&DATE<=end]
	x.8<-eight[station_id==i,DATE>=start&DATE<=end]
data.table(
	station_id = i,
	prcp.avg = mean(x$PRCP,na.rm=TRUE),
	prcp.mean = mean(x.5$PRCP,na.rm=TRUE),
	prcp.events= nrow(x.5),
	dry.days = nrow(x.6),
	prcp.xhigh = nrow(x.7),
	prcp.high = nrow(x.8),
	dry.days.length = norm(x.6)$mean,
	prcp.high.length = norm(x.8)$mean,
	prcp.xhigh.length = norm(x.7)$mean)})
