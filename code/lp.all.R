#filtering WoSIS soil datasets by HWSD leptosol regions, combining all WoSIS soil data into one data set, cleaning up dataset, 
#assigning NOAA GHCN stations to WoSIS soil data sites

#load necessary R packages
library(readxl)
library(data.table)
library(tidyverse)

#upload HWSD soil classification data
smu_latlon<-read.csv("/disks/home/abigail/thesis-repository/code/data/smu_latlon.csv")
hwsd <- read_excel("/disks/home/abigail/thesis-repository/code/data/HWSD2_LAYERS.xlsx")

#filter smu_latlon to only include cells with leptosol dominant soils
hwsd_smaller<-with(hwsd,subset(hwsd,select=c(HWSD2_SMU_ID,WRB2)))
hwsd_SMU_lp<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="LP"))
hwsd_SMU_lp<-unique(hwsd_SMU_lp)
lp_smu_latlon<-merge(hwsd_SMU_lp,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)

setDT(lp_smu_latlon)

soil<-c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500","phpwsl","phptot","phprtn",
	"phnf","phkc","phetm3","phetb1","phca","elcosp","elco50","elco25","elco20","cecph7","cecph8","bdwsod","bdfifm","bdfiad","bdfi33",
	"wg0500","wg0200","wg0100","wg0033","wg0100","wv1500","wv0500","wv0033","wv0010")

soil<-as.vector(soil)

files<-paste0("data/wosis-data/",soil,".csv",sep="")
soil.data<-setNames(lapply(files,fread),tools::file_path_sans_ext(basename(files)))

soil.data.1<-lapply(names(soil.data),function(i){
	a<-paste0(i)
	b<-paste0(i,"_avg")
	f<-soil.data[[i]]
	names(f)[names(f)=="value"]<-a
	names(f)[names(f)=="value_avg"]<-b
	setDT(f)
	f<-lp_smu_latlon[f,on=.(xmin<=X,xmax>=X,ymin<=Y,ymax>=Y),nomatch=0]
	names(f)[names(f)=="ymin"]<-"latitude"
	names(f)[names(f)=="xmin"]<-"longitude"
	f[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL
	f<-unique(f)
	f<-subset(f,f$date!="????-??-??")
	f$year<-substr(f$date,1,4)
	f$month.1<-substr(f$date,6,7)
	f$month.2<-substr(f$date,6,6)
	f$day.1<-substr(f$date,9,10)	
	f$day.2<-substr(f$date,8,8)
	f$day.3<-substr(f$date,9,9)
	f<-subset(f,f$year!="????"&f$month.1!="??"&f$month.2!="?"&f$day.1!="??"&f$day.2!="?"&f$day.3!="?")
	f$date<-as.Date(f$date, format = "%Y-%m-%d")
	f
})

lp.all <- Reduce(function(x, y)
  merge(x, y,all = TRUE),
  soil.data.1)

#test<-list("orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500","phpwsl","phptot","phprtn","phnf","phkc",
#	"phetm3","phetb1","phca","elcosp","elco50","elco25","elco20","cecph7","bdwsod","bdfifm","bdfiad","bdfi33","wg0500","wg0200","wg0100","wg0033","wg0010",
#	"wv1500","wv0500","wv0033","wv0010")

#check<-data.frame(var=character(),orig=numeric(),final=numeric())

#for(i in test){
#	final<-paste0(i,"_avg",sep="")
#check<-rbind(check,data.frame(var=i,orig=nrow(soil.data.1[[i]]),final=sum(!is.na(lp.all[[final]]))))
#}

ghcl_stations <- read_fwf("/disks/home/abigail/thesis-repository/code/data/ghcl_stations.txt",
	fwf_widths(c(11, 9, 10, 8, 35, 10),c("station_id", "latitude", "longitude", "elevation", "station_name", "extra")
	),show_col_types=FALSE)

lp.all$latitude<-as.numeric(lp.all$latitude)
lp.all$longitude<-as.numeric(lp.all$longitude)

lp.all$min_lat<-lp.all$latitude-0.1
lp.all$max_lat<-lp.all$latitude+0.1
lp.all$min_lon<-lp.all$longitude-0.1
lp.all$max_lon<-lp.all$longitude+0.1

ghcl_stations$latitude<-as.numeric(ghcl_stations$latitude)
ghcl_stations$longitude<-as.numeric(ghcl_stations$longitude)

setDT(lp.all)
setDT(ghcl_stations)
lp.clim <- lp.all[ghcl_stations,on = .(min_lat <= latitude, max_lat >= latitude,min_lon <= longitude, max_lon >= longitude),nomatch = 0]

soil<-lp.clim

soil$date<-as.Date(soil$date,format="%Y-%m-%d")

soil$YEAR  <- format(soil$date,"%Y")
soil$MONTH <- format(soil$date,"%m")
soil$DAY   <- format(soil$date,"%d")

soil<-soil %>% filter(YEAR >= 1973, YEAR <= 2023)

soil<-soil %>% mutate(start.date = date %m-% years(1))

soil[,c("tceq","orgm","orgc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500","totc","phpwsl","phptot","phprtn","phnf","phkc",
	"phetm3","phetb1","wg0500","wg0200","wg0100","wg0033","wg0010","wv1500","wv0500","wv0033","wv0010","elco50","elco25","elco20","elcosp",
	"phca","bdfiad","bdfifm","bdfi33","bdwsod","cecph7","cecph8","max_lat","max_lon","year","month.1","month.2","day.1","day.2","day.3","extra")]<-NULL

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
names(soil)[names(soil)=="totc_avg"]<-"totc"
names(soil)[names(soil)=="phetm3_avg"]<-"phetm3"
names(soil)[names(soil)=="phkc_avg"]<-"phkc"
names(soil)[names(soil)=="phnf_avg"]<-"phnf"
names(soil)[names(soil)=="phprtn_avg"]<-"phprtn"
names(soil)[names(soil)=="phptot_avg"]<-"phptot"
names(soil)[names(soil)=="phpwsl_avg"]<-"phpwsl"
names(soil)[names(soil)=="phetb1_avg"]<-"phetb1"
names(soil)[names(soil)=="wg0500_avg"]<-"wg0500"
names(soil)[names(soil)=="wg0200_avg"]<-"wg0200"
names(soil)[names(soil)=="wg0100_avg"]<-"wg0100"
names(soil)[names(soil)=="wg0033_avg"]<-"wg0033"
names(soil)[names(soil)=="wg0010_avg"]<-"wg0010"
names(soil)[names(soil)=="wv1500_avg"]<-"wv1500"
names(soil)[names(soil)=="wv0500_avg"]<-"wv0500"
names(soil)[names(soil)=="wv0033_avg"]<-"wv0033"
names(soil)[names(soil)=="wv0010_avg"]<-"wv0010"
names(soil)[names(soil)=="elco25_avg"]<-"elco25"
names(soil)[names(soil)=="elco50_avg"]<-"elco50"
names(soil)[names(soil)=="elcosp_avg"]<-"elcosp"
names(soil)[names(soil)=="phca_avg"]<-"phca"
names(soil)[names(soil)=="bdfiad_avg"]<-"bdfiad"
names(soil)[names(soil)=="bdfi33_avg"]<-"bdfi33"
names(soil)[names(soil)=="bdfifm_avg"]<-"bdfifm"
names(soil)[names(soil)=="bdwsod_avg"]<-"bdwsod"
names(soil)[names(soil)=="cecph7_avg"]<-"cecph7"
names(soil)[names(soil)=="cecph8_avg"]<-"cecph8"
names(soil)[names(soil)=="elco20_avg"]<-"elco20"

write.csv(soil,"/disks/home/abigail/thesis-repository/code/data/soil.final.csv")
