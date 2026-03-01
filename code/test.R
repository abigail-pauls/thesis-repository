library(readxl)
library(data.table)
library(tidyverse)

tceq<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/tceq.csv")
orgm<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/orgm.csv")
totc<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/totc.csv")
orgc<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/orgc.csv")
nitkjd<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/nitkjd.csv")
ecec<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/ecec.csv")
cfgr<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/cfgr.csv")
cfvo<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/cfvo.csv")
clay<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/clay.csv")
silt<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/silt.csv")
sand<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/sand.csv")
phaq<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phaq.csv")
phetol<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phetol.csv")
wg1500<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg1500.csv")

smu_latlon<-read.csv("/disks/home/abigail/thesis-repository/code/data/smu_latlon.csv")
hwsd <- read_excel("/disks/home/abigail/thesis-repository/code/data/HWSD2_LAYERS.xlsx")

hwsd_smaller<-with(hwsd,subset(hwsd,select=c(HWSD2_SMU_ID,WRB2)))
hwsd_SMU_lp<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="LP"))
hwsd_SMU_lp<-unique(hwsd_SMU_lp)
lp_smu_latlon<-merge(hwsd_SMU_lp,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)

setDT(lp_smu_latlon)
cfgr<-rename(cfgr,cfgr="value")
cfgr<-rename(cfgr,cfgr_avg="value_avg")
setDT(cfgr)
cfgr.1 <- lp_smu_latlon[cfgr,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cfgr.1<-rename(cfgr.1,latitude="ymin")
cfgr.1<-rename(cfgr.1,longitude="xmin")
cfgr.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

cfvo<-rename(cfvo,cfvo="value")
cfvo<-rename(cfvo,cfvo_avg="value_avg")
setDT(cfvo)
cfvo.1 <- lp_smu_latlon[cfvo,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cfvo.1<-rename(cfvo.1,latitude="ymin")
cfvo.1<-rename(cfvo.1,longitude="xmin")
cfvo.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

clay<-rename(clay,clay="value")
clay<-rename(clay,clay_avg="value_avg")
setDT(clay)
clay.1 <- lp_smu_latlon[clay,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
clay.1<-rename(clay.1,latitude="ymin")
clay.1<-rename(clay.1,longitude="xmin")
clay.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

ecec<-rename(ecec,ecec="value")
ecec<-rename(ecec,ecec_avg="value_avg")
setDT(ecec)
ecec.1 <- lp_smu_latlon[ecec,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
ecec.1<-rename(ecec.1,latitude="ymin")
ecec.1<-rename(ecec.1,longitude="xmin")
ecec.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

orgc<-rename(orgc,orgc="value")
orgc<-rename(orgc,orgc_avg="value_avg")
setDT(orgc)
orgc.1 <- lp_smu_latlon[orgc,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
orgc.1<-rename(orgc.1,latitude="ymin")
orgc.1<-rename(orgc.1,longitude="xmin")
orgc.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

nitkjd<-rename(nitkjd,nitkjd="value")
nitkjd<-rename(nitkjd,nitkjd_avg="value_avg")
setDT(nitkjd)
nitkjd.1 <- lp_smu_latlon[nitkjd,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
nitkjd.1<-rename(nitkjd.1,latitude="ymin")
nitkjd.1<-rename(nitkjd.1,longitude="xmin")
nitkjd.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

orgm<-rename(orgm,orgm="value")
orgm<-rename(orgm,orgm_avg="value_avg")
setDT(orgm)
orgm.1 <- lp_smu_latlon[orgm,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
orgm.1<-rename(orgm.1,latitude="ymin")
orgm.1<-rename(orgm.1,longitude="xmin")
orgm.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

phaq<-rename(phaq,phaq="value")
phaq<-rename(phaq,phaq_avg="value_avg")
setDT(phaq)
phaq.1 <- lp_smu_latlon[phaq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phaq.1<-rename(phaq.1,latitude="ymin")
phaq.1<-rename(phaq.1,longitude="xmin")
phaq.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

phetol<-rename(phetol,phetol="value")
phetol<-rename(phetol,phetol_avg="value_avg")
setDT(phetol)
phetol.1 <- lp_smu_latlon[phetol,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phetol.1<-rename(phetol.1,latitude="ymin")
phetol.1<-rename(phetol.1,longitude="xmin")
phetol.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

sand<-rename(sand,sand="value")
sand<-rename(sand,sand_avg="value_avg")
setDT(sand)
sand.1 <- lp_smu_latlon[sand,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
sand.1<-rename(sand.1,latitude="ymin")
sand.1<-rename(sand.1,longitude="xmin")
sand.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

silt<-rename(silt,silt="value")
silt<-rename(silt,silt_avg="value_avg")
setDT(silt)
silt.1 <- lp_smu_latlon[silt,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
silt.1<-rename(silt.1,latitude="ymin")
silt.1<-rename(silt.1,longitude="xmin")
silt.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

tceq<-rename(tceq,tceq="value")
tceq<-rename(tceq,tceq_avg="value_avg")
setDT(tceq)
tceq.1 <- lp_smu_latlon[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
tceq.1<-rename(tceq.1,latitude="ymin")
tceq.1<-rename(tceq.1,longitude="xmin")
tceq.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

totc<-rename(totc,totc="value")
totc<-rename(totc,totc_avg="value_avg")
setDT(totc)
totc.1 <- lp_smu_latlon[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
totc.1<-rename(totc.1,latitude="ymin")
totc.1<-rename(totc.1,longitude="xmin")
totc.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

wg1500<-rename(wg1500,wg1500="value")
wg1500<-rename(wg1500,wg1500_avg="value_avg")
setDT(wg1500)
wg1500.1 <- lp_smu_latlon[wg1500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg1500.1<-rename(wg1500.1,latitude="ymin")
wg1500.1<-rename(wg1500.1,longitude="xmin")
wg1500.1[,c("xmax","ymax","method_options","license","positional_uncertainty","dataset_id","profile_id","layer_id","profile_code")]<-NULL

cfgr.2<-unique(cfgr.1)
cfvo.2<-unique(cfvo.1)
clay.2<-unique(clay.1)
ecec.2<-unique(ecec.1)
nitkjd.2<-unique(nitkjd.1)
orgc.2<-unique(orgc.1)
orgm.2<-unique(orgm.1)
phaq.2<-unique(phaq.1)
phetol.2<-unique(phetol.1)
sand.2<-unique(sand.1)
silt.2<-unique(silt.1)
tceq.2<-unique(tceq.1)
totc.2<-unique(totc.1)
wg1500.2<-unique(wg1500.1)

merge<-list("tceq.2","orgm.2","orgc.2","totc.2","nitkjd.2","ecec.2","cfgr.2","cfvo.2","clay.2","silt.2","sand.2","phaq.2","phetol.2","wg1500.2")
merge<-as.vector(merge)

lp.all<-data.frame()

for(i in merge){
	x<-get(i)
	lp.all<-merge(lp.all,x,all=TRUE)}

lp.all<-subset(lp.all,lp.all$date!="????-??-??")

lp.all$year<-substr(lp.all$date,1,4)
lp.all$month.1<-substr(lp.all$date,6,7)
lp.all$month.2<-substr(lp.all$date,6,6)
lp.all$day.1<-substr(lp.all$date,9,10)
lp.all$day.2<-substr(lp.all$date,8,8)
lp.all$day.3<-substr(lp.all$date,9,9)

lp.all<-subset(lp.all,lp.all$year!="????"&lp.all$month.1!="??"&lp.all$month.2!="?"&lp.all$day.1!="??"&lp.all$day.2!="?"&lp.all$day.3!="?")

lp.all$date<-as.Date(lp.all$date, format = "%Y-%m-%d")

ghcl_stations <- read_fwf(
  "/disks/home/abigail/thesis-repository/code/data/ghcl_stations.txt",
  fwf_widths(
    c(11, 9, 10, 8, 35, 10),
    c("station_id", "latitude", "longitude", "elevation", "station_name", "extra")
  ),show_col_types=FALSE
)

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

write.csv(lp.clim,"/disks/home/abigail/thesis-repository/code/data/soil.final.csv")
