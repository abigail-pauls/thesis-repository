#filtering WoSIS soil datasets by HWSD leptosol regions, combining all WoSIS soil data into one data set, cleaning up dataset, 
#assigning NOAA GHCN stations to WoSIS soil data sites

#load necessary R packages
library(readxl)
library(data.table)
library(tidyverse)

#upload WoSIS soil data of interest
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
phptot<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phptot.csv")
phpwsl<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phpwsl.csv")
phnf<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phnf.csv")
phprtn<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phprtn.csv")
phetb1<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phetb1.csv")
phetm3<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phetm3.csv")
phkc<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phkc.csv")
phca<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/phca.csv")
elcosp<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/elcosp.csv")
elco50<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/elco50.csv")
elco25<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/elco25.csv")
elco20<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/elco20.csv")
cecph8<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/cecph8.csv")
cecph7<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/cecph7.csv")
bdwsod<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/bdwsod.csv")
bdfifm<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/bdfifm.csv")
bdfiad<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/bdfiad.csv")
bdfi33<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/bdfi33.csv")
wg0500<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg0500.csv")
wg0200<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg0200.csv")
wg0100<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg0100.csv")
wg0033<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg0033.csv")
wg0010<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wg0010.csv")
wv0500<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wv0500.csv")
wv1500<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wv1500.csv")
wv0033<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wv0033.csv")
wv0010<-read.csv("/disks/home/abigail/thesis-repository/code/data/wosis-data/wv0010.csv")

#upload HWSD soil classification data
smu_latlon<-read.csv("/disks/home/abigail/thesis-repository/code/data/smu_latlon.csv")
hwsd <- read_excel("/disks/home/abigail/thesis-repository/code/data/HWSD2_LAYERS.xlsx")

#filter smu_latlon to only include cells with leptosol dominant soils
hwsd_smaller<-with(hwsd,subset(hwsd,select=c(HWSD2_SMU_ID,WRB2)))
hwsd_SMU_lp<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="LP"))
hwsd_SMU_lp<-unique(hwsd_SMU_lp)
lp_smu_latlon<-merge(hwsd_SMU_lp,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)

setDT(lp_smu_latlon)

#filter WoSIS datasets to only include leptosol dominant soils and remove unnecessary columns
#CFGR: gravimentic coarse fragments
cfgr<-rename(cfgr,cfgr="value")
cfgr<-rename(cfgr,cfgr_avg="value_avg")
setDT(cfgr)
cfgr.1 <- lp_smu_latlon[cfgr,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cfgr.1<-rename(cfgr.1,latitude="ymin")
cfgr.1<-rename(cfgr.1,longitude="xmin")
cfgr.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#CFVO: volumetric coarse fragments
cfvo<-rename(cfvo,cfvo="value")
cfvo<-rename(cfvo,cfvo_avg="value_avg")
setDT(cfvo)
cfvo.1 <- lp_smu_latlon[cfvo,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cfvo.1<-rename(cfvo.1,latitude="ymin")
cfvo.1<-rename(cfvo.1,longitude="xmin")
cfvo.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#CLAY: soil clay content
clay<-rename(clay,clay="value")
clay<-rename(clay,clay_avg="value_avg")
setDT(clay)
clay.1 <- lp_smu_latlon[clay,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
clay.1<-rename(clay.1,latitude="ymin")
clay.1<-rename(clay.1,longitude="xmin")
clay.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#ECEC: effective cation exchange capacity
ecec<-rename(ecec,ecec="value")
ecec<-rename(ecec,ecec_avg="value_avg")
setDT(ecec)
ecec.1 <- lp_smu_latlon[ecec,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
ecec.1<-rename(ecec.1,latitude="ymin")
ecec.1<-rename(ecec.1,longitude="xmin")
ecec.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#ORGC: organic carbon
orgc<-rename(orgc,orgc="value")
orgc<-rename(orgc,orgc_avg="value_avg")
setDT(orgc)
orgc.1 <- lp_smu_latlon[orgc,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
orgc.1<-rename(orgc.1,latitude="ymin")
orgc.1<-rename(orgc.1,longitude="xmin")
orgc.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#NITKJD: total nitrogen
nitkjd<-rename(nitkjd,nitkjd="value")
nitkjd<-rename(nitkjd,nitkjd_avg="value_avg")
setDT(nitkjd)
nitkjd.1 <- lp_smu_latlon[nitkjd,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
nitkjd.1<-rename(nitkjd.1,latitude="ymin")
nitkjd.1<-rename(nitkjd.1,longitude="xmin")
nitkjd.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#ORGM: organic matter
orgm<-rename(orgm,orgm="value")
orgm<-rename(orgm,orgm_avg="value_avg")
setDT(orgm)
orgm.1 <- lp_smu_latlon[orgm,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
orgm.1<-rename(orgm.1,latitude="ymin")
orgm.1<-rename(orgm.1,longitude="xmin")
orgm.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHAQ: aquious pH
phaq<-rename(phaq,phaq="value")
phaq<-rename(phaq,phaq_avg="value_avg")
setDT(phaq)
phaq.1 <- lp_smu_latlon[phaq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phaq.1<-rename(phaq.1,latitude="ymin")
phaq.1<-rename(phaq.1,longitude="xmin")
phaq.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHETOL: total phosphorus
phetol<-rename(phetol,phetol="value")
phetol<-rename(phetol,phetol_avg="value_avg")
setDT(phetol)
phetol.1 <- lp_smu_latlon[phetol,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phetol.1<-rename(phetol.1,latitude="ymin")
phetol.1<-rename(phetol.1,longitude="xmin")
phetol.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#SAND: soil sand content
sand<-rename(sand,sand="value")
sand<-rename(sand,sand_avg="value_avg") 
setDT(sand) 
sand.1 <- lp_smu_latlon[sand,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0] 
sand.1<-rename(sand.1,latitude="ymin") 
sand.1<-rename(sand.1,longitude="xmin") 
sand.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#SILT: soil silt content
silt<-rename(silt,silt="value")
silt<-rename(silt,silt_avg="value_avg")
setDT(silt)
silt.1 <- lp_smu_latlon[silt,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
silt.1<-rename(silt.1,latitude="ymin")
silt.1<-rename(silt.1,longitude="xmin")
silt.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#TCEQ: total calcium carbonate equivilant
tceq<-rename(tceq,tceq="value")
tceq<-rename(tceq,tceq_avg="value_avg")
setDT(tceq)
tceq.1 <- lp_smu_latlon[tceq,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
tceq.1<-rename(tceq.1,latitude="ymin")
tceq.1<-rename(tceq.1,longitude="xmin")
tceq.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#TOTC: total carbon
totc<-rename(totc,totc="value")
totc<-rename(totc,totc_avg="value_avg")
setDT(totc)
totc.1 <- lp_smu_latlon[totc,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
totc.1<-rename(totc.1,latitude="ymin")
totc.1<-rename(totc.1,longitude="xmin")
totc.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#WG1500: gravimetric water content, 1500kPa
wg1500<-rename(wg1500,wg1500="value")
wg1500<-rename(wg1500,wg1500_avg="value_avg")
setDT(wg1500)
wg1500.1 <- lp_smu_latlon[wg1500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg1500.1<-rename(wg1500.1,latitude="ymin")
wg1500.1<-rename(wg1500.1,longitude="xmin")
wg1500.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHPWSL
phpwsl<-rename(phpwsl,phpwsl="value")
phpwsl<-rename(phpwsl,phpwsl_avg="value_avg")
setDT(phpwsl)
phpwsl.1 <- lp_smu_latlon[phpwsl,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phpwsl.1<-rename(phpwsl.1,latitude="ymin")
phpwsl.1<-rename(phpwsl.1,longitude="xmin")
phpwsl.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHPTOT
phptot<-rename(phptot,phptot="value")
phptot<-rename(phptot,phptot_avg="value_avg")
setDT(phptot)
phptot.1 <- lp_smu_latlon[phptot,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phptot.1<-rename(phptot.1,latitude="ymin")
phptot.1<-rename(phptot.1,longitude="xmin")
phptot.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHPRTN
phprtn<-rename(phprtn,phprtn="value")
phprtn<-rename(phprtn,phprtn_avg="value_avg")
setDT(phprtn)
phprtn.1 <- lp_smu_latlon[phprtn,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phprtn.1<-rename(phprtn.1,latitude="ymin")
phprtn.1<-rename(phprtn.1,longitude="xmin")
phprtn.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHNF
phnf<-rename(phnf,phnf="value")
phnf<-rename(phnf,phnf_avg="value_avg")
setDT(phnf)
phnf.1 <- lp_smu_latlon[phnf,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phnf.1<-rename(phnf.1,latitude="ymin")
phnf.1<-rename(phnf.1,longitude="xmin")
phnf.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHKC
phkc<-rename(phkc,phkc="value")
phkc<-rename(phkc,phkc_avg="value_avg")
setDT(phkc)
phkc.1 <- lp_smu_latlon[phkc,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phkc.1<-rename(phkc.1,latitude="ymin")
phkc.1<-rename(phkc.1,longitude="xmin")
phkc.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHETM3
phetm3<-rename(phetm3,phetm3="value")
phetm3<-rename(phetm3,phetm3_avg="value_avg")
setDT(phetm3)
phetm3.1 <- lp_smu_latlon[phetm3,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phetm3.1<-rename(phetm3.1,latitude="ymin")
phetm3.1<-rename(phetm3.1,longitude="xmin")
phetm3.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#PHETB1
phetb1<-rename(phetb1,phetb1="value")
phetb1<-rename(phetb1,phetb1_avg="value_avg")
setDT(phetb1)
phetb1.1 <- lp_smu_latlon[phetb1,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phetb1.1<-rename(phetb1.1,latitude="ymin")
phetb1.1<-rename(phetb1.1,longitude="xmin")
phetb1.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

phca<-rename(phca,phca="value")
phca<-rename(phca,phca_avg="value_avg")
setDT(phca)
phca.1 <- lp_smu_latlon[phca,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
phca.1<-rename(phca.1,latitude="ymin")
phca.1<-rename(phca.1,longitude="xmin")
phca.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

elcosp<-rename(elcosp,elcosp="value")
elcosp<-rename(elcosp,elcosp_avg="value_avg")
setDT(elcosp)
elcosp.1 <- lp_smu_latlon[elcosp,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
elcosp.1<-rename(elcosp.1,latitude="ymin")
elcosp.1<-rename(elcosp.1,longitude="xmin")
elcosp.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL


elco50<-rename(elco50,elco50="value")
elco50<-rename(elco50,elco50_avg="value_avg")
setDT(elco50)
elco50.1 <- lp_smu_latlon[elco50,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
elco50.1<-rename(elco50.1,latitude="ymin")
elco50.1<-rename(elco50.1,longitude="xmin")
elco50.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

elco25<-rename(elco25,elco25="value")
elco25<-rename(elco25,elco25_avg="value_avg")
setDT(elco25)
elco25.1 <- lp_smu_latlon[elco25,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
elco25.1<-rename(elco25.1,latitude="ymin")
elco25.1<-rename(elco25.1,longitude="xmin")
elco25.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL


elco20<-rename(elco20,elco20="value")
elco20<-rename(elco20,elco20_avg="value_avg")
setDT(elco20)
elco20.1 <- lp_smu_latlon[elco20,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
elco20.1<-rename(elco20.1,latitude="ymin")
elco20.1<-rename(elco20.1,longitude="xmin")
elco20.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

cecph8<-rename(cecph8,cecph8="value")
cecph8<-rename(cecph8,cecph8_avg="value_avg")
setDT(cecph8)
cecph8.1 <- lp_smu_latlon[cecph8,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cecph8.1<-rename(cecph8.1,latitude="ymin")
cecph8.1<-rename(cecph8.1,longitude="xmin")
cecph8.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

cecph7<-rename(cecph7,cecph7="value")
cecph7<-rename(cecph7,cecph7_avg="value_avg")
setDT(cecph7)
cecph7.1 <- lp_smu_latlon[cecph7,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
cecph7.1<-rename(cecph7.1,latitude="ymin")
cecph7.1<-rename(cecph7.1,longitude="xmin")
cecph7.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

bdwsod<-rename(bdwsod,bdwsod="value")
bdwsod<-rename(bdwsod,bdwsod_avg="value_avg")
setDT(bdwsod)
bdwsod.1 <- lp_smu_latlon[bdwsod,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
bdwsod.1<-rename(bdwsod.1,latitude="ymin")
bdwsod.1<-rename(bdwsod.1,longitude="xmin")
bdwsod.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

bdfifm<-rename(bdfifm,bdfifm="value")
bdfifm<-rename(bdfifm,bdfifm_avg="value_avg")
setDT(bdfifm)
bdfifm.1 <- lp_smu_latlon[bdfifm,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
bdfifm.1<-rename(bdfifm.1,latitude="ymin")
bdfifm.1<-rename(bdfifm.1,longitude="xmin")
bdfifm.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

bdfiad<-rename(bdfiad,bdfiad="value")
bdfiad<-rename(bdfiad,bdfiad_avg="value_avg")
setDT(bdfiad)
bdfiad.1 <- lp_smu_latlon[bdfiad,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
bdfiad.1<-rename(bdfiad.1,latitude="ymin")
bdfiad.1<-rename(bdfiad.1,longitude="xmin")
bdfiad.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

bdfi33<-rename(bdfi33,bdfi33="value")
bdfi33<-rename(bdfi33,bdfi33_avg="value_avg")
setDT(bdfi33)
bdfi33.1 <- lp_smu_latlon[bdfi33,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
bdfi33.1<-rename(bdfi33.1,latitude="ymin")
bdfi33.1<-rename(bdfi33.1,longitude="xmin")
bdfi33.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wg0500<-rename(wg0500,wg0500="value")
wg0500<-rename(wg0500,wg0500_avg="value_avg")
setDT(wg0500)
wg0500.1 <- lp_smu_latlon[wg0500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg0500.1<-rename(wg0500.1,latitude="ymin")
wg0500.1<-rename(wg0500.1,longitude="xmin")
wg0500.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wg0200<-rename(wg0200,wg0200="value")
wg0200<-rename(wg0200,wg0200_avg="value_avg")
setDT(wg0200)
wg0200.1 <- lp_smu_latlon[wg0200,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg0200.1<-rename(wg0200.1,latitude="ymin")
wg0200.1<-rename(wg0200.1,longitude="xmin")
wg0200.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL


wg0100<-rename(wg0100,wg0100="value")
wg0100<-rename(wg0100,wg0100_avg="value_avg")
setDT(wg0100)
wg0100.1 <- lp_smu_latlon[wg0100,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg0100.1<-rename(wg0100.1,latitude="ymin")
wg0100.1<-rename(wg0100.1,longitude="xmin")
wg0100.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wg0033<-rename(wg0033,wg0033="value")
wg0033<-rename(wg0033,wg0033_avg="value_avg")
setDT(wg0033)
wg0033.1 <- lp_smu_latlon[wg0033,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg0033.1<-rename(wg0033.1,latitude="ymin")
wg0033.1<-rename(wg0033.1,longitude="xmin")
wg0033.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL


wg0010<-rename(wg0010,wg0010="value")
wg0010<-rename(wg0010,wg0010_avg="value_avg")
setDT(wg0010)
wg0010.1 <- lp_smu_latlon[wg0010,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wg0010.1<-rename(wg0010.1,latitude="ymin")
wg0010.1<-rename(wg0010.1,longitude="xmin")
wg0010.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wv1500<-rename(wv1500,wv1500="value")
wv1500<-rename(wv1500,wv1500_avg="value_avg")
setDT(wv1500)
wv1500.1 <- lp_smu_latlon[wv1500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wv1500.1<-rename(wv1500.1,latitude="ymin")
wv1500.1<-rename(wv1500.1,longitude="xmin")
wv1500.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wv0500<-rename(wv0500,wv0500="value")
wv0500<-rename(wv0500,wv0500_avg="value_avg")
setDT(wv0500)
wv0500.1 <- lp_smu_latlon[wv0500,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wv0500.1<-rename(wv0500.1,latitude="ymin")
wv0500.1<-rename(wv0500.1,longitude="xmin")
wv0500.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wv0033<-rename(wv0033,wv0033="value")
wv0033<-rename(wv0033,wv0033_avg="value_avg")
setDT(wv0033)
wv0033.1 <- lp_smu_latlon[wv0033,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wv0033.1<-rename(wv0033.1,latitude="ymin")
wv0033.1<-rename(wv0033.1,longitude="xmin")
wv0033.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

wv0010<-rename(wv0010,wv0010="value")
wv0010<-rename(wv0010,wv0010_avg="value_avg")
setDT(wv0010)
wv0010.1 <- lp_smu_latlon[wv0010,on = .(xmin <= X, xmax >= X, ymin <= Y, ymax >= Y),nomatch = 0]
wv0010.1<-rename(wv0010.1,latitude="ymin")
wv0010.1<-rename(wv0010.1,longitude="xmin")
wv0010.1[,c("xmax","ymax","method_options","licence","positional_uncertainty","dataset_id","profile_id","layer_name","profile_code")]<-NULL

#ensure WoSIS soil data does not have duplicates
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
phpwsl.2<-unique(phpwsl.1)
phptot.2<-unique(phptot.1)
phprtn.2<-unique(phprtn.1)
phnf.2<-unique(phnf.1)
phkc.2<-unique(phkc.1)
phetm3.2<-unique(phetm3.1)
phetb1.2<-unique(phetb1.1)
elco50.2<-unique(elco50.1)
elco25.2<-unique(elco25.1)
elco20.2<-unique(elco20.1)
cecph8.2<-unique(cecph8.1)
cecph7.2<-unique(cecph7.1)
bdwsod.2<-unique(bdwsod.1)
bdfifm.2<-unique(bdfifm.1)
bdfiad.2<-unique(bdfiad.1)
bdfi33.2<-unique(bdfi33.1)
elcosp.2<-unique(elcosp.1)
phca.2<-unique(phca.1)
wg0500.2<-unique(wg0500.1)
wg0200.2<-unique(wg0200.1)
wg0100.2<-unique(wg0100.1)
wg0033.2<-unique(wg0033.1)
wg0010.2<-unique(wg0010.1)
wv1500.2<-unique(wv1500.1)
wv0500.2<-unique(wv0500.1)
wv0033.2<-unique(wv0033.1)
wv0010.2<-unique(wv0010.1)

data<-list("orgm.2","orgc.2","totc.2","nitkjd.2","ecec.2","cfgr.2","cfvo.2","clay.2","silt.2","sand.2","phaq.2","phetol.2","wg1500.2",
	"phpwsl.2","phptot.2","phprtn.2","phnf.2","phkc.2","phetm3.2","phetb1.2","phca.2","elcosp.2","elco50.2","elco25.2","elco20.2","cecph7.2","cecph8.2",
	"bdwsod.2","bdfifm.2","bdfiad.2","bdfi33.2","wg0500","wg0200","wg0100","wg0033","wg0100","wv1500","wv0500","wv0033","wv0100")
data<-as.vector(data)

lp.all<-tceq.2

for(i in data){
	a<-get(i)
	lp.all<-merge(lp.all,a,all=TRUE)}

test<-list("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500","phpwsl","phptot","phprtn","phnf","phkc",
       "phetm3","phetb1","phca","elcosp","elco50","elco25","elco20","cecph7","bdwsod","bdfifm","bdfiad","bdfi33","wg0500","wg0200","wg0100","wg0033","wg0010",
	"wv1500","wv0500","wv0033","wv0010")

check<-data.frame(var=character(),orig=numeric(),final=numeric())

for(i in test){
       a<-paste0(i,".2",sep="")
       orig<-get(a)
       final<-paste0(i,"_avg",sep="")
check<-rbind(check,data.frame(var=i,orig=nrow(orig),final=sum(!is.na(lp.all[[final]]))))
}

lp.all<-subset(lp.all,lp.all$date!="????-??-??")

lp.all$year<-substr(lp.all$date,1,4)
lp.all$month.1<-substr(lp.all$date,6,7)
lp.all$month.2<-substr(lp.all$date,6,6)
lp.all$day.1<-substr(lp.all$date,9,10)
lp.all$day.2<-substr(lp.all$date,8,8)
lp.all$day.3<-substr(lp.all$date,9,9)

lp.all<-subset(lp.all,lp.all$year!="????"&lp.all$month.1!="??"&lp.all$month.2!="?"&lp.all$day.1!="??"&lp.all$day.2!="?"&lp.all$day.3!="?")

lp.all$date<-as.Date(lp.all$date, format = "%Y-%m-%d")

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
	"phetm3","phetb1","max_lat","max_lon","licence","station_name","year","month","month.1","month.2","day","day.1","day.2","day.3","extra",
	"layer_name")]<-NULL

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
