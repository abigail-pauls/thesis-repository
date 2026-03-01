getwd()
setwd("C:/Users/pauls/OneDrive/Documents/Fall 2025/THESIS/THESIS/WoSIS_Data")
#load packages used in this code
library(readxl)
library(mgsub)
library(purrr)
library(tidyverse)
#BATCH UPLOAD WOSIS DATA
#create a list containing the names of each csv file
CSVfiles <- list.files(pattern="\\.csv$", full.names=TRUE)
print(CSVfiles)
dataframe_names <- mgsub(CSVfiles, c("./",".csv"), c("",""))
print(dataframe_names)

dataframe_names <- mgsub(CSVfiles, c("./",".csv"), c("",""))

#import data from each csv into a list
dat <- list()
for(x in unique(CSVfiles)){
  dat[[x]] <- read.csv(x)
}
#split the list into individual dataframes
dat <- dat %>% set_names(dataframe_names)
invisible(list2env(dat ,.GlobalEnv))
ls()

#CLEAN UP DATAFRAMES
#rename 'value' and 'value_avg' column in each csv so the data can be merged and retain all information labeled correctly

for (i in dataframe_names) {
  t<-print(i)
  print(t)
}
?rename
?assign
bdfi33<-rename(bdfi33,bdfi33 = "value")
colnames(wv1500)[10] <- "wv1500"
names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

#remove data columns from each csv to streamline merge process
for (i in dataframe_names) {
  x=get(i)
  x$layer_name<-NULL
  x$method_options<-NULL
  x$licence<-NULL
  x$positional_uncertainty<-NULL
  assign(i,x)
}
#MERGE ALL DATA INTO A SINGLE DATAFRAME
#create dataframe to merge all csv information into
wosis<-data.frame()

data_merge<-list("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phptot","wg1500")
data_merge<-as.vector(data_merge)

#merge all data frames in list
wosis<-Reduce(function(x, y) merge(x, y, all=TRUE), data_merge)

wosis_1<-wosis

wosis<-merge(wosis,profiles)


#add all datlist()#add all data into the wosis dataframe
for (j in data_merge) {
  x=get(j)
  wosis<-merge(wosis,x,all=TRUE)
}

write.csv(wosis,"wosis_og.csv", row.names = FALSE)

wosis <- read.csv("~/Fall 2025/THESIS/THESIS/WoSIS_Data/wosis_og.csv")


#save a copy of this dataframe to return to if any mistakes are made later
wosis_1<-wosis
wosis<-wosis_1

bdfi33<-rename(bdfi33,bdfi33="value")
bdfi33<-rename(bdfi33,bdfi33_avg="value_avg")
bdfiad<-rename(bdfiad,bdfiad="value")
bdfiad<-rename(bdfiad,bdfiad_avg="value_avg")
bdfifm<-rename(bdfifm,bdfifm="value")
bdfifm<-rename(bdfifm,bdfifm_avg="value_avg")
bdwsod<-rename(bdwsod,bdwsod="value")
bdwsod<-rename(bdwsod,bdwsod_avg="value_avg")
cecph7<-rename(cecph7,cecph7="value")
cecph7<-rename(cecph7,cecph7_avg="value_avg")
cecph8<-rename(cecph8,cecph8="value")
cecph8<-rename(cecph8,cecph8_avg="value_avg")
cfgr<-rename(cfgr,cfgr="value")
cfgr<-rename(cfgr,cfgr_avg="value_avg")
cfvo<-rename(cfvo,cfvo="value")
cfvo<-rename(cfvo,cfvo_avg="value_avg")
clay<-rename(clay,clay="value")
clay<-rename(clay,clay_avg="value_avg")
ecec<-rename(ecec,ecec="value")
ecec<-rename(ecec,ecec_avg="value_avg")
elco20<-rename(elco20,elco20="value")
elco20<-rename(elco20,elco20_avg="value_avg")
elco25<-rename(elco25,elco25="value")
elco25<-rename(elco25,elco25_avg="value_avg")
elco50<-rename(elco50,elco50="value")
elco50<-rename(elco50,elco50_avg="value_avg")
orgc<-rename(orgc,orgc="value")
orgc<-rename(orgc,orgc_avg="value_avg")
elcosp<-rename(elcosp,elcosp="value")
elcosp<-rename(elcosp,elcosp_avg="value_avg")
nitkjd<-rename(nitkjd,nitkjd="value")
nitkjd<-rename(nitkjd,nitkjd_avg="value_avg")
orgm<-rename(orgm,orgm="value")
orgm<-rename(orgm,orgm_avg="value_avg")
phaq<-rename(phaq,phaq="value")
phaq<-rename(phaq,phaq_avg="value_avg")
phca<-rename(phca,phca="value")
phca<-rename(phca,phca_avg="value_avg")
phetb1<-rename(phetb1,phetb1="value")
phetb1<-rename(phetb1,phetb1_avg="value_avg")
phetm3<-rename(phetm3,phetm3="value")
phetm3<-rename(phetm3,phetm3_avg="value_avg")
phetol<-rename(phetol,phetol="value")
phetol<-rename(phetol,phetol_avg="value_avg")
phkc<-rename(phkc,phkc="value")
phkc<-rename(phkc,phkc_avg="value_avg")
phnf<-rename(phnf,phnf="value")
phnf<-rename(phnf,phnf_avg="value_avg")
phprtn<-rename( phprtn, phprtn="value")
phprtn<-rename( phprtn, phprtn_avg="value_avg")
phptot<-rename(phptot,phptot="value")
phptot<-rename(phptot,phptot_avg="value_avg")
phpwsl<-rename(phpwsl,phpwsl="value")
phpwsl<-rename(phpwsl,phpwsl_avg="value_avg")
sand<-rename(sand,sand="value")
sand<-rename(sand,sand_avg="value_avg")
silt<-rename(silt,silt="value")
silt<-rename(silt,silt_avg="value_avg")
tceq<-rename(tceq,tceq="value")
tceq<-rename(tceq,tceq_avg="value_avg")
totc<-rename(totc,totc="value")
totc<-rename(totc,totc_avg="value_avg")
wg0006<-rename(wg0006,wg0006="value")
wg0006<-rename(wg0006,wg0006_avg="value_avg")
wg0010<-rename(wg0010,wg0010="value")
wg0010<-rename(wg0010,wg0010_avg="value_avg")
wg0033<-rename(wg0033,wg0033="value")
wg0033<-rename(wg0033,wg0033_avg="value_avg")
wg0100<-rename(wg0100,wg0100="value")
wg0100<-rename(wg0100,wg0100_avg="value_avg")
wg0200<-rename(wg0200,wg0200="value")
wg0200<-rename(wg0200,wg0200_avg="value_avg")
wg0500<-rename(wg0500,wg0500="value")
wg0500<-rename(wg0500,wg0500_avg="value_avg")
wg1500<-rename(wg1500,wg1500="value")
wg1500<-rename(wg1500,wg1500_avg="value_avg")
wv0010<-rename(wv0010,wv0010="value")
wv0010<-rename(wv0010,wv0010_avg="value_avg")
wv0033<-rename(wv0033,wv0033="value")
wv0033<-rename(wv0033,wv0033_avg="value_avg")
wv0500<-rename(wv0500,wv0500="value")
wv0500<-rename(wv0500,wv0500_avg="value_avg")
wv1500<-rename(wv1500,wv1500="value")
wv1500<-rename(wv1500,wv1500_avg="value_avg")

dataframe_names
dataframe_vector<-as.vector(dataframe_names)

bdfi33$layer_name<-NULL
bdfi33$method_options<-NULL
bdfi33$licence<-NULL

for (i in dataframe_names) {
  x=get(i)
  x$layer_name<-NULL
  x$method_options<-NULL
  x$licence<-NULL
  x$positional_uncertainty<-NULL
  assign(i,x)
}

wosis<-data.frame()

for (j in dataframe_names) {
  x=get(j)
  wosis<-merge(wosis,x,all=TRUE)
  assign(j,x)
}

wosis_1<-wosis

wosis$country_name<-as.factor(wosis$country_name)
levels(wosis$country_name)
wosis$region<-as.factor(wosis$region)
levels(wosis$region)
wosis$continent<-as.factor(wosis$continent)
levels(wosis$continent)
wosis$profile_id<-as.factor(wosis$profile_id)
levels(wosis$profile_id)
profiles$profile_id<-as.factor(profiles$profile_id)
levels(profiles$profile_id)
wosis$profile_code<-as.factor(wosis$profile_code)
levels(wosis$profile_code)
profiles$profile_code<-as.factor(profiles$profile_code)
levels(profiles$profile_code)
wosis$upper_depth<-as.numeric(wosis$upper_depth)
max(wosis$upper_depth)


wosis<-merge(wosis,wv1500,by=c("X","Y","profile_id","profile_code"),all=TRUE)
?merge

test3<-merge(wv1500,wv0500,by=c("X","Y","organic_surface","dataset_id","country_name","region","continent","date"),all = TRUE)

test<-merge(wv1500,wv0500,all=TRUE)
test<-merge(test,wv0033,all=TRUE)
test<-merge(test,wv0010,all=TRUE)
test<-merge(test,bdfi33,all=TRUE)
test<-merge(test,bdfiad,all=TRUE)
test<-merge(test,bdfifm,all=TRUE)
test<-merge(test,bdwsod,all=TRUE)
test<-merge(test,cecph7,all=TRUE)
test<-merge(test,cecph8,all=TRUE)
test<-merge(test,cfgr,all=TRUE)
test<-merge(test,cfvo,all=TRUE)
test<-merge(test,clay,all=TRUE)
test<-merge(test,ecec,all=TRUE)
test<-merge(test,elco20,all=TRUE)
test<-merge(test,elco25,all=TRUE)
test<-merge(test,elco50,all=TRUE)
test<-merge(test,elcosp,all=TRUE)
test<-merge(test,nitkjd,all=TRUE)
test<-merge(test,orgc,all=TRUE)
test<-merge(test,orgm,all=TRUE)
test<-merge(test,phaq,all=TRUE)
test<-merge(test,phca,all=TRUE)
test<-merge(test,phetb1,all=TRUE)
test<-merge(test,phetm3,all=TRUE)
test<-merge(test,phetol,all=TRUE)
test<-merge(test,phkc,all=TRUE)
test<-merge(test,phnf,all=TRUE)
test<-merge(test,phprtn,all=TRUE)
test<-merge(test,phptot,all=TRUE)
test<-merge(test,phpwsl,all=TRUE)
test<-merge(test,silt,all=TRUE)
test<-merge(test,sand,all=TRUE)
test<-merge(test,tceq,all=TRUE)
test<-merge(test,totc,all=TRUE)
test<-merge(test,wg0006,all=TRUE)
test<-merge(test,wg0010,all=TRUE)
test<-merge(test,wg0033,all=TRUE)
test<-merge(test,wg0100,all=TRUE)
test<-merge(test,wg0200,all=TRUE)
test<-merge(test,wg0500,all=TRUE)
test<-merge(test,wg1500,all=TRUE)

hwsd <- raster("./HWSD_RASTER/hwsd.bil")
RG_SMU <- read_excel("~/RG SMU.xlsx")

#HWSD R Protocol
hwsd.zhnj <- crop(hwsd, extent(c(117.5, 119.5, 31, 33)))
nrow(hwsd.zhnj); ncol(hwsd.zhnj); bbox(hwsd.zhnj)
unique(hwsd.zhnj)

plot(hwsd.zhnj, col=bpy.colors(length(unique(hwsd.zhnj))))
xy <- click(hwsd.zhnj, n=1, id=TRUE, xy=TRUE, type="p")

ncol(hwsd)
#43200
nrow(hwsd)
#21600
res(hwsd)
#0.008333333 and 0.008333333
extent(hwsd)
#class      : Extent 
#xmin       : -180 
#xmax       : 180 
#ymin       : -90 
#ymax       : 90 
projection(hwsd)
#"+proj=longlat +datum=WGS84 +no_defs"
hwsd.zhnj<-crop(hwsd,extent(c(117.5,119.5,31,33)))
nrow(hwsd.zhnj)
#240
ncol(hwsd.zhnj)
#240
bbox(hwsd.zhnj)
#min   max
#s1 117.5 119.5
#s2  31.0  33.0
unique(hwsd.zhnj)
#[1] 11328 11331 11341 11365 11367 11368
#[7] 11372 11373 11375 11376 11377 11379
#[13] 11381 11389 11390 11391 11392 11394
#[19] 11434 11435 11460 11461 11466 11472
#[25] 11474 11476 11481 11483 11485 11486
#[31] 11488 11489 11490 11491 11492 11493
#[37] 11495 11499 11501 11513 11535 11604
#[43] 11605 11609 11613 11614 11615 11616
#[49] 11617 11619 11620 11621 11623 11625
#[55] 11627 11630 11634 11645 11649 11650
#[61] 11651 11652 11655 11656 11657 11661
#[67] 11663 11665 11667 11668 11671 11672
#[73] 11673 11675 11677 11678 11679 11680
#[79] 11814 11815 11817 11818 11823 11834
#[85] 11857 11858 11859 11860 11863 11870
#[91] 11875 11876 11877 11878 11925 11927
#[97] 11928 11929
plot(hwsd.zhnj,col=bpy.colors(length(unique(hwsd.zhnj))))
hwsd.zhnj3<-(hwsd.zhnj%/%100)
freq(hwsd.zhnj3)
#value count
#[1,]   113  5919
#[2,]   114  2152
#[3,]   115   273
#[4,]   116 32591
#[5,]   118 12536
#[6,]   119  4129
require(RColorBrewer)
plot(hwsd.zhnj3,col=brewer.pal(length(unique(hwsd.zhnj3)),"Accent"))
print(paste("UTM zone:", utm.zone<-floor((sum(bbox(hwsd.zhnj3)[1, ])/2+180)/6)+1))
#"UTM zone: 50"
proj4string.utm50<-
  paste("+proj=utm +zone=", utm.zone, "+datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",sep="")
hwsd.zhnj3.utm<-projectRaster(hwsd.zhnj3, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", method = "ngb")
unique(hwsd.zhnj3.utm)
#[1] 113 114 115 116 118 119
(cell.dim<-res(hwsd.zhnj3.utm))
#[1] 787 924
paste("Cell N dimension is ", round(((cell.dim[2]/cell.dim[1])-1)*100,1),"% larger than cell dimension E",sep = "")
#[1] "Cell N dimension is 17.4% larger than cell dimension E"
plot(hwsd.zhnj3.utm,col=brewer.pal(6,"Accent"),asp=1)
grid()
(cell.area<-cell.dim[1]*cell.dim[2]/10^4)
#[1] 72.7188
(tmp<-cbind(freq(hwsd.zhnj3.utm)[,1],freq(hwsd.zhnj3.utm)[,2]*cell.area/10^2))
#[,1]       [,2]
#[1,]  113  4280.2286
#[2,]  114  1570.7261
#[3,]  115   198.5223
#[4,]  116 23744.1426
#[5,]  118  9115.3016
#[6,]  119  2999.6505
#[7,]   NA  5189.9408
ix<-which(is.na(tmp[,1]))
sum(tmp[-ix,2])
#[1] 41908.57
rm(cell.dim,cell.area,tmp,ix)
rm(hwsd.zhnj3.utm)
library(maps)
library(mapdata)
str(tmp<-map('worldHires','Bhutan',fill = TRUE,plot = FALSE))
#List of 4
#$ x    : num [1:1666] 91.7 91.7 91.7 91.7 91.7 ...
#$ y    : num [1:1666] 27.8 27.8 27.8 27.8 27.8 ...
#$ range: num [1:4] 88.8 92.1 26.7 28.3
#$ names: chr "Bhutan"
#- attr(*, "class")= chr "map"