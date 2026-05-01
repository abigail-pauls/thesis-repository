#filtering WoSIS soil datasets by HWSD leptosol regions, combining all WoSIS soil data into one data set, cleaning up dataset, 
#assigning NOAA GHCN stations to WoSIS soil data sites

#load necessary R packages
library(readxl)
library(data.table)
library(tidyverse)


r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
          resolution = c(0.05,0.05), crs = "EPSG:4326")

grid <- as.polygons(r)

grid$cell_id <- 1:nrow(grid)

hwsd.sp <- rast(hwsd)

soil_resampled <- resample(hwsd.sp, r, method = "near")

zonal_stats <- zonal(hwsd.sp, grid, fun = "modal", na.rm = TRUE)

write.csv(zonal_stats,"zonalstats.csv", row.names = FALSE)

grid$dominant_soil_unit <- zonal_stats$hwsd

grid.df<-as.data.frame(grid)

write.csv(grid.df,"grid_SMU.csv", row.names = FALSE)

grided_RG_SMU<-merge(grid.df,RG_SMU,by.x="dominant_soil_unit",by.y="HWSD2_SMU_ID")

grid_SMU <- read.csv("~/Fall 2025/THESIS/THESIS/grid_SMU.csv")

r <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
          resolution = c(0.05, 0.05), crs = "EPSG:4326")

cells <- 1:ncell(r)

xy <- xyFromCell(r, cells)

halfres <- res(r) / 2

bounds <- data.frame(
  cell_id = cells,
  xmin = xy[, 1] - halfres[1],
  xmax = xy[, 1] + halfres[1],
  ymin = xy[, 2] - halfres[2],
  ymax = xy[, 2] + halfres[2]
)

smu_latlon<-merge(grid_SMU,bounds,by="cell_id")

write.csv(smu_latlon,"smu_latlon.csv", row.names = FALSE)

#upload HWSD soil classification data
smu_latlon<-read.csv("/disks/home/abigail/thesis-repository/code/data/smu_latlon.csv")
hwsd <- read_excel("/disks/home/abigail/thesis-repository/code/data/HWSD2_LAYERS.xlsx")

#filter smu_latlon to only include cells with leptosol dominant soils
hwsd_smaller<-with(hwsd,subset(hwsd,select=c(HWSD2_SMU_ID,WRB2)))
hwsd_SMU_lp<-with(hwsd_smaller,subset(hwsd_smaller,WRB2=="LP"))
hwsd_SMU_lp<-unique(hwsd_SMU_lp)
lp_smu_latlon<-merge(hwsd_SMU_lp,smu_latlon,by.x="HWSD2_SMU_ID",by.y="dominant_soil_unit",all=FALSE)

setDT(lp_smu_latlon)

soil<-c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","clay","silt","sand","wg1500",
	"phca","elco50","bdwsod","bdfifm")

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

test<-list("orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt","sand","phaq","phetol","wg1500","phpwsl","phptot","phprtn","phnf","phkc",
	"phetm3","phetb1","phca","elcosp","elco50","elco25","elco20","cecph7","bdwsod","bdfifm","bdfiad","bdfi33","wg0500","wg0200","wg0100","wg0033","wg0010",
	"wv1500","wv0500","wv0033","wv0010")

check<-data.frame(var=character(),orig=numeric(),final=numeric())

for(i in test){
	final<-paste0(i,"_avg",sep="")
check<-rbind(check,data.frame(var=i,orig=nrow(!is.na(soil.data.1[[i]])),final=sum(!is.na(lp.all[[final]]))))
}

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

soil<-soil %>% filter(YEAR >= 1964, YEAR <= 2014)

soil<-soil %>% mutate(start.date = date %m-% years(1))

soil[,c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","clay","silt","sand","wg1500",
       "phca","elco50","bdwsod","bdfifm")]<-NULL

names(soil)[names(soil)=="tceq_avg"]<-"tceq"
names(soil)[names(soil)=="orgm_avg"]<-"orgm"
names(soil)[names(soil)=="orgc_avg"]<-"orgc"
names(soil)[names(soil)=="nitkjd_avg"]<-"nitkjd"
names(soil)[names(soil)=="ecec_avg"]<-"ecec"
names(soil)[names(soil)=="cfgr_avg"]<-"cfgr"
names(soil)[names(soil)=="clay_avg"]<-"clay"
names(soil)[names(soil)=="silt_avg"]<-"silt"
names(soil)[names(soil)=="sand_avg"]<-"sand"
names(soil)[names(soil)=="wg1500_avg"]<-"wg1500"
names(soil)[names(soil)=="min_lon"]<-"noaa.lon"
names(soil)[names(soil)=="min_lat"]<-"noaa.lat"
names(soil)[names(soil)=="latitude"]<-"wosis.lat"
names(soil)[names(soil)=="longitude"]<-"wosis.lon"
names(soil)[names(soil)=="elevation"]<-"noaa.elev"
names(soil)[names(soil)=="totc_avg"]<-"totc"
names(soil)[names(soil)=="elco50_avg"]<-"elco50"
names(soil)[names(soil)=="phca_avg"]<-"phca"
names(soil)[names(soil)=="bdfifm_avg"]<-"bdfifm"
names(soil)[names(soil)=="bdwsod_avg"]<-"bdwsod"

write.csv(soil,"/disks/home/abigail/thesis-repository/code/data/soil.final.csv")
