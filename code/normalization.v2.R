library(tidyverse)
library(data.table)

final<-read.csv("/disks/home/abigail/thesis-repository/code/data/final.csv")
#final<-soil

final$soil.layer<-with(final,ifelse(upper_depth<=20,"one",
       ifelse(upper_depth>=21&upper_depth<=40,"two",
      ifelse(upper_depth>=41&upper_depth<=60,"three",
     ifelse(upper_depth>=61&upper_depth<=80,"four",
    ifelse(upper_depth>=81&upper_depth<=100,"five","six"))))))

final$soil.layer<-as.factor(final$soil.layer)

final<-final[final$soil.layer=="one",]

norm.prcp<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.prcp.csv")
norm.prcp.1<-norm.prcp[,-c(1)]

norm.tmax<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.tmax.csv") 
norm.tmax.1<-norm.tmax[,-c(1)] 
names(norm.tmax.1)[names(norm.tmax.1)=="tmax.avg"]<-"tmax.avg.u"

norm.tmin<-read.csv("/disks/home/abigail/thesis-repository/code/data/norm.tmin.csv")
norm.tmin.1<-norm.tmin[,-c(1)]
names(norm.tmin.1)[names(norm.tmin.1)=="tmin.avg"]<-"tmin.avg.u"

final<-merge(final,norm.prcp.1,all.x=TRUE,by.x="station_id",by.y="station_id")
final<-merge(final,norm.tmax.1,all.x=TRUE)
final<-merge(final,norm.tmin.1,all.x=TRUE)

final$n.prcp.avg<-(final$prcp.avg-final$prcp.avg.u)/final$prcp.avg.sd

final$n.prcp.mean<-((final$prcp.mean-final$prcp.mean.u)/final$prcp.mean.sd)

final$n.prcp.event<-((final$prcp.events-final$prcp.event.u)/final$prcp.event.sd)

final$n.dry.days<-((final$dry.days-final$dry.days.u)/final$dry.days.u)

final$n.prcp.xhigh<-((final$prcp.xhigh-final$prcp.xhigh.u)/final$prcp.xhigh.sd)

final$n.prcp.high<-((final$prcp.high-final$prcp.high.u)/final$prcp.xhigh.sd)

final$n.tmax.avg<-((final$tmax.avg-final$tmax.avg.u)/final$tmax.sd)

final$n.tmax.xhigh<-((final$tmax.xhigh-final$tmax.xhigh.u)/final$tmax.xhigh.sd)

final$n.tmax.high<-((final$tmax.high-final$tmax.high.u)/final$tmax.high.sd)

final$n.tmax.xlow<-((final$tmax.xlow-final$tmax.xlow.u)/final$tmax.xlow.sd)

final$n.tmax.low<-((final$tmax.low-final$tmax.low.u)/final$tmax.low.sd)

final$n.tmin.avg<-((final$tmin.avg-final$tmin.avg.u)/final$tmin.sd)

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

ecosystems<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.csv")

biome<-ecosystems[,c(1,4,6,7)]
colnames(biome)<-c("station_id","eco_name","biome_name","realm")

biome <- biome %>% dplyr::mutate(biome = NA_real_)

biome_map <- c(
  "Boreal Forests/Taiga" = "boreal",
  "Deserts & Xeric Shrublands" = "desert",
  "Mediterranean Forests, Woodlands & Scrub" = "mediterranean",
	"Montane Grasslands & Shrublands" = "montane.grassland",
  "Temperate Broadleaf & Mixed Forests" = "temperate.forest",
  "Temperate Conifer Forests" = "temperate.conifer",
  "Temperate Grasslands, Savannas & Shrublands" = "temperate.grassland",
  "Tropical & Subtropical Coniferous Forests" = "tropical.coniferous",
  "Tropical & Subtropical Dry Broadleaf Forests" = "tropical.dry",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "tropical.grassland",
  "Tropical & Subtropical Moist Broadleaf Forests" = "tropical.moist",
  "Tundra" = "tundra")

biome$biome <- biome_map[biome$biome_name]

data<-merge(final,biome,all.x=TRUE)

data<-data[,-c(2:7,10,13,19:23,40,42,45,78:139,167:169)]

write.csv(data,"/disks/home/abigail/thesis-repository/code/data/data.20260426.csv")

