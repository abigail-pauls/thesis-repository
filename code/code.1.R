library(multcompView)

#data.1.stats<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

#ecosystems<-read.csv("/disks/home/abigail/thesis-repository/code/data/biomes.csv")

#biome<-ecosystems[,c(1,4,6,7)]
#colnames(biome)<-c("station_id","eco_name","biome_name","realm")

#biome <- biome %>% dplyr::mutate(biome = NA_real_)

#biome_map <- c(
#  "Boreal Forests/Taiga" = "boreal",
#  "Deserts & Xeric Shrublands" = "desert",
#  "Mediterranean Forests, Woodlands & Scrub" = "mediterranean",
#  "Montane Grasslands & Shrublands" = "montane.grassland",
#  "Temperate Broadleaf & Mixed Forests" = "temperate.forest",
#  "Temperate Conifer Forests" = "temperate.conifer",
#  "Temperate Grasslands, Savannas & Shrublands" = "temperate.grassland",
#  "Tropical & Subtropical Coniferous Forests" = "tropical.coniferous",
#  "Tropical & Subtropical Dry Broadleaf Forests" = "tropical.dry",
#  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "tropical.grassland",
#  "Tropical & Subtropical Moist Broadleaf Forests" = "tropical.moist",
#  "Tundra" = "tundra"
#)

#biome$biome <- biome_map[biome$biome_name]

#data.1.stats<-merge(data.1.stats,biome,all.x=TRUE)

#data.1.stats<-data.1.stats[,colSums(!is.na(data.1.stats))> 0]

#attribute<-c("sand","silt","clay","ecec","cecph7","elco50","cfgr","orgm","bdfifm","bdwsod")

data.1.stats$biome<-as.factor(data.1.stats$biome)

n.slopes<-c("nitkjd.n.dry.days.est","nitkjd.n.prcp.event.est","nitkjd.n.prcp.mean.est",
"nitkjd.n.tmax.xhigh.est","nitkjd.n.tmax.xhigh.length.est","nitkjd.n.tmax.xlow.est","nitkjd.n.tmax.xlow.length.est","nitkjd.n.tmax.avg.est",
"nitkjd.n.tmin.xhigh.est","nitkjd.n.tmin.xhigh.length.est","nitkjd.n.tmin.xlow.est","nitkjd.n.tmin.low.est","nitkjd.n.tmin.avg.est")

nitkjd.biomes<-data.frame(
		name=character(),
		Df=numeric(),
		f.value=numeric(),
		p.value=numeric())

nitkjd.tukey<-data.frame()

nitkjd.sig<-list()

for(ss in n.slopes){
	df<-data.1.stats[,c(ss,"biome")]
	colnames(df)<-c("slope","biome")
	df<-na.omit(df)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
	model<-aov(slope~biome,data=df)	
	stats<-summary(model)[[1]]
	nitkjd.biomes<-rbind(nitkjd.biomes,data.frame(
		name=ss,
		Df=stats$`Df`[1],
		f.value=stats$`F value`[1],
		p.value=stats$`Pr(>F)`[1]))
	post.hoc<-TukeyHSD(model)
	tukey<-as.data.frame(post.hoc$biome)
	tukey$name<-ss
	nitkjd.tukey<-rbind(nitkjd.tukey,tukey)
	nitkjd.sig[[ss]]<-multcompLetters4(model,post.hoc)}
nitkjd.biomes$signif<-ifelse(nitkjd.biomes$p.value<0.05,"yes","no")
nitkjd.tukey$signif<-ifelse(nitkjd.tukey$`p adj`<0.05,"yes","no")

o.slopes<-c("orgc.n.dry.days.est","orgc.n.tmax.xhigh.est","orgc.n.tmax.xhigh.length.est","orgc.n.tmax.high.est","orgc.n.tmin.high.est",
"orgc.n.tmin.high.length.est","orgc.n.tmin.xhigh.est","orgc.n.tmin.xhigh.length.est","orgc.n.tmin.xlow.est","orgc.n.tmin.low.est","orgc.n.tmin.avg.est")

orgc.biomes<-data.frame(
                name=character(),
                Df=numeric(),
                f.value=numeric(),
                p.value=numeric())

orgc.tukey<-data.frame()

orgc.sig<-list()

for(ss in o.slopes){
        df<-data.1.stats[,c(ss,"biome")]
        colnames(df)<-c("slope","biome")
        df<-na.omit(df)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
        model<-aov(slope~biome,data=df)
        stats<-summary(model)[[1]]
        orgc.biomes<-rbind(orgc.biomes,data.frame(
                name=ss,
                Df=stats$`Df`[1],
                f.value=stats$`F value`[1],
                p.value=stats$`Pr(>F)`[1]))
	post.hoc<-TukeyHSD(model)
        tukey<-as.data.frame(post.hoc$biome)
        tukey$name<-ss
        orgc.tukey<-rbind(orgc.tukey,tukey)
        orgc.sig[[ss]]<-multcompLetters4(model,post.hoc)}
orgc.biomes$signif<-ifelse(orgc.biomes$p.value<0.05,"yes","no")
orgc.tukey$signif<-ifelse(orgc.tukey$`p adj`<0.05,"yes","no")

t.slopes<-c("totc.n.dry.days.est","totc.n.tmax.xhigh.est","totc.n.tmax.xhigh.length.est","totc.n.tmax.high.est","totc.n.tmin.high.est","totc.n.tmin.high.length.est",
"totc.n.tmin.xhigh.est","totc.n.tmin.xhigh.length.est","totc.n.tmin.xlow.est","totc.n.tmin.low.est","totc.n.tmin.avg.est")            

totc.biomes<-data.frame(
                name=character(),
                Df=numeric(),
                f.value=numeric(),
                p.value=numeric())

totc.tukey<-data.frame()

totc.sig<-list()

for(ss in t.slopes){
        df<-data.1.stats[,c(ss,"biome")]
        colnames(df)<-c("slope","biome")
        df<-na.omit(df)
if(nrow(df)<3) next
if(length(levels(df$biome))<2)next
        model<-aov(slope~biome,data=df)
        stats<-summary(model)[[1]]
        totc.biomes<-rbind(totc.biomes,data.frame(
                name=ss,
                Df=stats$`Df`[1],
                f.value=stats$`F value`[1],
                p.value=stats$`Pr(>F)`[1]))
	post.hoc<-TukeyHSD(model)
        tukey<-as.data.frame(post.hoc$biome)
        tukey$name<-ss
        totc.tukey<-rbind(totc.tukey,tukey)
	totc.sig[[ss]]<-multcompLetters4(model,post.hoc)}
totc.biomes$signif<-ifelse(totc.biomes$p.value<0.05,"yes","no")
totc.tukey$signif<-ifelse(totc.tukey$`p adj`<0.05,"yes","no")

p.slopes<-c("phetb1.n.dry.days.est","phetb1.n.prcp.event.est","phetb1.n.prcp.mean.est","phetb1.n.tmax.xhigh.est","phetb1.n.tmax.xhigh.length.est",
"phetb1.n.tmax.xlow.est","phetb1.n.tmax.xlow.length.est","phetb1.n.tmax.avg.est","phetb1.n.tmin.xhigh.est","phetb1.n.tmin.xhigh.length.est","phetb1.n.tmin.xlow.est",
"phetb1.n.tmin.low.est","phetb1.n.tmin.avg.est")

phetb1.biomes<-data.frame(
                name=character(),
                Df=numeric(),
                f.value=numeric(),
                p.value=numeric())

phetb1.tukey<-data.frame()

phetb1.sig<-list()

for(ss in p.slopes){
        df<-data.1.stats[,c(ss,"biome")]
        colnames(df)<-c("slope","biome")
        df<-na.omit(df)
	df$biome <- droplevels(as.factor(df$biome))
	df$biome<-as.factor(df$biome)
if(nrow(df)<3) next
if(nlevels(df$biome)<2)next
        model<-aov(slope~biome,data=df)
        stats<-summary(model)[[1]]
        phetb1.biomes<-rbind(phetb1.biomes,data.frame(
                name=ss,
                Df=stats$`Df`[1],
                f.value=stats$`F value`[1],
                p.value=stats$`Pr(>F)`[1]))
	post.hoc<-TukeyHSD(model)
        tukey<-as.data.frame(post.hoc$biome)
        tukey$name<-ss
        phetb1.tukey<-rbind(phetb1.tukey,tukey)
        phetb1.sig[[ss]]<-multcompLetters4(model,post.hoc)}
phetb1.biomes$signif<-ifelse(phetb1.biomes$p.value<0.05,"yes","no")
phetb1.tukey$signif<-ifelse(phetb1.tukey$`p adj`<0.05,"yes","no")



