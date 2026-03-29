
library(data.table)
library(tidyverse)

#data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.stats.csv")

#data.one<-subset(data,data$soil.layer=="one")

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

#data.one<-merge(data.one,biome,all.x=TRUE)

#data.one<-data.one[,colSums(!is.na(data.one))> 0]

#setDT(data.one)

valid_var <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(FALSE)
  
  if (is.factor(x)) {
    return(nlevels(droplevels(x)) > 1)
  } else {
    return(length(unique(x)) > 1)
  }
}

slope.w<-c("wg1500.n.prcp.xhigh.length.est","wg1500.n.dry.days.est","wg1500.n.dry.length.est","wg1500.n.tmax.xhigh.est",
"wg1500.n.tmax.xhigh.length.est","wg1500.n.tmax.high.est","wg1500.n.tmax.high.length.est","wg0200.n.prcp.xhigh.est",
"wg0200.n.prcp.xhigh.length.est","wg0200.n.dry.days.est","wg0200.n.dry.length.est","wg0200.n.tmax.xhigh.est",
"wg0033.n.prcp.xhigh.est","wg0033.n.prcp.xhigh.length.est","wg0033.n.dry.days.est","wg0033.n.dry.length.est",
"wg0033.n.tmax.xhigh.est","wg0033.n.tmax.xhigh.length.est","wg0033.n.tmax.high.est","wg0033.n.tmax.high.length.est")


predictors <- c("tceq","orgm","orgc","totc","nitkjd","ecec","cfgr","cfvo","clay","silt",
                    "sand","phetol","phetm3","phetb1","phca","elco50","cecph7","bdwsod",
                    "bdfi33","wg0200","wg0033","map","total.avg.tmax","total.avg.tmin","biome")

valid_var <- function(x) {
  x <- x[!is.na(x)]
  length(unique(x)) > 1
}

#results_list <- list()

#for (m in slope.w) {
  
#  cols <- c(m, predictors)
#  df <- data.one[, ..cols]
  
#  if (nrow(df) < 3) next
  
#  setnames(df, old = m, new = "slope")
  
  # Initial valid predictors
#  valid <- predictors[sapply(df[, ..predictors], valid_var)]
#  if (length(valid) == 0) next
#  
#  df_model <- df[, c("slope", valid), with = FALSE]
#  df_model <- na.omit(df_model)
#  
#  if (nrow(df_model) < 3) next
  
  # Re-check validity after NA removal (important improvement)
#  valid2 <- valid[sapply(df_model[, ..valid], valid_var)]
#  if (length(valid2) == 0) next
  
#  formula <- as.formula(paste("slope ~", paste(valid2, collapse = "+")))
  
#  model <- try(lm(formula, data = df_model), silent = TRUE)
#  if (inherits(model, "try-error")) next
  
#  summod <- summary(model)
#  coef <- summod$coefficients
  
#  if (nrow(coef) < 2) next
  
#  results_list[[length(results_list) + 1]] <- data.table(
#    slope = rep(m, nrow(coef)),
#    property = rownames(coef),
#    slope.2 = coef[, 1],
#    p.value = coef[, 4],
#    r2 = summod$r.squared,
#    r2.adj = summod$adj.r.squared
#  )
#}

# Combine results
#final_results <- rbindlist(results_list, fill = TRUE)

#results_list <- vector("list", length(slope.w))
#counter <- 1

#for(m in slope.w){
#	cols <- c(m, predictors)
#	df <- data.one[, ..cols]
#if(nrow(df) < 3) next
#if(nrow(df)==0) next
#	setnames(df, old = m, new = "slope")
#valid <- predictors[sapply(df[, ..predictors], valid_var)]
#if(length(valid) == 0) next

#df_model <- df[, c("slope", valid), with = FALSE]
#df_model <- na.omit(df_model)

#if(nrow(df_model) < 3) next
#	formula <- as.formula(paste("slope ~", paste(valid, collapse = "+")))
#	model <- try(lm(formula, data = df_model), silent = TRUE)
#if(inherits(model, "try-error")) next
#	summod <- summary(model)
#	coef<-summod$coefficients
#if(nrow(coef) < 2) next
#results_list[[counter]] <- data.table(
#	slope = rep(m,nrow(coef)),
#	property = rownames(coef),
#	slope.2 = coef[,1],
#	p.value = coef[,4],
#	r2 = summod$r.squared,
#	r2.adj = summod$adj.r.squared)
#counter <- counter + 1}

#water.slope <- rbindlist(results_list, fill = TRUE)
#water.slope[, sig := fifelse(water.slope$p.value < 0.05, "yes", "no")]

#property.c<- c("map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

#carbon.slope<-data.frame(
#      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

water.slope<-data.frame()

for(m in slope.w){
	valid <- predictors[sapply(data.one[,predictors,with=FALSE], valid_var)]
if (length(valid) == 0) next
	cols<-c(m,valid)
	df<-data.one[,cols,with=FALSE]
	names(df)[names(df)==m]<-"slope"
	df<-subset(df,!is.na(df$slope))
	df<-droplevels(df)
if(nrow(df)<3) next
	valid1<-valid[sapply(df[,valid,with=FALSE],valid_var)]
if(length(valid1)==0) next
	formula <- as.formula(paste("slope ~", paste(valid1, collapse = "+")))
	model <-try(lm(formula, data = df),silent=TRUE)
if (inherits(model, "try-error")) next
	sum<-summary(model)
       coef<-coef(summary(model))
if(nrow(coef)<2) next
       water.slope<-rbind(water.slope,data.frame(slope=rep(m,nrow(coef)),property=rownames(coef),slope.2=coef[,1],
                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}
#water.slope$sig<-ifelse(water.slope$p.value<0.05,"yes","no")



#property.n<- c("map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

#nutrient.slope<-data.frame(
#      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(m in slope){
#for(p in property.n){
#       df<-data[,c(m,"map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")]
#       colnames(df)<-c("slope","map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")
#if(nrow(df)<3) next
#if(nrow(df)==0)next
#       model<-lm(slope~bdfi33+bdwsod+tceq+orgm+orgc+totc+nitkjd+cecph7+ecec+elco50+cfgr+cfvo+clay+
#        silt+sand+phca+phetb1+phetm3+phetol+map+total.tmax.avg+total.tmin.avg,data=df)
#       sum<-summary(model)
#       coef<-summary(model)$coefficients
#if(nrow(coef)<2) next
#       nutrient.slope<-rbind(nutrient.slope,data.frame(slope=rep(m,nrow(coef)),property=namerows(coef),slope.2=coef[,1],
#                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}}
#nutrient.slope$sig<-ifelse(nutrient.slope$p.value<0.05,"yes","no")



#property.t<- c("map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

#texture.slope<-data.frame(
#      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

#for(m in slope){
#for(p in property.t){
#       df<-data[,c(m,"map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")]
#       colnames(df)<-c("slope","map","total.avg.tmax","total.avg.tmin","bdfi33","bdwsod","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")
#if(nrow(df)<3) next
#if(nrow(df)==0)next
#       model<-lm(slope~bdfi33+bdwsod+tceq+orgm+orgc+totc+nitkjd+cecph7+ecec+elco50+cfgr+cfvo+clay+
#        silt+sand+phca+phetb1+phetm3+phetol+map+total.tmax.avg+total.tmin.avg,data=df)
#       sum<-summary(model)
#       coef<-summary(model)$coefficients
#if(nrow(coef)<2) next
#       texture.slope<-rbind(texture.slope,data.frame(slope=rep(m,nrow(coef)),property=namerows(coef),slope.2=coef[,1],
#                        p.value=coef[,4],r2=rep(sum$r.squared,nrow(coef)),r2.adj=rep(sum$adj.r.squared,nrow(coef))))}}
#texture.slope$sig<-ifelse(texture.slope$p.value<0.05,"yes","no")

