data<-read.csv("/disks/home/abigail/thesis-repository/code/data/data.2.csv")

slope<-c("slope.wg1500.n.prcp.xhigh","slope.wg1500.n.dry.days",
#"slope.wg1500.prcp.xhigh.length","slope.wg1500.n.dry.length",
	"slope.wg1500.n.tmax.xhigh","slope.wg1500.n.tmax.high",
#"slope.wg1500.n.tmax.xhigh.length","slope.wg1500.n.tmax.high.length",
	"slope.wg0200.n.prcp.xhigh","slope.wg0200.n.dry.days",
#"slope.wg0200.prcp.xhigh.length","slope.wg0200.n.dry.length",
        "slope.wg0200.n.tmax.xhigh","slope.wg0200.n.tmax.high",
#"slope.wg0200.n.tmax.xhigh.length","slope.wg0200.n.tmax.high.length",
	"slope.wg0033.n.prcp.xhigh","slope.wg0033.n.dry.days",
#"slope.wg0033.prcp.xhigh.length","slope.wg0033.n.dry.length",
        "slope.wg0033.n.tmax.xhigh","slope.wg0033.n.tmax.high")
#,"slope.wg0033.n.tmax.xhigh.length","slope.wg0033.n.tmax.high.length")

#property<- c("bdfi33","bdwsod","tceq","orgm","orgc","totc","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
#        "silt","sand","phca","phetb1","phetm3","phetol")

water.slope<-data.frame(
      slope=character(),property=character(),slope.2=numeric(),p.value=numeric(),r2=numeric(),r2.adj=numeric())

for(m in slope){
for(p in property){
       df<-data[,c(m,"bdfi33","bdwsod","tceq","orgm","orgc","totc","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","map","total.avg.tmax","total.avg.tmin")]
       colnames(df)<-c("slope","bdfi33","bdwsod","tceq","orgm","orgc","totc","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
        "silt","sand","phca","phetb1","phetm3","phetol","map","total.tmax.avg","total.tmin.avg")
if(nrow(df)<3) next
if(nrow(df)==0)next
       model<-lm(slope~bdfi33+bdwsod+tceq+orgm+orgc+totc+nitkjd+cecph7+ecec+elco50+cfgr+cfvo+clay+
	silt+sand+phca+phetb1+phetm3+phetol+map+total.tmax.avg+total.tmin.avg,data=df)
       sum<-summary(model)
       coef<-summary(model)$coefficients
if(nrow(coef)<2) next
       water.slope<-rbind(water.slope,data.frame(slope=m,property=p,slope.2=coef[,1],p.value=coef[,4],r2=sum$r.squared,r2.adj=sum$adj.r.squared))}}

water.slope$sig<-ifelse(water.slope$p.value<0.05,"yes","no")

