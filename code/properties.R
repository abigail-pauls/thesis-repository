pp<- c("bdfi33","bdwsod","tceq","orgm","orgc","totc","nitkjd","cecph7","ecec","elco50","cfgr","cfvo","clay",
	"silt","sand","phca","phetb1","phetm3","phetol","wg1500","wg0200","wg0033")

#prcp.result<-list()

#site<-data.one$station_id
#site<-unique(site)

#for(p in pp){
#	result<-list()
#for(s in site){
#	a<-data.one[data.one$station_id==s,c("station_id",p,"n.prcp.xhigh","n.prcp.event","n.dry.days")]
#	a<-na.omit(a)
#	colnames(a)<-c("station_id","metric","n.prcp.xhigh","n.prcp.event","n.dry.days")
#if(nrow(a)<3) next
#if(var(a$n.prcp.xhigh)==0||var(a$n.prcp.event)==0||var(a$n.dry.days)==0)next
#	model<-lm(metric~n.prcp.xhigh+n.prcp.event+n.dry.days,data=a)
#	result[[as.character(s)]]<-summary(model)$coefficients
#	}
#prcp.result[[p]]<-result
#}

result<-list()

for(p in pp){
	a<-data.one[,c("station_id",p,"n.prcp.xhigh","n.prcp.event","n.dry.days")]
        a<-na.omit(a)
        colnames(a)<-c("station_id","metric","n.prcp.xhigh","n.prcp.event","n.dry.days")
if(nrow(a)<3) next
if(var(a$n.prcp.xhigh)==0||var(a$n.prcp.event)==0||var(a$n.dry.days)==0)next
        model<-lmer(metric~n.prcp.xhigh+n.prcp.event+n.dry.days+(1|station_id),data=a)
	text<-Anova(mode,type="II")
	coef<-summary(model)
}


        #result[[p]]<-data.frame(summary(model)$coefficients
        #}

