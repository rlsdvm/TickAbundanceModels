library(bnlearn)
library(DT)
install.packages("BiocManager")
BiocManager::install("Rgraphviz")

##All
load("AllTickData.Rdata")
library(tidyr)

AllDataN=pivot_wider(TickAll[,c(1:6,9,13:15,19,22)],names_from = "Species",values_from = "Rounded_Median_Daily_Nymphs_per_Month",names_prefix = "Nymphs")
AllDataA=pivot_wider(TickAll[,c(1:6,9,13:15,20,22)],names_from = "Species",values_from = "Rounded_Median_Daily_Adults_per_Month",names_prefix = "Adults")
AllData=cbind(AllDataN[4:13],AllDataA[,11:13])
names(AllData)=c("Region","Time Frame","Precipitation","T[max]","DP","VP[max]","VP[min]",
                 "IXSC Nymphs","DEVA Nymphs","AMAM Nymphs","IXSC Adults","DEVA Adults","AMAM Adults")
wl=cbind(names(AllData)[c(rep(3,3))], #precip->humidity,dewpoint,VP
         names(AllData)[c(5:7)])
bl=cbind(names(AllData)[c(2:13,1,3:13,rep(8:13,each=5))], 
         #nothing into region or time frame, ticks not influencing weather
         names(AllData)[c(rep(1,12),rep(2,12),9:13,8,10:13,8:9,11:13,8:10,12:13,8:11,13,8:12)])
colnames(wl)=colnames(bl)=c("from","to")
wl=as.data.frame(wl)
bl=as.data.frame(bl)
AllData$Region=factor(AllData$Region)
AllData$`Time Frame`=factor(AllData$`Time Frame`)
AllData$`IXSC Adults`[is.na(AllData$`IXSC Adults`)]=0
AllData$`IXSC Nymphs`[is.na(AllData$`IXSC Nymphs`)]=0

All.dag=tabu(AllData,blacklist = bl,whitelist = wl)
gR=graphviz.plot(All.dag,shape = "rectangle",render = F)
graph::nodeRenderInfo(gR)<-list(fontsize=25)
Rgraphviz::renderGraph(gR)

str.sl=boot.strength(AllData,algorithm = "hc",algorithm.args = list(whitelist=wl,blacklist=bl))
datatable(str.sl[which(str.sl$strength > 0.675),], editable = TRUE)
attr(str.sl,"threshold")
avg.sl=averaged.network(str.sl)
strength.plot(avg.sl,str.sl,shape = "ellipse")
par(mfrow=c(1,2))
graphviz.compare(avg.sl,All.dag,shape = "ellipse",main = c("averaged","single"))
compare(avg.sl,All.dag,arcs = T)
undirected.arcs(cpdag(All.dag,wlbl = T))
compare(cpdag(avg.sl,wlbl=T),cpdag(All.dag,wlbl=T),arcs = T)
par(mfrow=c(1,1))
plot(str.sl)
abline(v=0.275,col=3,lty=2,lwd=2)
nrow(str.sl[str.sl$strength>attr(str.sl,"threshold")&str.sl$direction>0.275,])
avg.simpler=averaged.network(str.sl,threshold = 0.275)
strength.plot(avg.simpler,str.sl,shape = "ellipse")

fitted.avg=bn.fit(avg.simpler,AllData)
save(fitted.avg,file = "fittedBN_All.Rdata")
write.csv(amat(fitted.avg),file="fittedBN_All.csv")


outtable=matrix(NA,ncol=11,nrow=47)
regTF=paste(rep(levels(AllData$Region),2),rep(levels(AllData$`Time Frame`),each=2),sep = ";")
TF=levels(AllData$`Time Frame`)
rownames(outtable)=c(TF,regTF,levels(AllData$Region),
                     paste(regTF,"Precipitation",sep = ";"),paste(levels(AllData$Region),"Precipitation",sep = ";"),paste(TF,"Precipitation",sep = ";"),
                     paste(regTF,"T[max]",sep = ";"),paste(levels(AllData$Region),"T[max]",sep = ";"),paste(TF,"T[max]",sep = ";"),
                     paste(regTF,"DP",sep = ";"),paste(TF,"DP",sep = ";"),
                     paste(regTF,"VP[max]",sep = ";"),paste(levels(AllData$Region),"VP[max]",sep = ";"),paste(TF,"VP[max]",sep = ";"),
                     paste(regTF,"VP[min]",sep = ";"),paste(levels(AllData$Region),"VP[min]",sep = ";"),paste(TF,"VP[min]",sep = ";"),
                     "Intercept")
colnames(outtable)=colnames(AllData)[3:13]
outtable[1:2,1]=fitted.avg$Precipitation$coefficients
outtable[1:2,2]=fitted.avg$`T[max]`$coefficients[1,]
outtable[15:16,2]=fitted.avg$`T[max]`$coefficients[2,]
outtable[29:30,2]=fitted.avg$`T[max]`$coefficients[3,]
outtable[45:46,2]=fitted.avg$`T[max]`$coefficients[4,]
outtable[3:6,3]=fitted.avg$DP$coefficients[1,]
outtable[9:12,3]=fitted.avg$DP$coefficients[2,]
outtable[39:42,3]=fitted.avg$DP$coefficients[3,]
outtable[3:6,4]=fitted.avg$`VP[max]`$coefficients[1,]
outtable[9:12,4]=fitted.avg$`VP[max]`$coefficients[2,]
outtable[17:20,4]=fitted.avg$`VP[max]`$coefficients[3,]
outtable[25:28,4]=fitted.avg$`VP[max]`$coefficients[4,]
outtable[39:42,4]=fitted.avg$`VP[max]`$coefficients[5,]
outtable[7:8,5]=fitted.avg$`VP[min]`$coefficients[1,]
outtable[13:14,5]=fitted.avg$`VP[min]`$coefficients[2,]
outtable[47,6]=fitted.avg$`IXSC Nymphs`$coefficients
outtable[47,7]=fitted.avg$`DEVA Nymphs`$coefficients
outtable[1:2,8]=fitted.avg$`AMAM Nymphs`$coefficients[1,]
outtable[1:2,9]=fitted.avg$`IXSC Adults`$coefficients[1,]
outtable[35:36,9]=fitted.avg$`IXSC Adults`$coefficients[2,]
outtable[7:8,10]=fitted.avg$`DEVA Adults`$coefficients[1,]
outtable[13:14,10]=fitted.avg$`DEVA Adults`$coefficients[2,]
outtable[43:44,10]=fitted.avg$`DEVA Adults`$coefficients[3,]
outtable[7:8,11]=fitted.avg$`AMAM Adults`$coefficients[1,]
outtable[13:14,11]=fitted.avg$`AMAM Adults`$coefficients[2,]
outtable=signif(outtable,digits=3)
write.csv(outtable,file="BNoutputTable.csv")

#No precipitation
AllDataNP=AllData[,c(1:2,4:13)]
wl=NULL
bl=bl[-c(2,14),]

All.dag=tabu(AllDataNP,blacklist = bl,whitelist = wl)
gR=graphviz.plot(All.dag,shape = "rectangle",render = F)
graph::nodeRenderInfo(gR)<-list(fontsize=25)
Rgraphviz::renderGraph(gR)

str.sl=boot.strength(AllDataNP,algorithm = "hc",algorithm.args = list(whitelist=wl,blacklist=bl))
datatable(str.sl[which(str.sl$strength > 0.675),], editable = TRUE)
attr(str.sl,"threshold")
avg.sl=averaged.network(str.sl)
strength.plot(avg.sl,str.sl,shape = "ellipse")
par(mfrow=c(1,2))
graphviz.compare(avg.sl,All.dag,shape = "ellipse",main = c("averaged","single"))
compare(avg.sl,All.dag,arcs = T)
undirected.arcs(cpdag(All.dag,wlbl = T))
compare(cpdag(avg.sl,wlbl=T),cpdag(All.dag,wlbl=T),arcs = T)
par(mfrow=c(1,1))
plot(str.sl)
abline(v=0.675,col=3,lty=2,lwd=2)
nrow(str.sl[str.sl$strength>attr(str.sl,"threshold")&str.sl$direction>0.675,])
avg.simpler=averaged.network(str.sl,threshold = 0.4)
strength.plot(avg.simpler,str.sl,shape = "ellipse")

