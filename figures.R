load("ISCAP_data.Rdata")
load("DEVA_data.Rdata")
load("AMAM_data.Rdata")

TickAll=rbind(COISCAPall,TickCODEVAall,TickCOAMAMall)
TickAll$Species=c(rep("Ixodes\nscapularis",nrow(COISCAPall)),rep("Dermacentor\nvariabilis",nrow(TickCODEVAall)),rep("Amblyomma\namericanum",nrow(TickCOAMAMall)))
save(TickAll,file="AllTickData.Rdata")

library(ggplot2)
library(tidyverse)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


names(TickAll)[17:18]=c("Nymphs","Adults")
TickAll_l=pivot_longer(TickAll,17:18,names_to = "Lifestage",values_to = "Median_Daily_per_Month")

ggplot(TickAll_l,aes(y=Median_Daily_per_Month,x=Average_Daily_Max_Temperature_per_Month,color=Region))+
         geom_point()+facet_grid(Lifestage~Species,scales = "free_y")

TickAll_l$Species_Lifestage=paste(TickAll_l$Species,TickAll_l$Lifestage,sep=" ")
tiff("Figure1.tif",height = 240,width = 171,res = 300,units = "mm")
ggplot(TickAll_l,aes(y=Median_Daily_per_Month,x=TimeFrame,fill=Region))+
  facet_grid(Species~Lifestage,scales = "free_y")+
  geom_point(position = position_jitterdodge(),alpha=0.8,aes(color=Region))+
  xlab("Season")+ylab("Rounded Median Daily per Month")+scale_color_manual(values=cbbPalette)
dev.off()

#########
#corrplot
load("AllTickData.Rdata")

AllDataN=pivot_wider(TickAll[,c(1:6,9,13:15,19,22)],names_from = "Species",values_from = "Rounded_Median_Daily_Nymphs_per_Month",names_prefix = "Nymphs")
AllDataA=pivot_wider(TickAll[,c(1:6,9,13:15,20,22)],names_from = "Species",values_from = "Rounded_Median_Daily_Adults_per_Month",names_prefix = "Adults")
AllData=cbind(AllDataN[4:13],AllDataA[,11:13])
names(AllData)=c("Region","Time\nFrame","Precip.","Tmax","DP","VPmax","VPmin",
                 "IXSC\nNymph","DEVA\nNymph","AMAM\nNymph","IXSC\nAdult","DEVA\nAdult","AMAM\nAdult")
AllData$Region=factor(AllData$Region)
AllData$`Time Frame`=factor(AllData$`Time Frame`)
AllData$`IXSC Adults`[is.na(AllData$`IXSC Adults`)]=0
AllData$`IXSC Nymphs`[is.na(AllData$`IXSC Nymphs`)]=0

library(GGally)
tiff("Figure2.tif",height = 240,width = 171,res = 300,units = "mm")
ggpairs(AllData,upper = list(continuous = wrap('cor', size = 2)) )+theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


