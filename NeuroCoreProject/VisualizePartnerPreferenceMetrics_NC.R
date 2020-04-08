#Visualize change in partner preference.
library(ggplot2)
library(gridExtra)
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStats2.RData")

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 

##########################################################
#Sex partner preference
##########################################################

groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017")
for (gy in 1:length(groupyear)){ #For each group
  
  name.0 = paste(groupyear[gy],".0",sep="")
  data.0 = AllStats[[name.0]]; data.0$isPost = 0
  name.1 = paste(groupyear[gy],".1",sep="")
  data.1 = AllStats[[name.1]]; data.1$isPost = 1
  data= rbind(data.0, data.1)
  
  FFpair<-ggplot(data, aes(x= as.factor(isPost), y=eo.FF, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("FF Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. FF")+
    ylim(1,2.2)
  
  MMpair<-ggplot(data, aes(x= as.factor(isPost), y=eo.MM, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("MM Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. MM")+
    ylim(0,0.6)

  CrossPair<-ggplot(data, aes(x= as.factor(isPost), y=eo.cross, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Cross Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Cross")+
    ylim(0.5,1.3)
  
  Plot = grid.arrange(FFpair, MMpair, CrossPair, ncol=3)
  name = paste("SexPref",gy,sep="")
  assign(name,Plot)

}
FullPlot = grid.arrange(SexPref1,SexPref2,SexPref3,SexPref4,SexPref5,nrow=5)
ggsave(FullPlot, file = "SexPartnerPreference.png")

##########################################################
#Kinship preference
##########################################################

for (gy in 1:length(groupyear)){ #For each group
  
  name.0 = paste(groupyear[gy],".0",sep="")
  data.0 = AllStats[[name.0]]; data.0$isPost = 0
  name.1 = paste(groupyear[gy],".1",sep="")
  data.1 = AllStats[[name.1]]; data.1$isPost = 1
  data= rbind(data.0, data.1)
  
  CKpair <- ggplot(data, aes(x= as.factor(isPost), y=eo.ck, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Close Kin Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Close Kin")
  
  DKpair <- ggplot(data, aes(x= as.factor(isPost), y=eo.dk, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Distant Kin Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Distant Kin")
  
  Upair <- ggplot(data, aes(x= as.factor(isPost), y=eo.u, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Unrelated Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Unrelated")
  
  Plot = grid.arrange(CKpair, DKpair, Upair, ncol=3)
  name = paste("KinPref",gy,sep="")
  assign(name,Plot)
  
}
FullPlot = grid.arrange(KinPref1,KinPref2,KinPref3,KinPref4,KinPref5,nrow=5)
ggsave(FullPlot, file = "KinPartnerPreference.png")

##########################################################
#Rank preference
##########################################################

for (gy in 1:length(groupyear)){ #For each group
  
  name.0 = paste(groupyear[gy],".0",sep="")
  data.0 = AllStats[[name.0]]; data.0$isPost = 0
  name.1 = paste(groupyear[gy],".1",sep="")
  data.1 = AllStats[[name.1]]; data.1$isPost = 1
  data= rbind(data.0, data.1)
  
  HHpair <- ggplot(data, aes(x= as.factor(isPost), y=eo.HH, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("High Rank Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. High Rank")
  
  LLpair <- ggplot(data, aes(x= as.factor(isPost), y=eo.LL, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Low Rank Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Low Rank")
  
  Crosspair <- ggplot(data, aes(x= as.factor(isPost), y=eo.crossR, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Cross Rank Pair ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Obs./Exp. Cross Rank")
  
  Plot = grid.arrange(HHpair, LLpair, Crosspair, ncol=3)
  name = paste("RankPref",gy,sep="")
  assign(name,Plot)
  
}
FullPlot = grid.arrange(RankPref1,RankPref2,RankPref3,RankPref4,RankPref5,nrow=5)
ggsave(FullPlot, file = "KinPartnerPreference.png")