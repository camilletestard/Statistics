# Visualize Global Network Metrics
library(ggplot2)
library(gridExtra)
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStats2.RData")
AllStats[["KK.2013.1"]]= NULL
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 

groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017")
for (gy in 1:length(groupyear)){ #For each group
  name.0 = paste(groupyear[gy],".0",sep="")
  data.0 = AllStats[[name.0]]; data.0$isPost = 0
  name.1 = paste(groupyear[gy],".1",sep="")
  data.1 = AllStats[[name.1]]; data.1$isPost = 1
  data= rbind(data.0, data.1)
  
  name <-paste("density",gy,sep="")
  plot <-ggplot(data, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Density ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
    ylim(0, 0.35)
  assign(name, plot)
 } 

FullPlot = grid.arrange(density1, density2, density3,density4,density5, ncol=3, nrow=2)

ggsave(FullPlot, file = paste("DensityPlots.png",sep=""))