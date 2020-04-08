#Load libraries
library(ggplot2)
library(ggpubr)

#Load AllScans file and Sumsampling function
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

ExSubScans = calcRandomScans(allScans) # Subsample Scans pre and post hurricane to make comparisons as clean as possible
unqIDs = as.character(unique(ExSubScans$focalID)) #Find the unique IDs in the subsampled scan

###################################################################
#For probability of being in proximity (or "accompanied"):
###################################################################

#Create isprox data frame from subsampled scan for later plotting
isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 4)); colnames(isProx)=c("prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){ #For all individuals
  id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individuals
  id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-re-hurricane data for that individuals
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
    count = count+1
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    count = count+1
    isProx$prob[count] = sum(id.all.post)/length(id.all.post)
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
  }
}
isProx$groupsex = paste(isProx$group,isProx$sex,sep=".")
isProx=isProx[-which(is.na(isProx$prob)),]

#plot box plots of p(Acc) (one data point = one individual)
ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")

#Plot distributions of p(Acc) from before and after the hurricane
hist(isProx$prob[which(isProx$isPost==0)],col=rgb(1,0,0,0.5), breaks=20, main="Individual p(Acc) pre- to post-hurricane",xlab="p(Acc)")
hist(isProx$prob[which(isProx$isPost==1)],col=rgb(0,1,1,0.5), breaks=20, add=T)
box()


###################################################################
#For probability of being ina grooming state (or "social")):
###################################################################

#Create isSocial data frame from subsampled scan for later plotting
isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 4)); colnames(isSocial)=c("prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){ #For all individuals
  id.all.pre = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #Find all observations pre-hurricane
  id.all.post = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)] #Find all observations post-hurricane
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #Create group attribute
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #Create sex attribute
  if (length(id.all.pre)>10) { #If there are more than 10 observations for that individual both pre- and post-hurricane
    count = count+1
    isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isSocial$isPost[count] = 0; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
    count = count+1
    isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
    isSocial$isPost[count] = 1; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
  }
}
isSocial$groupsex = paste(isSocial$group,isSocial$sex,sep=".")
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#plot box plots of p(Social) (one data point = one individual)
ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
  facet_grid(~group)

#Plot distributions of p(Social) from before and after the hurricane
hist(isSocial$prob[which(isProx$isPost==0)],col=rgb(1,0,0,0.5), breaks=20,main="Individual p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isProx$isPost==1)],col=rgb(0,1,1,0.5), breaks=20,add=T)
box()
