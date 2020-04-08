#GENERATE GLOBAL NETOWRK METRICS

# load libraries
library(dplyr)
library(igraph)
library(tnet)
library(ineq)
library(stringr)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")
source("Social_Network_Analysis/KinshipPedigree.R")

#Load scan data and population info
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")

#Compute pedigree for all IDs in this group
FemID = allScans$focalID[which(allScans$sex=="F" & (allScans$group == "KK"|allScans$group == "V"))];
groupIDs = as.character(unique(FemID))
IDmatch = match(groupIDs, as.character(bigped$ID)); discard.na = which(is.na(IDmatch))
if(length(discard.na)!=0) 
{pedigree = bigped[IDmatch[-discard.na],c("ID","DAM","SIRE")]
  } else {pedigree = bigped[IDmatch,c("ID","DAM","SIRE")]}
ped <- KinshipPedigree(pedigree)

#Load Dominance info
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

num_iter = 100; hrs <- 5 #sample(1:10, 1) #randomly sample a number of hours followed.
AllStats = list()
# GlobalStats = as.data.frame(matrix(NA, ncol = 18, nrow = 1))
# names(GlobalStats) = c("dens","dens.w","gini","gini.w kcomm","clust.w","dens.f","dens.f.w","gini.f",
#                        "gini.f.w", "kcomm.f", "clust.f.w","dens.m","dens.m.w","gini.m","gini.m.w",
#                        "kcomm.m", "clust.m.w")
# SexPairStats
# KinPairStats

start_time <- Sys.time()
for (iter in 40:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  
  # 1. Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  
  #For each group, each year separately: 
  group = c("V","KK")
  
  for (g in 1:length(group)){ #For each group
    randscansG = randomScans[which(randomScans$group==group[g]),] 
    
    years = unique(randscansG$year)
    for (y in 1:length(years)){ #For each year in that group
      randscansY = randscansG[which(randscansG$year==years[y]),] 
      year = years[y]
      
      isPost = c(0,1)
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
 
        rscans = randscansY[which(randscansY$isPost==isPost[h]),] 
        # 2. Find the number of unique IDs.
        #Find all unique IDs
        unqIDs = unique(c(as.character(rscans$focalID)))
        
        # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
        masterEL = calcMasterEL(unqIDs)
        
        # 4. Output weighted edgelist from the Master Edgelist.
        options(warn = -1) #set options to ignore all warnings
        weightedEL = calcEdgeList(rscans,masterEL)
        weightedEL$weight <- round(weightedEL$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
        weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
        
        if (years[y]!=2013) { #KK2013 has to be excluded
          
          # 5. Generate undirected, symmetrical graphs from proximity scans (has sex and age vertices attributes)
          sexage= data.frame()
          for (i in 1:length(unqIDs)){
            idx = which(rscans$focalID == unqIDs[i])
            sexage[i,c("id","sex","age")] <- c(unqIDs[i],as.character(rscans$sex[idx[1]]),rscans$age[idx[1]])
          }
          sexage$isFemale = 0; sexage$isFemale[which(sexage$sex == "F")]=1;
          options(warn = -1)
          netList <- createIG(weightedEL, unqIDs, sexage)
          
          # 6. Calculate global network statistics, for whole network, male only and female only.
          GlobalStats <- calcGenStats(netList)
        
          # 7. Calculate partner sex preference, for whole network
          SexPairStats = calcSexProps(netList)
          
          # 8. Calculate partner kin preference, for female-only network
          KinPairStats <- calcKinProps(netList, ped) 
          
          # 9. Calculate partner rank preference, for female-only network
          RankPairStats <- calcRankProps(netList, dominance_info, year)
          
          # 10. Combine all stats and save
          AllStatsDF <- bind_cols(GlobalStats, SexPairStats, KinPairStats, RankPairStats)
          name = paste(group[g],years[y],isPost[h],sep=".")
          
          AllStats[[name]] = rbind(AllStats[[name]], AllStatsDF)  
          save(AllStats,file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStats2.RData")
          
        } #end of "if" clause about network weights >0
      } #end of of pre-/post-hurricane loop
    } #end of year for loop
  } #end of group for loop
} #end of iter for loop


end_time <- Sys.time()
end_time - start_time