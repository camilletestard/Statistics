#Generating Social Network graphs

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")

#Load scan data
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

hrs=5 #Set number of hours of observations (arbitrary)

# 1. Calculate random subsamples
randomScans = calcRandomScans(allScans)

#Plot social netowrk graphs for each group, each year separately: 
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
        
        #Need to upload an adjacency matrix, rather than socprog style data...
        adjMat = dils::AdjacencyFromEdgelist(weightedEL)
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read adjacency matrix
        m=as.matrix(data) # coerces the data set as a matrix
        am.g=graph.adjacency(m,mode="directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
        
        #increase space between nodes if overlapping
        #fruchterman reingold layout
        l <- layout.fruchterman.reingold(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
        #changes size of labels of vertices
        V(am.g)$label.cex <- 0.5
        
        #plot graph
        png(paste("Social Network ",group[g],years[y],".",isPost[h],".png",sep=""))
        plot.igraph(am.g,layout=l, vertex.label=V(am.g)$name, vertex.color="CYAN1", vertex.size=7,edge.color="grey20", 
                    edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5, main = paste("Social Network ",group[g],years[y],".",isPost[h],sep=""))
        dev.off()

      }
    }
  }
}  