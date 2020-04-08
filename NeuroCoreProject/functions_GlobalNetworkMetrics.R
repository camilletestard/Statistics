###############################################
#Functions to generate networks from unique IDs
###############################################

#1. Output the Master Edgelist of all possible pairs given the unique IDs
calcMasterEL    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will take into account focal individuals in our social networks.
  alter <- NULL; ego <- NULL
  for(i in 1:length(unqIDs)){
    alter <- append(alter, rep(unqIDs[i], length(unqIDs) - i)) #append: add to the variable "alter"
    ego   <- append(ego  , unqIDs[(i+1):length(unqIDs)])
  }
  ego <- ego[1:length(alter)]
  
  masterEL <- data.frame(alter, ego)
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".")
  masterEL$count <- 0
  
  return(masterEL)
}

#2. Calculate Edgelist
calcEdgeList    <- function(rscans, masterEL){
  
  prox.partners = str_split(rscans$in.proximity, c(","), simplify = TRUE)
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,prox.partners)
  
  PP <- NULL  
  for(ii in 1:nrow(a)){ #for all observations
    for(p in 2:ncol(a)){ #for all proximity partners (not counting the first column, which is the focal ID column)
      if(!is.na(a[ii,p])) { #if not NA
        if(a[ii,p] != "" ) { #if not empty
          S1 <- data.frame(ego = as.character(a[ii, 1]), alter = as.character(a[ii, p]))
          PP <- dplyr::bind_rows(PP, S1) #bind rows, adds values to PP
        }
      }
    }
  }
  PP$conc1 <- paste(PP$ego,   PP$alter, sep=".")
  PP$conc2 <- paste(PP$alter, PP$ego  , sep=".")
  head(PP)
  head(masterEL)  
  
  for(i in 1:nrow(PP)){
    if(PP$conc1[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc1[i])] <- masterEL$count[which(masterEL$conc == PP$conc1[i])] +1 #Find the index and add counts
    }
    
    if(PP$conc2[i] %in% masterEL$conc){
      masterEL$count[which(masterEL$conc == PP$conc2[i])] <- masterEL$count[which(masterEL$conc == PP$conc2[i])] +1
    }
  }
  
  return(masterEL)
}


#3. Create igraph and tnet objects
createIG        <- function(edgelist, unqIDs, sexage){
  
  # create igraph object
  ig <- simplify(graph.data.frame(d=edgelist, directed = F), #create undirected igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete.edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0
  
  # Add in isolated individuals
  ig <- add.vertices(ig, length(unqIDs[which(!unqIDs %in% V(ig)$name)]), #find unique IDs that are not in ig vertices (i.e. isolated individuals whose edge weight are all 0)
                     attr = list(name = as.character(unqIDs[which(!unqIDs %in% V(ig)$name)]))) #add names of those isolated individuals
  
  # Add Sex to IG
  ig <- set.vertex.attribute(ig, name = "isFemale", value = sexage$isFemale[match(V(ig)$name, as.character(sexage$id))])
  
  # Add Age to IG
  ig <- set.vertex.attribute(ig, name = "age", value = sexage$age[match(V(ig)$name, as.character(sexage$id))])
  
  # Create tnet object
  tnet <- cbind(get.edgelist(ig, names=F), #get the list of edges from object ig
                E(ig)$weight) #get the weights of edges
  if(!is.directed(ig)){ #if ig is not directed, which is the case in proximity networks
    tnet <- symmetrise_w(tnet)} #make sure the network is symmetrical (i.e. undirected)
  tnet  <- as.tnet(tnet, type="weighted one-mode tnet") 
  
  # create female only networks
  igFem <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==0))    
  igFem <-  delete_vertex_attr(igFem, name="isFemale")
  
  tnetFem <- cbind(get.edgelist(igFem, names=FALSE), E(igFem)$weight)
  if(!is.directed(igFem)){tnetFem <- symmetrise_w(tnetFem)}
  tnetFem  <- as.tnet(tnetFem, type="weighted one-mode tnet") 
  
  # create male only networks: 
  igMal <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==1))    
  igMal <-  delete_vertex_attr(igMal, name="isFemale")
  
  tnetMal <- cbind(get.edgelist(igMal, names=FALSE), E(igMal)$weight)
  if (length(tnetMal)>0){
    if(!is.directed(igMal)){tnetMal <- symmetrise_w(tnetMal)} 
    tnetMal  <- as.tnet(tnetMal, type="weighted one-mode tnet") 
    netList <- list(ig, tnet, igFem, tnetFem, igMal, tnetMal)}
  else {
    netList <- list(ig, tnet, igFem, tnetFem)
  }
  
  return(netList)
}


###############################################
#Caculate Global network proporties
###############################################

# Calculate generic network values
calcGenStats <- function(netList){
  
  # total network
  dens    <- length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]]))) # num edges / all possible edges
  dens.w  <- sum(E(netList[[1]])$weight) * (length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]])))) #network density * sum of all weights
  
  #Compute gini coeff if network was completely random
  null_gini_coeff = as.numeric();
  for (p in 1:100){
    rg = sample_gnp(length(V(netList[[1]])), dens, directed = T, loops = FALSE)
    null_gini_coeff[p] = ineq::ineq(as.numeric(degree(rg)), "gini")}
  
  gini    <- ineq::ineq(as.numeric(degree(netList[[1]]))  , "gini")/mean(null_gini_coeff) #Compute gini coefficient as a measure of equality and scale by null gini (gini given a random network) 
  gini.w  <- ineq::ineq(as.numeric(strength(netList[[1]])), "gini")/mean(null_gini_coeff)
  kcomm   <- length(fastgreedy.community( #Find dense subgraph, also called communities in graphs
    delete.vertices(as.undirected(netList[[1]]), degree(netList[[1]]) == 0))[]) #delete vertices that have no edges
  clust.w <- as.numeric(tnet::clustering_w(netList[[2]])) #clustering coefficient
  
  # female network
  dens.f    <- length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]])))
  dens.f.w  <- sum(E(netList[[3]])$weight) * (length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]]))))
  
  #Compute gini coeff if network was completely random
  null_gini_coeff_f = as.numeric();
  for (p in 1:100){
    rg = sample_gnp(length(V(netList[[1]])), dens, directed = T, loops = FALSE)
    null_gini_coeff_f[p] = ineq::ineq(as.numeric(degree(rg)), "gini")}
  
  gini.f    <- ineq::ineq(as.numeric(degree(netList[[3]]))  , "gini")/mean(null_gini_coeff_f)    
  gini.f.w  <- ineq::ineq(as.numeric(strength(netList[[3]])), "gini")/mean(null_gini_coeff_f)
  kcomm.f   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[3]]), degree(netList[[3]]) == 0))[])
  clust.f.w <- as.numeric(tnet::clustering_w(netList[[4]]))
  
  if (length(netList)>4){
    # male network
    dens.m    <- length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]])))
    dens.m.w  <- sum(E(netList[[5]])$weight) * (length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]]))))
    gini.m    <- ineq::ineq(as.numeric(degree(netList[[5]]))  , "gini")/dens.m     
    gini.m.w  <- ineq::ineq(as.numeric(strength(netList[[5]])), "gini") /dens.m.w
    kcomm.m   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[5]]), degree(netList[[5]]) == 0))[]) 
    clust.m.w <- as.numeric(tnet::clustering_w(netList[[6]]))
  }
  else {
    dens.m = NA; dens.m.w = NA ; gini.m = NA ; gini.m.w = NA ; kcomm.m = NA ; clust.m.w = NA
  }
  df <- data.frame(dens, dens.w, gini, gini.w, kcomm, clust.w,
                   dens.f, dens.f.w, gini.f, gini.f.w, kcomm.f, clust.f.w,
                   dens.m, dens.m.w, gini.m, gini.m.w, kcomm.m, clust.m.w) 
  
  return(df)
}

###############################################
#Caculate partner preference
###############################################

# Calculate sex-based joint-counts
calcSexProps <- function(netList){
  # Classifying pairs as FF, MM or Opposite
  el            <- data.frame(get.edgelist(netList[[1]]), E(netList[[1]])$weight); colnames(el) <- c("ego", "alter", "weight") #tranform tnet object to dataframe
  el$isFemEgo   <- sexage$isFemale[match(as.character(el$ego),   sexage$id)]
  el$isFemAlter <- sexage$isFemale[match(as.character(el$alter), sexage$id)]
  el$pairClass  <- "opp"; el$pairClass[which(el$isFemEgo == 1 & el$isFemAlter == 1)] <- "bothFem"; el$pairClass[which(el$isFemEgo == 0 & el$isFemAlter == 0)] <- "bothMal"
  
  # Calculating Sex Pair weights
  weight.FF     <- sum(el$weight[el$pairClass=="bothFem"]) #sum of all the weights for class FF
  weight.MM     <- sum(el$weight[el$pairClass=="bothMal"]) #sum of all the weights for class MM                       
  weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF  
  
  #Compute expected proportions of pair classes if the network was completely random
  possFemPairs        <-    (length(V(netList[[3]]))^2)      - length(V(netList[[3]])) #all possible FF pairs
  possMalPairs        <-    (length(V(netList[[5]]))^2)      - length(V(netList[[5]])) #all possible MM pairs
  allPossiblePairs    <-    (length(V(netList[[1]]))^2)      - length(V(netList[[1]])) #all possible pairs
  possCrossPairs      <-    allPossiblePairs - (possFemPairs + possMalPairs) #all possible MF pairs
  
  #Weighted expected proportions
  exp.FF      <- (possFemPairs/allPossiblePairs)    * sum(el$weight)
  exp.MM      <- (possMalPairs/allPossiblePairs)    * sum(el$weight)
  exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  eo.FF     <- weight.FF/exp.FF
  eo.MM     <- weight.MM/exp.MM
  eo.cross  <- weight.cross/exp.cross
  
  sexPairStats <- data.frame(weight.FF, weight.MM, weight.cross, eo.FF, eo.MM, eo.cross) #tranform tnet object to dataframe
  
  #sexPairStats <- data.frame(FF, MM, cross, weight.FF, weight.MM, weight.cross) #tranform tnet object to dataframe
  return(sexPairStats)        
}

# Calculate kin-based joint-counts for female network only
calcKinProps <- function(netList, ped){
  
  el <- data.frame(get.edgelist(netList[[3]]), E(netList[[3]])$weight); colnames(el) <- c("ego", "alter", "weight") #tranform tnet object to dataframe
  
  KC      <- NULL; for(i in 1:length(el[,1])){ 
    KC[i] <-  ped[which(rownames(ped)==as.character(el$ego[i])) , which(colnames(ped)==as.character(el$alter[i]))]
  }
  el$KC   <- round(KC, 4)
  el$pairClass <- "unrelated"
  el$pairClass[which(el$KC >= .125 & el$KC < .25)] <- "dRel"
  el$pairClass[which(el$KC >= .25)] <- "rel"
  
  #Compute observed weight of related pair classes
  obs.ck     <- sum( el$weight[el$pairClass =="rel"])
  obs.dk     <- sum( el$weight[el$pairClass =="dRel"])
  obs.u      <- sum( el$weight[el$pairClass =="unrelated"])
  
  #Computer expected weight of related pair classes based on the distribution of kinship relationship in the whole population
  ckPairs    <- length(which(ped  >= .25))               ; exp.ck      <- (ckPairs   / length(ped))   * sum(el$weight)
  dkPairs    <- length(which(ped  >= .125 & ped <.25))   ; exp.dk      <- (dkPairs   / length(ped))   * sum(el$weight)
  uPairs     <- length(which(ped  <= .125))              ; exp.u       <- (uPairs    / length(ped))   * sum(el$weight)
  
  #Compute observed/expected ratio
  eo.ck      <- obs.ck    /   exp.ck
  eo.dk      <- obs.dk    /   exp.dk
  eo.u       <- obs.u     /   exp.u
  
  el$weightKC    <- el$weight * el$KC
  kinDegree      <- sum(el$weightKC) / sum(el$weight)
  
  kinPairStats <- data.frame(obs.ck, obs.dk, obs.u, eo.ck, eo.dk, eo.u, kinDegree)
  
  return(kinPairStats)
}
# Calculate Rank-based joint-counts for female network only
calcRankProps <- function(netList, dominance_info, year){
  
  #Get edgelist for female only network
  el <- data.frame(get.edgelist(netList[[3]]), E(netList[[3]])$weight); colnames(el) <- c("ego", "alter", "weight") #tranform tnet object to dataframe
  
  #Set high rank vs low rank
  dominance_info$ORD_RANK2 = "L"
  dominance_info$ORD_RANK2[which(dominance_info$X.DOMINATED>=70)]="H"
  # Classifying pairs as FF, MM or Opposite
  el$RankEgo   <- dominance_info$ORD_RANK2[match(paste(as.character(el$ego),year,sep=""), as.character(dominance_info$IDyear))]
  el$RankAlter <- dominance_info$ORD_RANK2[match(paste(as.character(el$alter),year,sep=""), as.character(dominance_info$IDyear))]
  el$pairClass  <- "opp"; el$pairClass[which(el$RankEgo == "H" & el$RankAlter == "H")] <- "bothH"; el$pairClass[which(el$RankEgo == "L" & el$RankAlter == "L")] <- "bothL"
  
  # Calculating Sex Pair weights
  weight.HH     <- sum(el$weight[el$pairClass=="bothH"]) #sum of all the weights for class FF
  weight.LL     <- sum(el$weight[el$pairClass=="bothL"]) #sum of all the weights for class MM                       
  weight.crossR  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF  
  
  #Compute expected proportions of pair classes if the network was completely random
  nodes = data.frame(matrix(0, nrow = length(V(netList[[3]])), ncol = 2)); names(nodes)=c("id","rank")
  nodes$id = as_ids(V(netList[[3]]))
  nodes$rank = dominance_info$ORD_RANK2[match(paste(nodes$id,year,sep=""), as.character(dominance_info$IDyear))]
  
  possHHPairs        <-    (length(which(nodes$rank=="H"))^2) - length(which(nodes$rank=="H")) #all possible HH pairs
  possLLPairs        <-    (length(which(nodes$rank=="L"))^2) - length(which(nodes$rank=="L"))  #all possible LL pairs
  allPossiblePairs    <-    (length(V(netList[[3]]))^2)      - length(V(netList[[3]])) #all possible pairs
  possCrossPairs      <-    allPossiblePairs - (possHHPairs + possLLPairs) #all possible MF pairs
  
  #Weighted expected proportions
  exp.HH      <- (possHHPairs/allPossiblePairs)    * sum(el$weight)
  exp.LL      <- (possLLPairs/allPossiblePairs)    * sum(el$weight)
  exp.crossR   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  eo.HH     <- weight.HH/exp.HH
  eo.LL     <- weight.LL/exp.LL
  eo.crossR  <- weight.crossR/exp.crossR
  
  RankPairStats <- data.frame(weight.HH, weight.LL, weight.crossR, eo.HH, eo.LL, eo.crossR) #tranform tnet object to dataframe
  
  #sexPairStats <- data.frame(FF, MM, cross, weight.FF, weight.MM, weight.cross) #tranform tnet object to dataframe
  return(RankPairStats)
}
