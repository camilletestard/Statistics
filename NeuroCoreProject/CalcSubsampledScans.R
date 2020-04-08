#Generate Random SubSampled Scans

calcRandomScans <- function(allScans) {
  
  #Only select group V and KK (pre and post hurricane data):
  PrePostScans = allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
  
  #Check which individuals are both in pre and post
  PreScans = PrePostScans[which(PrePostScans$isPost == 0),] ; total_obs_pre = nrow(PreScans)
  PostScans = PrePostScans[which(PrePostScans$isPost == 1),] ; total_obs_post = nrow(PostScans)
  
  #Subsample post-hurricane data to match pre-hurricane according to frequencies in numSamples_pre
  PostScans$subsampling_cat = paste(PostScans$focalID,PostScans$group,PostScans$Q,PostScans$timeBlock,sep=".")

  years = sort(unique(PreScans$year), decreasing=F) #unique years in increasing order
  SubPostScans = data.frame(); SubPreScans = data.frame() #initialize dataframes
  #count = 0
  for (y in 1:length(years)){ #for all years
    
    pre_scans = PreScans[which(PreScans$year == years[y]),] #Select pre-scans of that year
    pre_scans$subsampling_cat = paste(pre_scans$focalID,pre_scans$group,pre_scans$Q,pre_scans$timeBlock,sep=".") #create subsampling category ID/group/Q/timeBlock
    numSamples_pre = as.data.frame(table(pre_scans$subsampling_cat)) # find the number of samples of each category in pre-hurricane data of that year
    
    SubPostScansPerYear = data.frame(); #initialize dataframes
    for (cat in 1:nrow(numSamples_pre)){ #for all ID/group/Q/timeBlock pre-hurricane
      
      post_scans=NA #Initialize post-hurricane data for that year

      idxPre = which(pre_scans$subsampling_cat == as.character(numSamples_pre[cat,1])) #find occurrence of this ID/group/Q/timeBlock categeory in pre-hurricane data
      idxPost = which(as.character(PostScans$subsampling_cat) == as.character(numSamples_pre[cat,1])) #find occurrence of this ID/group/Q/timeBlock categeory in post-hurricane data
      
      if(length(idxPost)==0) { #if there is no match post-hurricane
        pre_scans <-pre_scans[-idxPre,] #Discard pre-hurricane observations from that category, from that year
      }
      else {
        
        if (length(idxPost)<length(idxPre)){ #if there are more observations pre-hurricane in that category, in that year
          idxPre_subSample = sample(idxPre,length(idxPre)-length(idxPost), replace=F) # randomly select a sub-sample of occurences pre-hurricane to match post-hurricane
          pre_scans = pre_scans[-idxPre_subSample,]#sub-sample pre-hurricane data of that category, that year
          post_scans = PostScans[idxPost,]
          post_scans$year = as.numeric(years[y])
        }
        
        else { #sub-sample randomly from post-hurricane data
          num_subsamples = numSamples_pre[cat,2];#round(runif(1,numSamples_pre[cat,2]-1,numSamples_pre[cat,2]+1))
          idxPost_subSample = sample(idxPost,num_subsamples, replace=F) #randomly select a sub-sample of occurences post-hurricane
          post_scans = PostScans[idxPost_subSample,] #sub-sample post-hurricane data of that category
          post_scans$year = as.numeric(years[y]) #change year for "corresponsing subsampling year" (instead of actual year of data collection)
        }
        SubPostScansPerYear = rbind(SubPostScansPerYear, post_scans) #Accumulate adjusted post-hurricane scans per category, per year
      }
    }
    SubPostScans = rbind(SubPostScans, SubPostScansPerYear) #Accumulate adjusted post-hurricane scans per year
    SubPreScans = rbind(SubPreScans, pre_scans) #Accumulate adjusted post-hurricane scans per year
  }  
  
  #Merge sub-sampled pre- and post-hurricane data
  SubPostScans$subsampling_cat = paste(SubPostScans$focalID,SubPostScans$group,SubPostScans$Q,SubPostScans$timeBlock,SubPostScans$isPost,sep=".")
  SubPreScans$subsampling_cat = paste(SubPreScans$focalID,SubPreScans$group,SubPreScans$Q,SubPreScans$timeBlock,SubPreScans$isPost,sep=".")
  SubScans = rbind(SubPostScans,SubPreScans)
  
  return(SubScans)
}
