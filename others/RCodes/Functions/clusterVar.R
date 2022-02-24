############################################################################################

## Compute the variance of a given cluster 

############################################################################################

# Arguments:
# prob the probability vector of gray levels 
# clust a cluster of the gray levels
# start 0 or 1 for gray levels 0,...,L-1 or 1,...,L, respectively 


# Value:
# clusterVar returns an object with class "numeric" 

############################################################################################

clusterVar <- function(prob, clust, start){
  
  # Compute the cluster probability
  
  if (start == 0){
    clusterProb <- sum(prob[clust+1])
  }
  
  if (start == 1){
    clusterProb <- sum(prob[clust])
  }
  
  # Compute the cluster mean
  
  mu <- clusterMean(prob, clust, start)
  
  # Initialize
  
  term <- vector()
  
  # Compute the variance of each element in the cluster
  
  if (start == 0){
    for (j in (clust+1)){
      term[j] <- (j-1-mu)^2*prob[j]
    }
  }
  
  if (start == 1){
    for (j in (clust)){
      term[j] <- (j-mu)^2*prob[j]
    }
  }
  
  # Compute the variance of the cluster
  
  vc <- sum(term, na.rm = T)/clusterProb
  
  # Output
  
  return(vc)
}

