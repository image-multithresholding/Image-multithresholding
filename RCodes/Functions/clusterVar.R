############################################################################################

## Compute the variance of a given cluster 

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# clust a clust of the gray levels

# Value:
# clusterVar returns an object with class "numeric" 

############################################################################################

clusterVar <- function(prob, clust){
  
  # Compute the cluster probability
  
  clusterProb <- sum(prob[clust+1])
  
  # Compute the cluster mean
  
  mu <- clusterMean(prob, clust)
  
  # Initialize
  
  term <- vector()
  
  # Compute the variance of each element in the cluster
  
  for (j in (clust+1)){
    term[j] <- (j-1-mu)^2*prob[j]
  }
  
  # Compute the variance of the cluster
  
  vc <- sum(term, na.rm = T)/clusterProb
  
  # Output
  
  return(vc)
}

