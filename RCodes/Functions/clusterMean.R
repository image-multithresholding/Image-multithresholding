############################################################################################

## Compute the mean of a given cluster 

############################################################################################

# Arguments:
# prob the probability vector of gray levels ,1,...,L
# clust a clust of the gray levels

# Value:
# clustMean returns an object with class "numeric" 

############################################################################################

clusterMean <- function(prob, clust){
  
  # Initialize
  
  term <- vector()
  
  # Compute the expected value of each element in the cluster
  
  for (j in (clust+1)){
    term[j] <- (j-1)*prob[j]
    }
  
  # Compute the cluster probability
  
  clusterProb <- sum(prob[clust+1])
  
  # Compute the cluster mean
  
  cm <- sum(term, na.rm = T)/clusterProb
  
  # Output
  
  return(cm)
}


