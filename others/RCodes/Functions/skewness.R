## Compute the skewness of a given cluster of gray levels 

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# clust a cluster of the gray levels

# Value:
# skewness returns an object with class "numeric" 

############################################################################################

skewness <- function(prob, clust){
  
  # Find the second-order moment of the cluster
  
  mu2 <- clusterNthMoment(2, prob, clust)
  
  # Find the third-order moment of the cluster
  
  mu3 <- clusterNthMoment(3, prob, clust)
  
  # Find the skewness of the cluster
  
  skew <- mu3/sqrt(mu2^3)
  
  # Output
  
  return(skew)
}
