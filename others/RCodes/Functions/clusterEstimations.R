############################################################################################

## Find estimations of mean, variance and proportion of cluster of gray levels

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# clust cluster of gray levels

# Value:
# clusterEstimations returns an object with class "list" containing the following components:
# meanEstimation
#    estimation of the cluster mean 
# varianceEstimation
#    estimation of the cluster variance 
# proportionEstimation
#    estimation of the cluster proportion 

############################################################################################

clusterEstimations <- function(prob, clust){
  
  # Find the amount of gray levels equal to the number of probabilities
  
  L <- length(prob)
  
  # Find the length of the cluster
  
  len <- length(clust)
  
  # Find the mean estimation
  
  muHat <- clusterMean(prob, clust, 1)
  
  
  # Find the variance estimation
  
  varHat <- clusterVar(prob, clust, 1)
  
  
  # Find the proportion estimation
  
  pHat <- sum(prob[clust])
  
  # Joint data
  
  est <- list("meanEstimation" = muHat, "varianceEstimation" = varHat, "proportionEstimation" = pHat)
  
  # Output
  
  return(est)
}
