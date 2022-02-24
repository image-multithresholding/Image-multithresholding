## Compute the nth moment of a given cluster of gray levels 

############################################################################################

# Arguments:
# n order moment
# prob the probability vector of gray levels 1,...,L
# clust a cluster of the gray levels

# Value:
# clusterNthMoment returns an object with class "numeric" 

############################################################################################

clusterNthMoment <- function(n, prob, clust){
  
  # Find the cluster mean
  
  mu <- clusterMean(prob, clust, 1)
  
  # Find the cluster length
  
  len <- length(clust)
  
  # Initialize
  
  numerator <- vector()
  denominator <- vector()
  
  # Find nth moment factors 
  
  for (i in 1:len){
    numerator[i] <- (clust[i]-mu)^n*prob[clust[i]]
    denominator[i] <- prob[clust[i]]
  }
  
  # Find nth moment
  
  mom <- sum(numerator)/sum(denominator)
  
  # Output
  
  return(mom)
}
