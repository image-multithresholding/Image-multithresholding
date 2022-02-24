############################################################################################

## Compute the discrepancy of k classes, i.e. using k-1 thresholds

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# thr the k-1 thresholds definying the classes

# Value:
# discrepancy returns an object with class "numeric" 

############################################################################################

discrepancy <- function(prob, thr){
  
  # Find the amount of gray levels equal to the amount of probabilities
  
  L <- length(prob)
  
  # Find the probabilities of each class
  
  p <- probUpToLevel(prob, thr)
  
  # Find the classes
  
  clust <- grayClustering(L, thr)
  
  # Initialize
  
  var <- vector()
  
  # Find the variance per class (notice we must remove those fictitious elements; i.e. -1)
  
  for (i in 1:dim(clust)[1]){
    var[i] <- clusterVar(prob, clust[i,][clust[i,]!=-1], 0)
  }
  
  # Find the discrepancy
  
  dis <- sum(p*var)
  
  # Output
  
  return(dis)
}

