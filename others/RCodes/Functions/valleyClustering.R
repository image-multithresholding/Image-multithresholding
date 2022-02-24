############################################################################################

## Find limits of the clusters of a histogram of gray levels according to given valleys

############################################################################################

# Arguments:
# L number of gray levels 1,...,L
# val list od valleys

# Value:
# valleyClustering returns an objet with class "data.frame" containing the following components:
# start
#    first element of the cluster
# end
#    last element of the cluster

############################################################################################

valleyClustering <- function(L, val){
  
  # Find the amount pf clusters
  
  n = length(val) + 1 
  
  # Initialize
  
  clust <- matrix(0, nrow=n, ncol=2)
  
  # Find clusters
  
  clust[1,] <- c(1, val[1]-1)
  for (i in 2:(n-1)){
    clust[i,] <- c(val[i-1], val[i]-1)
  }
  clust[n,] <- c(val[n-1], L)
  
  # Name columns
  
  colnames(clust) <- c("start", "end")
  
  # Output
  
  return(clust)
}


