
############################################################################################

## Compute the variance between classes

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# levels a list of gray levels which give the breaks

# Value:
# betweenClassVar returns an object with class "numeric" 

############################################################################################

betweenClassVar <- function(prob, levels){
  
  # Find the probabilities per class

  w <- probUpToLevel(prob, levels)
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find the classes (notice to sum 1 since the argument is position, not level)
  
  clust <- grayClustering(L, levels+1)
  
  # Find the amount of classes
  
  n <- dim(clust)[1]
  
  # Initialize
  
  mu <- vector()
  
  # Find the mean per class
  
  for (i in 1:n){
    newClust <- clust[i,][clust[i,]!=-1] # notice to discard fictitious values, i.e. -1
    mu[i] <- clusterMean(prob, newClust)
  }
  
  # Find the total mean of gray levels
  
  totalMean <- clusterMean(prob, 0:(L-1))
  
  # Initialize
  
  term <- vector()
  
  # Find each term for between-class variance
  
  for (i in 1:n){
    term[i] <- w[i] * (mu[i]-totalMean)^2
  }
  
  # Find between-class variance
  
  BCVar <- sum(term)
  
  # Output
  
  return(BCVar)
}


