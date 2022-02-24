############################################################################################

## Compute the minimization criterium equivalent to maximum likelihood

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# levels a list of gray levels which give the breaks

# Value:
# dualMaxLikelihood returns an object with class "numeric" 

############################################################################################

dualMaxLikelihood <- function(prob, levels){
  
  # Find the probabilities per class
  
  w <- probUpToLevel(prob, levels-1)
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find the classes 
  
  clust <- grayClustering(L, levels)
  
  # Adjust because levels start by 1
  
  for (i in 1:dim(clust)[1]){
    for (j in 1:dim(clust)[2]){
      if (clust[i,j]!=-1){
        clust[i,j] <- clust[i,j]+1
      }
    }
  }
  
  # Find the amount of classes
  
  n <- dim(clust)[1]
  
  # Initialize
  
  var <- vector()
  
  # Find the variance per class
  
  for (i in 1:n){
    newClust <- clust[i,][clust[i,]!=-1] # notice to discard fictitious values, i.e. -1
    var[i] <- clusterVar(prob, newClust, 1)
  }
  
  # Find the objective function
  
  objectiveFunction <- sum(w*log(var))
  
  # Output
  
  return(objectiveFunction)
}
