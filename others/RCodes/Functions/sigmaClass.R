############################################################################################

## Compute a statistic of a given class 

############################################################################################

# Arguments:
# prob the probability vector of gray levels
# t2 the last element in the class
# t1 the value that defines start of the class (t1 < t2)

# Value:
# sigmaClass returns an object with class "numeric" 

############################################################################################

sigmaClass <- function(prob, t2, t1){# k>m

  # Find the elements in the class
  
  clust <- (t1+1):t2
  
  # Find length of the class
  
  n <- length(clust)
  
  # Initialize
  
  muTerm <- vector()
  
  # Find each term in weighted mean
  
  for (i in 1:n){
    muTerm[i] <- clust[i]*prob[clust[i]]
  }
  
  # Find weighted mean
  
  mu <- sum(muTerm)/sum(prob[clust])
  
  # Initialize
  
  varTerm <- vector()
  
  # Find terms
  
  for (i in 1:n){
    varTerm[i] <- (clust[i]-mu)^2*prob[clust[i]]
  }
  
  # Find weighted variance
  
  var <- sum(varTerm)
  
  # Find the deviation
  
  sigma <- sqrt(var)
  
  # Output
  
  return(sigma)
}
