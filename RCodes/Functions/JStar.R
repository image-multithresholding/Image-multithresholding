############################################################################################

## Compute the minimum of partial sums of the criterion function proposed by Kurita et al. (1992) 

############################################################################################

# Arguments:
# prob the probability vector of gray levels
# maxLevel upper bound
# M amount of terms in the sum

# Value:
# JStar returns an object with class "numeric" 

############################################################################################

JStar <- function(prob, M, maxLevel){
  
  # Find all the possible levels
  
  levels <- t(combn(1:(maxLevel-1), M-1, FUN = NULL, simplify = TRUE))
  
  # Find the amount of levels
  
  n <- dim(levels)[1]
  
  # Initialize
  
  term <- vector()
  
  # Find each partial sum
  
  for (i in 1:n){
    term[i] <- partialSum(prob, maxLevel, levels[i,])
  }
  
  # Discard cases where the variance is zero, i.e. classes with only one element
  
  for (i in 1:n){
    if(term[i] == -Inf){
      term[i] <- 100
    }
  }
  
  # Find the minimum
  
  minimum <- min(term)
  
  # Output
  
  return(minimum)
}
