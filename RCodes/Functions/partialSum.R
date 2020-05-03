############################################################################################

## Compute the partial sum of the criterion function proposed by Kurita et al. (1992) 

############################################################################################

# Arguments:
# prob the probability vector of gray levels
# maxLevel upper bound
# levels gray levels that define the classes

# Value:
# partialSum returns an object with class "numeric" 

############################################################################################

partialSum <- function(prob, maxLevel, levels){
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find the amount of levels
  
  n <- length(levels)
  
  if (n==1){
    term[1] <- criterionTerm(prob, levels, 0)
    term[2] <- criterionTerm(prob, maxLevel, levels)
  }
  
  else{
    # Initialize
    
    term <- vector()
    
    # Find each term
    
    term[1] <- criterionTerm(prob, levels[1],0)
    for (i in 2:n){
      term[i] <- criterionTerm(prob, levels[i], levels[i-1])
    }
    term[n+1] <- criterionTerm(prob, maxLevel, levels[n])
  }
  
  # Discard cases where the variance is zero, i.e. classes with only one element
  
  for (i in 1:n){
    if(term[i] == -Inf){
      term[i] <- 0
    }
  }
  
  # Find the sum
  
  s <- sum(term)
  
  # Output
  
  return(s)
}   
