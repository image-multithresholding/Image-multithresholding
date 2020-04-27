############################################################################################

## Error to minimize in approximation by sum of normal distributions 

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# prioriProb a list of priori probabilities per class
# means a list of mean per class
# vars a list of variance per class
# penalty in the objective function

# Value:
# objectiveError returns a list with class "numeric" 

############################################################################################

objectiveError <- function(prob, prioriProb, means, vars, penalty){
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Initialize
  
  normalComb <- vector()
  
  # Find each term in normal distribution combination
  
  for (j in 1:L){
    normalComb[j] <- normalSum(j, prioriProb, means, vars) 
  }
  
  # Find normal combination
  
  N <- sum((normalComb-prob)^2)/L + penalty*(sum(prioriProb)-1)^2
  
  # Output
  
  return(N)
}
