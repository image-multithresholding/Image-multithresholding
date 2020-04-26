############################################################################################

## Fitting by sum of normal distributions

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# prioriProb a list of priori probabilities per class
# means a list of mean per class
# vars a list of variance per class

# Value:
# normalSum returns an objet with class "numeric" 

############################################################################################

normalSum <- function(x, prioriProb, means, vars){
  
  # Initialize
  
  term <- vector()
  
  # Find each term
  
  for (i in 1:length(prioriProb)){
    term[i] = prioriProb[i]/sqrt(2*pi*vars[i]) * exp(-(x-means[i])^2/(2*vars[i]))
  }
  
  # Find the sum
  
  s <- sum(term)
  
  # Output
  
  return(s)
}

x=100
i=1

