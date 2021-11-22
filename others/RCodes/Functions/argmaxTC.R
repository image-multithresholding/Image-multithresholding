############################################################################################

## Compute the level at which the maximum total correlation is reached

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1

# Value:
# argmaxTC returns an object with class "numeric" 

############################################################################################

argmaxTC <- function(prob){
  
  # Find the amount of gray levels equal to the amount of probabilities
  
  L <- length(prob)
  
  # Initialize
  
  TC <- vector()
  
  # Find the total correlation varying the break level 
  # (notice that it makes no sense to consider the interval extrems since no partition holds)
  
  for(s in 1:(L-2)){
    TC[s] <- totalCorrelation(prob, s)
  }
  
  # Find the maximum total correlation
  
  maxTC <- max(TC) # Calculamos la máxima correlación total
  
  # Find the level at which the maximum total entropy is reached 

  argmax <- which(TC == maxTC) 
  
  # Output
  
  return(argmax)
}
