############################################################################################

## Select one chromosome by rolletwheel using fitness function proposed by Banimelhem and 
## Yahya (2011)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# pop population from which the selection is done
# K number of thresholds

# Value:
# RWS returns a list with class "numeric" 

############################################################################################

RWS <- function(prob, pop, K){
  
  # Find amount of populations
  
  n <- dim(pop)[1]
  
  # Initialize
  
  thr <- matrix(0, nrow=n, ncol=K)
  
  # Convert ninary to decimal
  
  for (i in 1:n){
    thr[i,] <- binaryToDecimal(pop[i,], K)
  }
  
  # Initialize
  
  fitPop <- vector()
  
  # Find the fitness function for the population
  
  for (i in 1:n){
    fitPop[i] <- fitnessFunction(prob, thr[i,])
  }
  
  # Add all the fitness values
  
  S <- sum(fitPop)  
  
  # Generate a random number in (0,S)
  
  r <- runif(1,0,S)
  
  # Initialize
  
  previousHypothesis <- vector()
  
  # Go over the population and find the fitness of previous hypothesis
  
  for (i in 1:n){
    previousHypothesis[i] <- sum(fitPop[1:i])
  }
  
  # Select the best hypotesis
  
  selection <- pop[min(which(previousHypothesis > r)),]
  
  # Output
  
  return(selection)
}
