############################################################################################

## Random selection from a given set of pheromone intensities

############################################################################################

# Arguments:
# lb lower bound of the interval
# ub upper bound of the interval
# pheromoneIntensity the pheromone intensities of the elements in [lb,up]
# intensityControl parameter to control intensity magnitude


# Value:
# randomSelection returns an object with class "integer" 

############################################################################################


randomSelection <- function(lb, ub, pheromoneIntensity, intensityControl){
  
  # Find amount of candidates
  
  n <- length(pheromoneIntensity)
  
  # Initialize
  
  p <- vector()
  
  # Normalize probabilities
  
  for (i in 1:n){
    p[i] <- pheromoneIntensity[i]^intensityControl/sum(pheromoneIntensity^intensityControl)
  }
  
  # Initialize
  
  partialSum <- vector()
  
  # Compute partial sums
  
  for (i in 1:n){
    partialSum[i] <- sum(p[1:i])
  }
  
  # Generate a randon number
  
  r <- runif(1)
  
  # Make selection
  # Se queda con el indice del primer elemento de partialSum que supera o alcanza r
  # [3, 4, 5, 6]
  # lb + index
  sel <- (lb:ub)[min(which(partialSum > r | partialSum == r))]
  
  # Output
  
  return(sel)
}



