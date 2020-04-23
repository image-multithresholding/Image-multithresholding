############################################################################################

## Find the set of all minimum points in the histogram for each partition

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# n amount of regions

# Value:
# partitionMin returns an object with class "integer" 

############################################################################################

partitionMin <- function(prob, n){
  
  # Find the regions
  
  regions <- matrix(prob, nrow=n, byrow=T) 
  
  # Find the minimum of each region
  
  minima <- apply(regions, 1, min)
  
  # Find argmin
  
  argmin <- which(prob %in% minima)
  
  # Output
  
  return(argmin)
}
