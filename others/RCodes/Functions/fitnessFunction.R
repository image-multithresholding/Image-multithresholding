############################################################################################

## Fitness function to measure the goodness of the segmentation proposed by animelhem and 
## Yahya (2011)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# thr a list of thresholds

# Value:
# fitnessFunction returns a list with class "numeric" 

############################################################################################

fitnessFunction <- function(prob, thr){
  
  # Find the between class variance
  
  BCV <- betweenClassVar(prob, thr)
  
  # Find the within class variance
  
  WCV <- withinClassVar(prob, thr)
  
  # Find the fitness value
  
  fit <- BCV/WCV
  
  # Output
  
  return(fit)
}
