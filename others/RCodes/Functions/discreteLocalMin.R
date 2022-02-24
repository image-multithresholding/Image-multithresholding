############################################################################################

## Find dicrete local argmin by dicrete gradiend descent

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L

# Value:
# discreteLocalMin returns an object with class "numeric" 

############################################################################################

discreteLocalMin <- function(prob){
  
  # Find the amount of probabilities
  
  n <- length(prob)
  
  # Initialize
  
  slop <- vector()
  
  # Find sign of line slope
  
  for (i in 1:(n-1)){
    slop[i] <- sign(prob[i+1]-prob[i])
  }
  
  # Initialize
  
  minima <- vector()
  
  # Find local minima
  
  for (i in 1:(n-1)){
    if (slop[i]==-1 & slop[i+1]==1){
      minima <- c(minima, i+1)
      }
  }
  
  # Output
  
  return(minima)
}

