############################################################################################

## Find the optimal window of a cluster as defined in Chang et al. (2002)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# w all searching windows of a cluster

# Value:
# optimalWindow returns a list with class "numeric" 

############################################################################################

# Definimos la función

optimalWindow <- function(prob, w){
  
  # Find the amount of searching windows
  
  n <- dim(w)[1]
  
  # Initialize
  
  skew <- vector()
  
  for (i in 1:n){
    skew[i] <- skewness(prob, w[i,])
  }
  
  # Find the module of skewnwss
  
  skew <- abs(skew)
  
  # Find the argmin of the module of skewness
  
  argminSkew <- which(skew == min(skew))
  
  # Find the optimal window
  
  optWin <- w[argminSkew, ]
  
  # Output
  
  return(optWin)
}

