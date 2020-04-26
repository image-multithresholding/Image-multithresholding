############################################################################################

## Expansion transformation

############################################################################################

# Arguments:
# x vector to which apply expansion transformation
# expansionFactor value for the expansion factor

# Value:
# expansion returns an objet with class "matrix" 

############################################################################################

expansion <- function(x, expansionFactor){
  
  # Find the amount of variables
  
  n <- length(x)
  
  # Find random diagonal matrix with elements obeying the Gaussian distribution
  
  mat <- diag(rnorm(n))
  
  # Apply expansion transformation
  
  y <- x + expansionFactor * mat %*% matrix(x)
  
  # Output
  
  return(y)
}

  