############################################################################################

## Rotation transformation

############################################################################################

# Arguments:
# x vector to which apply rotation transformation
# rotationFactor value for the rotation factor

# Value:
# rotation returns an objet with class "matrix" 

############################################################################################

rotation <- function(x, rotationFactor){
  
  # Find the amount of variables
  
  n <- length(x)
  
  # Find random matrix with elements in [-1,1]
  
  mat <- matrix(runif(n*n,-1,1), nrow = n)
  
  # Apply expansion transformation
  
  y <- x + rotationFactor/(n*norm(x, type="2")) * mat %*% matrix(x)
  
  # Output
  
  return(y)
}

  