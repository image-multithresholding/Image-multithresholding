############################################################################################

## Axesion transformation

############################################################################################

# Arguments:
# x vector to which apply axesion transformation
# axesionFactor value for the axesion factor

# Value:
# axesion returns an objet with class "matrix" 

############################################################################################

axesion <- function(x, axesionFactor){
  
  # Find the amount of variables
  
  n <- length(x)
  
  # Find random diagonal matrix with elements obeying the Gaussian distributions and only one
  # random position having nonzero value
  
  mat <- matrix(0, nrow = n, ncol = n)
  position <- sample(1:n, 1)
  value <- rnorm(1)
  mat[position,position] <- value
  
  # Apply axesion transformation
  
  y <- x + axesionFactor * mat %*% matrix(x)
  
  # Output
  
  return(y)
}

  