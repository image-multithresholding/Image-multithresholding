############################################################################################

## Translation transformation

############################################################################################

# Arguments:
# x1 vector to which apply translation transformation
# x0 vector in the previous iteration 
# translationFactor value for the translation factor

# Value:
# translation returns an objet with class "matrix" 

############################################################################################

translation <- function(x1, x0, translationFactor){
  
  # Find the amount of variables
  
  n <- length(x1)
  
  # Find a random variable in [0,1]
  
  mat <- runif(1)
  
  # Apply translation transformation
  
  y <- x + translationFactor * mat * matrix(x1-x0)/norm(x1-x0, type="2")
  
  # Output
  
  return(y)
}

  