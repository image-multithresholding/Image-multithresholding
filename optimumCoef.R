############################################################################################

## Find coefficients for optimization cuadratic problem 

############################################################################################

# Arguments:
# prioriProb priori probabilities per class
# means a list of mean per class
# vars a list of variance per class

# Value:
# optimumCoef returns a list with class "matrix" 

############################################################################################

optimumCoef <- function(prioriProb, means, vars){
  
  # Find the amount of thresholds 
  
  n <- length(prioriProb)-1
  
  # Initialize
  
  A <- vector()
  B <- vector()
  C <- vector()
  
  # Find the coefficients
  
  for (i in 1:n){
      A[i] <- vars[i] - vars[i+1]
      B[i] <- 2*(means[i] * vars[i+1] - means[i+1] * vars[i])
      C[i] <- vars[i] * means[i+1]^2 - vars[i+1] * means[i]^2 + 2 * vars[i] * vars[i+1] * 
        log((sqrt(vars[i+1]) * prioriProb[i])/(sqrt(vars[i]) * prioriProb[i+1]))
  }
    
  # Joint data
  
  coef <- cbind(A,B,C)
  
  # Output
  
  return(coef)
}

