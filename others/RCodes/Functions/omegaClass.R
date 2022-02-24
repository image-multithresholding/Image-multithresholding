############################################################################################

## Compute a statistic of a given class 

############################################################################################

# Arguments:
# prob the probability vector of gray levels
# t2 the last element in the class
# t1 the value that defines start of the class (t1 < t2)

# Value:
# omegaClass returns an object with class "numeric" 

############################################################################################

omegaClass <- function(prob, t2, t1){
  
  # Find the statistic
  
  w <- sum(prob[(t1+1):t2])
  
  # Output
  
  return(w)
}
