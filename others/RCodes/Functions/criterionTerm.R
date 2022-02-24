############################################################################################

## Compute term of the criterion function proposed by Kurita et al. (1992)

############################################################################################

# Arguments:
# prob the probability vector of gray levels
# t2 the last element in the class
# t1 the value that defines start of the class (t1 < t2)

# Value:
# criterionTerm returns an object with class "numeric" 

############################################################################################

criterionTerm <- function(prob, t2, t1){
  
  # Find term
  
  g <- omegaClass(prob, t2, t1) + log(sigmaClass(prob, t2, t1)/omegaClass(prob, t2, t1))
  
  # Output
  
  return(g)
}

