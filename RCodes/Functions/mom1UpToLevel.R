############################################################################################

## Compute the first-order moment up to a level

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# level a gray levels which give the cut

# Value:
# mom1UpToLevel returns a list with class "numeric" 

############################################################################################

mom1UpToLevel <- function(prob, level){
  
  # Initialize

  term <- vector()
  
  # Find the first-order moment of each element
  
  for (i in 1:(level+1)){
    term[i] <- (i-1)*prob[i]
  }
   
  # Find the fist-order moment
  
  mom1 <- sum(term)
  
  # Output
  
  return(mom1)
}

