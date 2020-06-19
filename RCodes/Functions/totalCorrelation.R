############################################################################################

## Compute the total correlation according to a list of breaking gray levels

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# levels a list of gray levels which give the breaks

# Value:
# totalCorrelation returns a list with class "numeric" 

############################################################################################


totalCorrelation <- function(prob, levels){
  
  # Find the probabilities according to the given levels
  
  p <- probUpToLevel(prob, levels-1)
  
  # Find the number of breaks and probabilities
  
  n <- length(levels)
  m <- length(prob)
  
  # Initialize
  
  C <- vector()
  
  if (n==1){
    
    # Find the correlation of both intervals
    
    C[1] <- -log(sum(prob[1:levels]^2)/p[1]^2)
    C[2] <- -log(sum(prob[(levels+1):m]^2)/p[2]^2)
   } 
  
  else {
  
  # Find the correlation of each interval
  
  C[1] <- -log(sum(prob[1:levels[1]]^2)/p[1]^2)
  for (i in 2:n){
    C[i] <- -log(sum(prob[(levels[i-1]+1):levels[i]]^2)/p[i]^2)
  }
  C[n+1] <- log(sum(prob[(levels[n]+1):m]^2)/p[n+1]^2)
  }
  
  # Find the total correlation
  
  TC <- sum(C)
  
  # Output
  
  return(TC)
}






