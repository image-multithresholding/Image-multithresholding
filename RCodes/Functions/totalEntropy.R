############################################################################################

## Compute the total entropy according to a list of breaking gray levels

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# levels a list of gray levels which give the breaks

# Value:
# totalEntropy returns a list with class "numeric" 

############################################################################################

totalEntropy <- function(prob, levels){
  
  # Find the probabilities according to the given levels
  
  p <- probUpToLevel(prob, levels)
  
  # Find the number of breaks and probabilities
  
  n <- length(levels)
  m <- length(prob)
  
  # Shift levels to start in 1
  
  levels <- levels+1
  
  # Initialize
  
  E <- vector()
  
  if (n==1){
    
    # Find the entropy of both intervals
    
    E[1] <- -sum(prob[1:levels]*log(prob[1:levels]))/p[1]
    E[2] <- -sum(prob[(levels+1):m]*log(prob[(levels+1):m]))/p[2]
   } 
  
  else {
  
  # Find the entropy of each interval
  
  E[1] <- -sum(prob[1:levels[1]]*log(prob[1:levels[1]]))/p[1]
  for (i in 2:n){
    E[i] <- -sum(prob[(levels[i-1]+1):levels[i]]*log(prob[(levels[i-1]+1):levels[i]]))/p[i]
  }
  E[n+1] <- -sum(prob[(levels[n]+1):m]*log(prob[(levels[n]+1):m]))/p[n+1]
  }
  
  # Find the total entropy
  
  TE <- sum(E)
  
  # Output
  
  return(TE)
}


