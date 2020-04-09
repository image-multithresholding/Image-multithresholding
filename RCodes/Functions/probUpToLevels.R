############################################################################################

## Compute a vector of probabilities up to a list of gray levels

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# levels a list of gray levels which give the breaks

# Value:
# probUpToLevel returns a list with class "numeric" 

############################################################################################

probUpToLevel <- function(prob, levels){
  
  # Find the amount of levels 
  
  n <- length(levels)
  
  # Shift levels to start in 1
  
  levels <- levels+1
  
  # Initialize
  
  p <- vector()
  
  if (n==1){
    # Find both probabilities
    p[1] <- sum(prob[1:levels])
    p[2] <- 1-p[1]
  }
  
  else{
  # Find probabilities up to each level
  
  p[1] <- sum(prob[1:levels[1]])
  for (i in 2:n){
    p[i] <- sum(prob[(levels[i-1]+1):levels[i]])
  }
  p[n+1] <- 1-sum(p)
  }
  
  # Output 
  
  return(p)
}

