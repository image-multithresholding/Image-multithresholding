############################################################################################

## Find thresholds of the gray levels according to Otsu method (1979)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# k number of classes

# Value:
# thresholdOtsu returns a list with class "integer" 

############################################################################################

thresholdOtsu <- function(prob, k){
  
  # Find the amount of gray levels equal to number of prababilities
  
  L <- length(prob)
  
  # We differentiate cases for memory problems
  
  if (k==2 | k==3 | k==4){
    
    # Find all the candidates
    
    candidates <- thrCandidates(L,k)
    
    # Find the amount of candidates
    
    n <- dim(candidates)[1]
    
    # Initialize
    
    BCVar <- vector()
    
    # Find the between-class variance per candidates
    
    for(i in 1:n){
      BCVar[i] <- betweenClassVar(prob, candidates[i,])
    }
    
    # Find argmax of between-class variance
    
    argmaxBCVar <- which(BCVar==max(BCVar))
    
    # Find thresholds
    
    thr <- candidates[argmaxBCVar,] 
  }
  
  if (k > 4){
    
    # Find all possible levels to be a threshold
    
    all <- 1:(L-2)
    
    # Find groups of three thresholds
    
    preCandidates <- thrCandidates(L,4) 
    
    # Find the amount of gropus of three thresholds
    
    n <- dim(preCandidates)[[1]]
    
    # Initialize
    
    maxBCVar <- -100
    thr <- 0
    
    for (i in 1:n){
      
      # Find candidates that does not belong to each group of three thresholds
      
      others <- all[!all %in% preCandidates[i,]]
      
      # Find the required groups to complete the total amount of thresholds
      
      new <- t(combn(others, k-4))
      
      # Find amount of new groups
      
      m <- dim(new)[1]
      
      for (j in 1:m){
        
        # Joint previous and new candidates 
        
        candidates <- sort(c(preCandidates[i,],new[j,]))
        
        # Find between-class variance
        
        BCVar <- betweenClassVar(prob, candidates)
        
        # Store maximum between-class variance and better thresholds
        
        if (BCVar > maxBCVar){
          maxBCVar <- BCVar
          thr <- candidates
        }
      }
    }
  }
  
  # Output
  
  return(thr)
}




