############################################################################################

## Find thresholds of the gray levels according to the maximum entropy criterion 
## proposed by Kapur et al. (1985)


############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# k number of classes

# Value:
# thresholdMEC returns a list with class "integer" 

############################################################################################

thresholdMEC <- function(prob, k){
  
  # Find the amount of gray levels equal to number of prababilities
  
  L <- length(prob)
  
  # We differentiat cases for memory problems
  
  if (k==2 | k==3 | k==4){
    
    # Find all the candidates
    
    candidates <- thrCandidates(L,k)
    
    # Find the amount of candidates
    
    n <- dim(candidates)[1]
    
    # Initialize
    
    TE <- vector()
    
    # Find the total entropy per candidates
    
    for(i in 1:n){
      TE[i] <- totalEntropy(prob, candidates[i,])
    }
    
    # Find argmax of total entropy
    
    argmaxTE <- which(TE==max(TE))
    
    # Find thresholds
    
    thr <- candidates[argmaxTE,] 
  }
  
  if (k > 4){
    
    # Find all possible levels to be a threshold
    
    all <- 1:(L-2)
    
    # Find groups of three thresholds
    
    preCandidates <- thrCandidates(L,4) 
    
    # Find the amount of gropus of three thresholds
    
    n <- dim(preCandidates)[[1]]
    
    # Initialize
    
    maxTE <- -100
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
        
        # Find total entropy
        
        TE <- totalEntropy(prob, candidates)
        
        # Store maximum total entropy and better thresholds
        
        if (TE > maxTE){
          maxTE <- TE
          thr <- candidates
        }
      }
    }
  }
  
  # Output
  
  return(thr)
}




