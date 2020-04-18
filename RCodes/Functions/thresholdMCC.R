############################################################################################

## Find thresholds of the gray levels according to the maximum correlation criterion 
## proposed by Yen et al. (1995)


############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdMCC returns a list with class "integer" 

############################################################################################

thresholdMCC <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of prababilities
  
  L <- length(prob)
  
  # We differentiate cases for memory problems
  
  if (k==2 | k==3 | k==4){
    
    # Find all the candidates
    
    candidates <- thrCandidates(L,k)
    
    # Find the amount of candidates
    
    n <- dim(candidates)[1]
    
    # Initialize
    
    TC <- vector()
    
    # Find the total correlation per candidates
    
    for(i in 1:n){
      TC[i] <- totalCorrelation(prob, candidates[i,])
    }
    
    # Find argmax of total correlation
    
    argmaxTC <- which(TC==max(TC))
    
    # Find thresholds
    
    thr <- candidates[argmaxTC,] 
  }
  
  if (k > 4){
    
    # Find all possible levels to be a threshold
    
    all <- 1:(L-2)
    
    # Find groups of three thresholds
    
    preCandidates <- thrCandidates(L,4) 
    
    # Find the amount of gropus of three thresholds
    
    n <- dim(preCandidates)[[1]]
    
    # Initialize
    
    maxTC <- -100
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
        
        # Find total correlation
        
        TC <- totalCorrelation(prob, candidates)
        
        # Store maximum total correlation and better thresholds
        
        if (TC > maxTC){
          maxTC <- TC
          thr <- candidates
        }
      }
    }
  }
  
  # Output
  
  return(thr)
}




