############################################################################################

## Find thresholds of the gray levels using maximum log-likelihood equivalent to the criterion
## proposed by Kittler and Illingworth (1986)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdMLL returns a list with class "integer" 

############################################################################################

thresholdMLL <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 1,...,L
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # We differentiate cases for memory problems
  
  if (k==2 | k==3 | k==4){
    
    # Find all the candidates
    
    candidates <- thrCandidates(L,k)+1
    
    # Find the amount of candidates
    
    n <- dim(candidates)[1]
    
    # Initialize
    
    objectiveFunction <- vector()
    
    # Find the value of the objective function per candidates
    
    for(i in 1:n){
      objectiveFunction[i] <- dualMaxLikelihood(prob, candidates[i,])
    }
    
    # Discard cases in which variance is null
    
    for (i in 1:n){
      if (objectiveFunction[i] == -Inf){
        objectiveFunction[i] <- 100
      }
    }
    
    # Find argmin of the objective function
    
    argminObjectiveFunction <- which(objectiveFunction == min(objectiveFunction))
    
    # Find thresholds
    
    thr <- candidates[argminObjectiveFunction,] 
  }
  
  if (k > 4){
    
    # Find all possible levels to be a threshold
    
    all <- 1:(L-2)
    
    # Find groups of three thresholds
    
    preCandidates <- thrCandidates(L,4) 
    
    # Find the amount of gropus of three thresholds
    
    n <- dim(preCandidates)[[1]]
    
    # Initialize
    
    minObjectiveFunction <- 100
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
        
        # Find the value of the objective function per candidates
        
        objectiveFunction <- dualMaxLikelihood(prob, candidates)
        
        # Store minimum objective function value and better thresholds
        
        if (objectiveFunction < minObjectiveFunction){
          minObjectiveFunction <- objectiveFunction
          thr <- candidates
        }
      }
    }
  }
  
  # Output
  
  return(thr)
}




