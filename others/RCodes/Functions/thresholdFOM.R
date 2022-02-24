############################################################################################

## Find thresholds of the gray levels according to fast method proposed by Liao et al. (1979)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdFOM returns a list with class "integer" 

############################################################################################

thresholdFOM <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find all u-v interval zeroth-order moment P(u,v) (notice that u,v = 1, ..., L)
  
  matrixP <- matrix(0, nrow=L, ncol=L)
  for (i in 1:L){
    for (j in i:L){
      matrixP[i,j] <- probUpToLevel(prob,j-1)[1]-probUpToLevel(prob,i-2)[1]
    }
  }
  
  # Find all u-v interval first-order moment S(u,v) (notice that u,v = 1, ..., L)

  matrixS <- matrix(0, nrow=L, ncol=L)
  for (i in 1:L){
    for (j in i:L){
      matrixS[i,j] <- mom1UpToLevel(prob,j-1)-mom1UpToLevel(prob,i-2)
    }
  }
  
  # Find the modified between-class variance per class
  
  matrixH <- matrix(0, nrow=L, ncol=L)
  for (i in 1:L){
    for (j in i:L){
      matrixH[i,j] <- matrixS[i,j]^2/matrixP[i,j]
    }
  }

  # We differentiate cases for memory problems
  
  if (k==2 | k==3 | k==4){
    
    # Find all the candidates
    
    candidates <- thrCandidates(L,k)
    
    # Find the amount of candidates
    
    n <- dim(candidates)[1]
    
    # Initialize
    
    modifiedBCVar <- vector()
    
    # Find the modified between-class variance per candidates
    
    for(i in 1:n){
      
      if (k==2){
        modifiedBCVar[i] <- sum(matrixH[1,candidates[i]+1]+matrixH[candidates[i]+2,L])
        }
      
      else{
        h <- vector()
        h[1] <- matrixH[1, candidates[i,1]+1]
        for (j in 2:(k-1)){
          h[j] <- matrixH[candidates[i,j-1]+2, candidates[i,j]+1]
        }
        h[k] <- matrixH[candidates[i,k-1]+2, L]
        modifiedBCVar[i] <- sum(h)
        }
      }
      
    # Find argmax of modified between-class variance
    
    argmaxmodifiedBCVar <- which(modifiedBCVar==max(modifiedBCVar))
    
    # Find thresholds
    
    thr <- candidates[argmaxmodifiedBCVar,] 
  }
  
  if (k > 4){
    
    # Find all possible levels to be a threshold
    
    all <- 1:(L-2)
    
    # Find groups of three thresholds
    
    preCandidates <- thrCandidates(L,4) 
    
    # Find the amount of gropus of three thresholds
    
    n <- dim(preCandidates)[[1]]
    
    # Initialize
    
    maxmodifiedBCVar <- -100
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
        
        # Find modified between-class variance
        
        h <- vector()
        h[1] <- matrixH[1, candidates[i,1]+1]
        for (j in 2:(k-1)){
          h[j] <- matrixH[candidates[i,j-1]+2, candidates[i,j]+1]
        }
        h[k] <- matrixH[candidates[i,k-1]+2, L]
        modifiedBCVar[i] <- sum(h)
      
        # Store maximum between-class variance and better thresholds
        
        if (modifiedBCVar > maxmodifiedBCVar){
          maxmodifiedBCVar <- modifiedBCVar
          thr <- candidates
        }
      }
    }
  }
  
  # Output
  
  return(thr)
}
