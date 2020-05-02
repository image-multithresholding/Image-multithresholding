############################################################################################

## Find thresholds of the gray levels according to the method proposed by Kurita et al. (1992)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdDPK returns a list with class "integer" 

############################################################################################

thresholdDPK <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 1,...,L
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)

  # Initialize 
  
  thr <- 0
  
  for (M in 2:k){
    
    # Initialize
    
    termStar <- vector()
    
    if (M==2){
      
      # Initialize
      
      termJStar <- vector()
      termStar <- vector()
      
      # Find all possible terms in J*
      
      for (t in 1:(L-k+M-1)){
        termJStar <- c(termJStar, criterionTerm(prob,t,0))
      }
      
      for (l in M:(L-k+M)){
        
        # Initialize
        
        term <- vector()
        
        # Find each term in JStar for each l
        
        for (t in 1:(l-1)){
          term <- c(term, termJStar[t] + criterionTerm(prob,l,t))
        }
        
        # Discard minus infinity cases
        
        for (i in 1:length(term)){
          if (term[i] == -Inf){
            term[i] <- 100
          }
        }
        
        # Find the minimun values
        
        termStar <- c(termStar, min(term))
      }
      
      # Find the minimum of all termStar
      
      minimum <- min(termStar) 
      
      # Find the value of l at which the minimum is reached
      
      lMin <- (2:(L-k+M))[which(termStar==minimum)]
      
      # Find the terms in JStar for lMin
      
      term <- vector()
      
      for (t in 1:(lMin-1)){
        term <- c(term, termJStar[t] + criterionTerm(prob,lMin,t))
      }
      
      # Discard minus infinity cases
      
      for (i in 1:length(term)){
        if (term[i] == -Inf){
          term[i] <- 100
        }
      }
      
      # Find the threshold which is the argmin
      
      thr[M-1] <- (1:(lMin-1))[which(term == min(term))]
      
    }
    
    else{
      
      # Initialize
      
      termJStar <- vector()
      termStar <- vector()
      
      # Find all possible terms in J*
      
      for (t in (thr[M-2]+1):(L-k+M-1)){
        termJStar <- c(termJStar, JStar(prob, M-1,t))
      }
      
      for (l in (thr[M-2]+2):(L-k+M)){
        
        # Initialize
        
        term <- vector()
        
        # Find each term in JStar for each l
        
        for (i in 1:(l-thr[M-2]-1)){
          term[i] <- termJStar[i] + criterionTerm(prob,l,((thr[M-2]+1):(L-k+M-1))[i])
        }
        
        # Discard minus infinity cases
        
        for (i in 1:length(term)){
          if (term[i] == -Inf){
            term[i] <- 100
          }
        }
        
        # Find the minimun values
        
        termStar <- c(termStar, min(term))
      }
      
      # Find the minimum of all termStar
      
      minimum <- min(termStar) 
      
      # Find the value of l at which the minimum is reached
      
      lMin <- ((thr[M-2]+2):(L-k+M))[which(termStar==minimum)]
      
      # Find the terms in JStar for lMin
      
      term <- vector()
      
      for (t in (thr[M-2]+1):(lMin-1)){
        term <- c(term, termJStar[t-thr[M-2]] + criterionTerm(prob,lMin,t))
      }
      
      # Discard minus infinity cases
      
      for (i in 1:length(term)){
        if (term[i] == -Inf){
          term[i] <- 100
        }
      }
      
      # Find the threshold which is the argmin
      
      thr[M-1] <- ((thr[M-2]+1):(lMin-1))[which(term == min(term))]
    }
  }
  
  # Output
  
  return(thr)
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  