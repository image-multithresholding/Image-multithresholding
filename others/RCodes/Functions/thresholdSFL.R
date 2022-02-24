############################################################################################

## Find thresholds of the gray levels using shuffled frog-leaping algorithm with between 
## class variance as fitness function

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 5 by default
# numberMemeplex amount of memeplex, 5 by default
# numberFrog amount of frogs in each memeplex, 10 by default
# numberEvolution total of replication in memeplex evolution, 5 by default

# Value:
# thresholdSFL returns a list with class "numeric"

############################################################################################

thresholdSFL <- function(img, k, iter=5, numberMemeplex=4, numberFrog=10, 
                         numberEvolution=10){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find total population
  
  populationSize <- numberMemeplex * numberFrog
  
  # Initialize
  
  frogs <- matrix(0, nrow = populationSize, ncol = k-1)
  
  # Generate the initial frogs positions
  
  for (i in 1:populationSize){
    for (j in 1:(k-1)){
      frogs[i,j] <- round(runif(1)*(L-1))
    }
  }
  
  # Sort frogs
  
  for (i in 1:populationSize){
    frogs[i,] <- sort(frogs[i,])
  }
  
  # Initialize
  
  BCV <- vector()
  
  # Find between class variance per frog
  
  for (i in 1:populationSize){
    if (length(unique(frogs[i,])) == k-1 & frogs[i,1] != 0 & frogs[i,k-1] != L-1){
      BCV[i] <- betweenClassVar(prob, frogs[i,])
    } else {
      BCV[i] <- 0
    }
  }
  
  # Find best BCV and best frog
  
  bestBCV <- max(BCV)
  bestGlobalFrog <- frogs[min(which(BCV == bestBCV)),]
  
  # Initialize
  
  counter <- 0
  
  # Repetition cycle
  
  repeat{
    
    # Sort frogs in descending order
    
    sortedFrogs <- frogs[order(BCV, decreasing = TRUE),]
    
    # Initialize
    
    allFrogs <- vector()
    
    # Distribute frogs and analize per memeplex
    
    for (m in 1:numberMemeplex){
      
      # Initialize
      
      memeplexFrogs <- matrix(0, nrow=numberFrog, ncol=k-1)
      
      # Build memeplex
      
      for (n in 1:numberFrog){
        if (k==2){
          memeplexFrogs[n,] <- sortedFrogs[n+(m-1)*numberFrog] 
        } else{
          memeplexFrogs[n,] <- sortedFrogs[n+(m-1)*numberFrog,] 
        }
      }
      
      evolution <- 0
      
      repeat{
        
        BCVMemeplex <- vector()
        
        # Compute between class variance for the frogs in the memeplex
        
        for (i in 1:numberFrog){
          if (length(unique(memeplexFrogs[i,])) == k-1 & memeplexFrogs[i,1] != 0 & memeplexFrogs[i,k-1] != L-1){
            BCVMemeplex[i] <- betweenClassVar(prob, memeplexFrogs[i,])
          } else {
            BCVMemeplex[i] <- 0
          }
        }
        
        # Find best and worst frogs 
        
        bestFrog <- memeplexFrogs[min(which(BCVMemeplex == max(BCVMemeplex))),]
        worstFrog <- memeplexFrogs[max(which(BCVMemeplex == min(BCVMemeplex))),]
        
        # Find position of worst frog
        
        worstPosition <- max(which(BCVMemeplex == min(BCVMemeplex)))
        
        # Generate new worst frog
        
        newWorstFrog <- sort(round(worstFrog + runif(1) * (bestFrog - worstFrog)))
        
        # Updates
        
        if (length(unique(newWorstFrog)) == k-1 & newWorstFrog[1] > 0 & newWorstFrog[k-1] < L-1){
          
          if (betweenClassVar(prob, newWorstFrog) > betweenClassVar(prob, worstFrog)){
            memeplexFrogs[worstPosition,] <- newWorstFrog
          } else {
            newWorstFrog <- sort(round(worstFrog + runif(1) * (bestGlobalFrog - worstFrog)))
            
            if (betweenClassVar(prob, newWorstFrog) > betweenClassVar(prob, worstFrog)){
              memeplexFrogs[worstPosition,] <- newWorstFrog
            } else {
              memeplexFrogs[worstPosition,] <- sort(round(runif(k-1)*(L-1)))
            }
          }
        }
          
        evolution <- evolution+1
        
        if(evolution > numberEvolution){
          break
        }
      }
      
      allFrogs <- rbind(allFrogs, memeplexFrogs)
    }
      
    # Find between class variance per frog
    
    for (i in 1:populationSize){
      if (length(unique(allFrogs[i,])) == k-1 & allFrogs[i,1] != 0 & allFrogs[i,k-1] != L-1){
        BCV[i] <- betweenClassVar(prob, allFrogs[i,])
      } else {
        BCV[i] <- 0
      }
    }
    
    # Find best BCV and best frog
    
    bestBCV <- max(BCV)
    bestGlobalFrog <- allFrogs[min(which(BCV == bestBCV)),]
    
    counter <- counter+1
    
    if(counter > iter){
      break
    }
  }
  
  
  # Find optimum solution
  
  thr <- bestGlobalFrog
  
  # Output 
  
  return(thr)
}







  
  
  
  
  
  