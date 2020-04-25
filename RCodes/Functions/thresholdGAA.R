############################################################################################

## Find thresholds of the gray levels according to the method proposed by Banimelhem and 
## Yahya (2011)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 100 by default
# populationSize number of initial populatio, 10 by default
# crossoverRate fraction to replace by crossover, 0.95 by default
# mutationRate fraction to replace by mutation, 0.05 by default

# Value:
# thresholdGAA returns a list with class "numeric"

############################################################################################

thresholdGAA <- function(img, k, iter=100, populationSize=10, crossoverRate=0.95, mutationRate=0.05){
  
  # Find the vector of frequencies of the gray leves 0,1,...,L-1
  
  hist <- imageHistogram(img)$freq
  
  # Find the amount of gray levels equal to number of frequencies
  
  L <- length(hist)
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find number of bits in each vector
  
  bits <- log(L)/log(2)
  
  # Initialize
  
  initialPop <- matrix(0, nrow=populationSize, ncol=bits*(k-1))
  
  # Generate initial population
  
  for (i in 1:populationSize){
    initialPop[i,] <- bitVector(bits*(k-1))
  }
  
  # Initialize
  
  bestHypothesis <- matrix(0, nrow=iter, ncol=k-1)
  
  # Replicate the process
  
  for (j in 1:iter){
    
    # Initialize
    
    newPop <- vector() 
    newSize <- 0
    
    while(newSize < populationSize){
      
      # Choose two chromosomes by roullete wheel selection conserving the number of desired thresholds
      
      repeat{
        
        # Selection
        
        chromo1 <- RWS(prob, initialPop, k-1)
        
        # Convert to decimal
        
        dec <- binaryToDecimal(chromo1, k-1)
        
        if(length(dec[!duplicated(dec)]) == k-1){
          break
        }
      }
      
      repeat{
        
        # Selection
        
        chromo2 <- RWS(prob, initialPop, k-1)
        
        # Convert to decimal
        
        dec <- binaryToDecimal(chromo2, k-1)
        
        if(length(dec[!duplicated(dec)]) == k-1){
          break
        }
      }
      
      # Add both to new population
      
      newPop <- rbind(newPop, chromo1, chromo2)
      
      # Generate a random number between 0 and 1
      
      rand <- runif(1)
      
      # Apply croosover
      
      if (rand < crossoverRate) {
        position <- floor(runif(1,1,bits*(k-1)))
        newChromo1 <- crossover(chromo1, chromo2, position)[1,]
        newChromo2 <- crossover(chromo1, chromo2, position)[2,]
      }
      
      # Apply mutation
      
      if (rand < mutationRate) {
        position <- floor(runif(1,1,bits*(k-1)))
        newChromo1 <- mutation(newChromo1, position)
        newChromo2 <- mutation(newChromo2, position)
      }
      
      # Find fitness
      
      fitChromo1 <- fitnessFunction(prob, binaryToDecimal(chromo1, k-1))
      fitChromo2 <- fitnessFunction(prob, binaryToDecimal(chromo2, k-1))
      
      # Add new population
      
      newChromo1Dec <- binaryToDecimal(newChromo1, k-1)
      if(length(newChromo1Dec[!duplicated(newChromo1Dec)]) == k-1){
        fitNewChromo1 <- fitnessFunction(prob, binaryToDecimal(newChromo1, k-1))
        
        if (fitNewChromo1 > max(fitChromo1, fitChromo2)) {
          newPop <- rbind(newPop, newChromo1)
        }
      }
      
      newChromo2Dec <- binaryToDecimal(newChromo2, k-1)
      if(length(newChromo2Dec[!duplicated(newChromo2Dec)]) == k-1){
        fitNewChromo2 <- fitnessFunction(prob, binaryToDecimal(newChromo2, k-1))
        
        if (fitNewChromo2 > max(fitChromo1, fitChromo2)) {
          newPop <- rbind(newPop, newChromo2)
        }
      }
      
      # Find new population size
      
      newSize <- dim(newPop)[1]
    }
    
    # Initialize
    
    fitNewPop <- vector()
    
    # Evaluate new population and select best goodness hypothesis
    
    for (i in 1:dim(newPop)[1]){
      fitNewPop[i] <- fitnessFunction(prob, binaryToDecimal(newPop[i,], k-1))
    }
    
    # Find best hypothesis
    
    bestHypothesis[j,] <- binaryToDecimal(newPop[min(which(fitNewPop==max(fitNewPop))),], k-1)
  } 
  
  # Initialize
  
  fitBest <- vector()
  
  # Find the hypothesis of greater goodness among all iterations
  
  for (i in 1:iter){
    fitBest[i] <- fitnessFunction(prob, bestHypothesis[i,])
  }
  
  # Find thresholds
  
  thr <- binaryToDecimal(bestHypothesis[min(which(fitBest==max(fitBest))),], k-1)  
  
  # Output 
  
  return(thr)
}


