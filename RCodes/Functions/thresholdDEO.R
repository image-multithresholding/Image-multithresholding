############################################################################################

## Find thresholds of the gray levels according to the method proposed by Cuevas et al. (2010)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 500 by default
# numberPop number of population, 90 by default
# mutationFactor value for the mutation factor, 0.25 by default
# crossoverConstant value for the crossover constant, 0.8 by default
# penalty in the objective function, 1.5 by defalut

# Value:
# thresholdDEO returns a list with class "numeric"

############################################################################################

thresholdDEO <- function(img, k, iter=500, numberPop=90, mutationFactor=0.25, 
                         crossoverConstant=0.8, penalty=1.5){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  ## Fix variable bounds
  
  minPrioriProb <- 0
  maxPrioriProb <- 1
  minMeans <- 0
  maxMeans <- L-1
  minVars <- 0
  maxVars <- 100
  
  # Initialize
  
  solutions <- matrix(0, nrow=numberPop, ncol=3*k)
  
  # Generate an initial random solution
  
  for (i in 1:numberPop){
    for (j in 1:k){
      solutions[i,j] <- minPrioriProb + runif(1)*(maxPrioriProb-minPrioriProb)
    }
    solutions[i,] <- solutions[i,]/sum(solutions[i,])
  }
  
  for (i in 1:numberPop){
    for(j in (k+1):(2*k)){
      solutions[i,j] <- minMeans + runif(1)*(maxMeans-minMeans)
    }
  }
  
  for (i in 1:numberPop){
    for(j in (2*k+1):(3*k)){
      solutions[i,j] <- minVars + runif(1)*(maxVars-minVars)
    }
  }
  
  # Initialize
  
  counter <- 0
  
  # Repetition cycle
  
  repeat{
    
    # Initialize 
    
    fittingError <- vector()
    
    # Find the fitting errors
    
    for (i in 1:numberPop){
      fittingError[i] <- objectiveError(prob, solutions[i,1:k], solutions[i,(k+1):(2*k)], 
                                        solutions[i,(2*k+1):(3*k)], penalty)
    }
    
    # Find the best solution in the population
    
    bestSolution <- solutions[min(which(fittingerror == min(fittingerror))),]
    
    ##############################################################
    
    # Initialize
    
    pos1 <- vector()
    pos2 <- vector()
    
    # Find places to mutate
    
    for (i in 1:numberPop){
      repeat{
        
        # Select first position
        
        pos1[i] <- sample(1:numberPop, 1)
        
        if (pos1[i]!=i){
          break
        }
      }
      
      repeat{
        
        # Select second position
        
        pos2[i] <- sample(1:numberPop, 1)
        
        if (pos2[i]!=i & pos2[i]!=pos1[i]){
          break
        }
      }
      
    }
    
    # Initialize
    
    mutantVector <- matrix(0, nrow=numberPop, ncol=3*k)
    
    # Apply mutation
    
    for (i in 1:numberPop){
      mutantVector[i,] <- bestSolution + mutationFactor * (solutions[pos1[i],]-solutions[pos2[i],])
    }
    
    # Initialize
    
    pos <- vector()
    
    # Find places to mutate
    
    for (i in 1:numberPop){
      pos[i] <- sample(1:numberPop, 1)
    }
    
    # Initialize
    
    rand <- vector()
    
    # Find random crossover constants
    
    for (i in 1:numberPop){
      rand[i] <- runif(1)
    }
    
    # Initialize
    
    crossedVector <- matrix(0, nrow=numberPop, ncol=3*k)
    
    # Apply exponentially crossover
    
    for (i in 1:numberPop){
      for (j in 1:(3*k)){
        if (rand[i] < crossoverConstant | j == pos[i]){
          crossedVector[i,j] <- mutantVector[i,j]
        } 
        else {
          crossedVector[i,j] <- solutions[i,j]
        }
      }
    }
    
    # Initialize
    
    newFittingError <- vector()
    
    # Find fitting errors for the new population
    
    for (i in 1:numberPop){
      newFittingError[i] <-objectiveError(prob, crossedVector[i,1:k], crossedVector[i,(k+1):(2*k)], 
                                          crossedVector[i,(2*k+1):(3*k)], penalty)
    }
    
    # Update the population
    
    for (i in 1:numberPop){
      if (is.na(newFittingError[i]) == FALSE & newFittingError[i] < fittingError[i]){
        solutions[i,] < crossedVector[i,]
      }
    }
    
    counter <- counter+1
    
    if (counter > iter){
      break
    }
  }
  
  # Initialize 
  
  finalFittingError <- vector()
  
  # Find the fitting errors for the final population
  
  for (i in 1:numberPop){
    finalFittingError[i] <- objectiveError(prob, solutions[i,1:k], solutions[i,(k+1):(2*k)], 
                                           solutions[i,(2*k+1):(3*k)], penalty)
  }
  
  # Find best solution
  
  bestSolution <- solutions[min(which(finalFittingError == min(finalFittingError))),]
  
  # Order the means
  
  orderMeans <- sort(bestSolution[(k+1):(2*k)])
  
  # Initialize
  
  orderPos <- vector()
  
  # Find positions according to mean order
  
  for (i in 1:length(orderMeans)){
    orderPos[i] <- which(bestSolution[(k+1):(2*k)] == orderMeans[i])
  }
  
  # Initialize
  
  orderBestSolution <- vector()
  
  # Order the classes according to order means
  
  for (i in 1:k){
    orderBestSolution[i] <- bestSolution[orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestSolution[k+i] <- bestSolution[k+orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestSolution[2*k+i] <- bestSolution[2*k+orderPos[i]]
  }
  
  # Find optimum coefficients
  
  optCoef <- optimumCoef(orderBestSolution[1:k], orderBestSolution[(k+1):(2*k)], 
                         orderBestSolution[(2*k+1):(3*k)])
  
  # Initialize
  
  thr <- matrix(0, nrow=k-1, ncol=2)
  
  # Find solutions of the optimization quadratic
  
  for (i in 1:(k-1)){
    thr[i,] <- quadraticSolve(optCoef[i,1], optCoef[i,2], optCoef[i,3])
  }
  
  # Convert to integer
  
  thr <- as.vector(round(thr,0))
  
  # Order only posible solutions
  
  thr <- sort(thr[thr>0 & thr<255])
  
  # Output 
  
  return(thr)
}
  