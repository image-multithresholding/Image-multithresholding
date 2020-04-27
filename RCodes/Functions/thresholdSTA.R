############################################################################################

## Find thresholds of the gray levels according to the method proposed by Han et al. (2015)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 500 by default
# maxRotationFactor maximum value for the rotation factor, 1 by default
# minRotationFactor minimum value for the rotation factor, 0.0001 by default
# tranlationFactor value for the translation factor, 1 by default
# expansionFactor value for the expansion factor, 1 by default
# axesionFactor value for the axesion factor, 1 by default
# lesseningCoef value for the lessening coefficient, 2 by default
# searchEnforcement value for the search enforcement, 30 by default
# penalty in the objective function, 10 by defalut

# Value:
# thresholdSTA returns a list with class "numeric"

############################################################################################

thresholdSTA <- function(img, k, iter=500, maxRotationFactor=1, minRotationFactor=0.0001, 
                         translationFactor=1, expansionFactor=1, axesionFactor=1,
                         lesseningCoef=2, searchEnforcement=30, penalty=10){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find number of parameters
  
  numberParam <- 3*k
  
  # Generate an initial random solution
  
  prioriProb <- matrix(0, nrow=searchEnforcement, ncol=k)
  for (i in 1:searchEnforcement){
    prioriProb[i,] <- runif(k)
    prioriProb[i,] <- prioriProb[i,]/sum(prioriProb[i,])
  }
  means <- matrix(runif(searchEnforcement*k, 0, L-1), nrow = searchEnforcement)
  vars <- matrix(runif(searchEnforcement*k, 0, 100), nrow = searchEnforcement)
  
  # Initialize
  
  counter <- 0
  solutions <- cbind(prioriProb, means, vars)
  rotationFactor <- runif(1)
  
  # Repetition cycle
  
  repeat{
    
    if (rotationFactor < minRotationFactor){
      rotationFactor <- maxRotationFactor
    }
    
    # Apply expansion transformation 
    
    for (i in 1:searchEnforcement){
      
      # Initial state for considering update
      
      oldState <- solutions[i,]
      
      # Apply expansion translation
      
      state <- expansion(oldState, expansionFactor)
      
      # Find fitting errors
      
      oldStateError <- objectiveError(prob, oldState[1:k], oldState[(k+1):(2*k)], oldState[(2*k+1):(3*k)], penalty)
      stateError <- objectiveError(prob, state[1:k], state[(k+1):(2*k)], state[(2*k+1):(3*k)], penalty)
      
      # Compare errors
      
      if(is.na(stateError) == FALSE & stateError < oldStateError){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- translation(state, oldState, translationFactor)
        
        # Find new error
        
        newStateError <- objectiveError(prob, newState[1:k], newState[(k+1):(2*k)], newState[(2*k+1):(3*k)], penalty)
        
        # Compare errors
        
        if(is.na(newStateError) == FALSE & newStateError < stateError){
        
          # Update
          
          solutions[i,] <- newState
        }
      }
    }
    
    
    # Apply rotation transformation 
    
    for (i in 1:searchEnforcement){
      
      # Initial state for considering update
      
      oldState <- solutions[i,]
      
      # Apply rotation translation
      
      state <- rotation(oldState, rotationFactor)
      
      # Find fitting errors
      
      oldStateError <- objectiveError(prob, oldState[1:k], oldState[(k+1):(2*k)], oldState[(2*k+1):(3*k)], penalty)
      stateError <- objectiveError(prob, state[1:k], state[(k+1):(2*k)], state[(2*k+1):(3*k)], penalty)
      
      # Compare errors
      
      if(is.na(stateError) == FALSE & stateError < oldStateError){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- translation(state, oldState, translationFactor)
        
        # Find new error
        
        newStateError <- objectiveError(prob, newState[1:k], newState[(k+1):(2*k)], newState[(2*k+1):(3*k)], penalty)
        
        # Compare errors
        
        if(is.na(newStateError) == FALSE & newStateError < stateError){
          
          # Update
          
          solutions[i,] <- newState
        }
      }
    }
    
    # Apply axesion transformation 
    
    for (i in 1:searchEnforcement){
      
      # Initial state for considering update
      
      oldState <- solutions[i,]
      
      # Apply axesion translation
      
      state <- axesion(oldState, axesionFactor)
      
      # Find fitting errors
      
      oldStateError <- objectiveError(prob, oldState[1:k], oldState[(k+1):(2*k)], oldState[(2*k+1):(3*k)], penalty)
      stateError <- objectiveError(prob, state[1:k], state[(k+1):(2*k)], state[(2*k+1):(3*k)], penalty)
      
      # Compare errors
      
      if(is.na(stateError) == FALSE & stateError < oldStateError){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- translation(state, oldState, translationFactor)
        
        # Find new error
        
        newStateError <- objectiveError(prob, newState[1:k], newState[(k+1):(2*k)], newState[(2*k+1):(3*k)], penalty)
        
        # Compare errors
        
        if(is.na(newStateError) == FALSE & newStateError < stateError){
          
          # Update
          
          solutions[i,] <- newState
        }
      }
    }
    
    # Modify rotation factor
    
    rotationFactor <- rotationFactor/lesseningCoef
    
    counter <- counter+1
    
    if (counter > iter){
      break
    }
  }
  
  # Initialize
  
  fittingError <- vector()
  
  # Find overall fitting error
  
  for (i in 1:searchEnforcement){
    fittingError[i] <- objectiveError(prob, solutions[i,1:k], solutions[i,(k+1):(2*k)], solutions[i,(2*k+1):(3*k)], penalty)
  }
  
  # Find best error
  
  bestError <- min(fittingError)
  
  # Find best state
  
  bestState <- c(solutions[which(fittingError == bestError),])
  
  # Order the means
  
  orderMeans <- sort(bestState[(k+1):(2*k)])
  
  # Initialize
  
  orderPos <- vector()
  
  # Find positions according to mean order
  
  for (i in 1:length(orderMeans)){
    orderPos[i] <- which(bestState[(k+1):(2*k)] == orderMeans[i])
  }
  
  # Initialize
  
  orderBestState <- vector()
  
  # Order the classes according to order means
  
  for (i in 1:k){
    orderBestState[i] <- bestState[orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestState[k+i] <- bestState[k+orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestState[2*k+i] <- bestState[2*k+orderPos[i]]
  }
  
  # Find optimum coefficients
  
  optCoef <- optimumCoef(orderBestState[1:k], orderBestState[(k+1):(2*k)], 
                         orderBestState[(2*k+1):(3*k)])
  
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
