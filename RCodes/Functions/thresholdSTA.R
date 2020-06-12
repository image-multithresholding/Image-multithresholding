############################################################################################

## Find thresholds of the gray levels according to the method proposed by Han et al. (2015)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 10 by default
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

thresholdSTA <- function(img, k, iter=10, maxRotationFactor=1, minRotationFactor=0.0001, 
                         translationFactor=1, expansionFactor=1, axesionFactor=1,
                         lesseningCoef=2, searchEnforcement=30){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  ## Fix parameter bounds
  
  minThr <- 0
  maxThr <- L-1
  
  # Generate an initial random solution
  
  # Initialize
  
  solutions <- matrix(0, nrow=searchEnforcement, ncol=k-1)
  
  # Generate an initial random solution
  
  for (i in 1:searchEnforcement){
    for (j in 1:(k-1)){
      solutions[i,j] <- round(minThr + runif(1)*(maxThr-minThr))
    }
  }
  
  # Order thresold candidates
  
  for (i in 1:searchEnforcement){
    solutions[i,] <- sort(solutions[i,])
  }
  
  # Initialize
  
  counter <- 0
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
      
      state <- round(expansion(oldState, expansionFactor))
      
      # Order thresold candidates
      
      state <- sort(state)
      
      # Fix in order to keep stablished ranks
      
      if (length(which(state < minThr))>0 | length(which(state > maxThr))>0){
          state <- oldState
      }
    
      # Find fitting function
      
      if (length(unique(oldState)) == k-1 & oldState[1] != 0 & oldState[k-1] != L-1){
        oldStateFit <- betweenClassVar(prob, oldState)
      } else {
        oldStateFit <- 0
      }
    
      if (length(unique(state)) == k-1 & state[1] != 0 & state[k-1] != L-1){
        stateFit <- betweenClassVar(prob, state)
      } else {
        stateFit <- 0
      }
    
      # Compare fitness
      
      if(stateFit > oldStateFit){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- round(translation(state, oldState, translationFactor))
        
        # Fix in order to keep stablished ranks
        
        if (length(which(newState < minThr))>0 | length(which(newState > maxThr))>0){
          newState <- state
        }
        
        # Order thresold candidates
        
        newState <- sort(newState)
        
        # Find new fitness
        
        if (length(unique(newState)) == k-1 & newState[1] != 0 & newState[k-1] != L-1){
          newStateFit <- betweenClassVar(prob, newState)
        } else {
          newStateFit <- 0
        }
        
        # Compare fitness
        
        if(newStateFit > stateFit){
        
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
      
      state <- round(rotation(oldState, rotationFactor))
      
      # Fix in order to keep stablished ranks
      
      if (length(which(state < minThr))>0 | length(which(state > maxThr))>0){
        state <- oldState
      }
      
      # Order thresold candidates
      
      state <- sort(state)
      
      # Find fitting function
      
      if (length(unique(oldState)) == k-1 & oldState[1] != 0 & oldState[k-1] != L-1){
        oldStateFit <- betweenClassVar(prob, oldState)
      }
      else{
        oldStateFit <- 0
      }
      
      if (length(unique(state)) == k-1 & state[1] != 0 & state[k-1] != L-1){
        stateFit <- betweenClassVar(prob, state)
      }
      else{
        stateFit <- 0
      }
      
      # Compare errors
      
      if(stateFit > oldStateFit){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- round(translation(state, oldState, translationFactor))
        
        # Fix in order to keep stablished ranks
        
        if (length(which(newState < minThr))>0 | length(which(newState > maxThr))>0){
          newState <- state
        }
        
        # Order thresold candidates
        
        newState <- sort(newState)
        
        # Find new fitness
        
        if (length(unique(newState)) == k-1 & newState[1] != 0 & newState[k-1] != L-1){
          newStateFit <- betweenClassVar(prob, newState)
        }
        else{
          newStateFit <- 0
        }
        
        # Compare fitness
        
        if(newStateFit > stateFit){
          
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
      
      state <- round(axesion(oldState, axesionFactor))
      
      # Order thresold candidates
      
      state <- sort(state)
      
      # Fix in order to keep stablished ranks
      
      if (length(which(state < minThr))>0 | length(which(state > maxThr))>0){
        state <- oldState
      }
      
      # Find fitting function
      
      if (length(unique(oldState)) == k-1 & oldState[1] != 0 & oldState[k-1] != L-1){
        oldStateFit <- betweenClassVar(prob, oldState)
      }
      else{
        oldStateFit <- 0
      }
      
      if (length(unique(state)) == k-1 & state[1] != 0 & state[k-1] != L-1){
        stateFit <- betweenClassVar(prob, state)
      }
      else{
        stateFit <- 0
      }
      
      # Compare fitness
      
      if(stateFit > oldStateFit){
        
        # Update
        
        solutions[i,] <- state
        
        # Apply translation transformation
        
        newState <- round(translation(state, oldState, translationFactor))
        
        # Fix in order to keep stablished ranks
        
        if (length(which(newState < minThr))>0 | length(which(newState > maxThr))>0){
          newState <- state
        }
        
        # Order thresold candidates
        
        newState <- sort(newState)
        # Find new fitness
        
        if (length(unique(newState)) == k-1 & newState[1] != 0 & newState[k-1] != L-1){
          newStateFit <- betweenClassVar(prob, newState)
        }
        else{
          newStateFit <- 0
        }
        
        # Compare fitness
        
        if(newStateFit > stateFit){
          
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
  
  fitness <- vector()
  
  # Find overall fitness
  
  for (i in 1:searchEnforcement){
    fitness[i] <- betweenClassVar(prob, solutions[i,])
  }
  
  # Find best fitness
  
  bestFit <- max(fitness)
  
  # Find best state
  
  bestState <- solutions[min(which(fitness == bestFit)),]
  
  # Find thresholds
  
  thr <- bestState
  
  # Output 
  
  return(thr)
}
