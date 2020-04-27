############################################################################################

## Find thresholds of the gray levels according to the method proposed by Lai (2006)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 500 by default
# numberPart number of particles, 40 by default
# acceleration1 acceleration constant, 2 by default
# acceleration2 acceleration constant, 2 by default
# inertia, 1 by default 
# penalty in the objective function, 0 by defalut

# Value:
# thresholdPSO returns a list with class "numeric"

############################################################################################

thresholdPSO <- function(img, k, iter=500, numberPart=40, acceleration1=2, acceleration2=2, 
                         inertia=1, penalty=0){
  
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
  
  particlePosition <- matrix(0, nrow=numberPart, ncol=3*k)
  
  # Generate the initial particle positions
  
  for (i in 1:numberPart){
    for (j in 1:k){
      particlePosition[i,j] <- minPrioriProb + runif(1)*(maxPrioriProb-minPrioriProb)
    }
    particlePosition[i,] <- particlePosition[i,]/sum(particlePosition[i,])
  }
  
  for (i in 1:numberPart){
    for(j in (k+1):(2*k)){
      particlePosition[i,j] <- minMeans + runif(1)*(maxMeans-minMeans)
    }
  }
  
  for (i in 1:numberPart){
    for(j in (2*k+1):(3*k)){
      particlePosition[i,j] <- minVars + runif(1)*(maxVars-minVars)
    }
  }
  
  # Initialize
  
  particleCost <- vector()
  
  # Find the particle costs
  
  for (i in 1:numberPart){
    particleCost[i] <- objectiveError(prob, particlePosition[i,1:k], particlePosition[i,(k+1):(2*k)], 
                          particlePosition[i,(2*k+1):(3*k)], penalty)
  }
  
  # Initialize best positions and best costs
  
  particleBestPosition <- particlePosition
  particleBestCost <- particleCost
  
  # Initialize global best position and global best cost
  
  globalBestCost <- min(particleCost)
  globalBestPosition <- particlePosition[min(which(particleCost == globalBestCost)),]
  
  # Initialize
  
  counter <- 0
  particleVelocity <- matrix(0, nrow=numberPart, ncol=3*k)
  
  # Repetition cycle
  
  repeat{
    
    # Update velocity
    
    for (i in 1:numberPart){
      r1 <- runif(numberPart)
      r2 <- runif(numberPart)
      particleVelocity[i,] <- inertia * particleVelocity[i,] + 
        acceleration1 * r1[i] * (particleBestPosition[i,] - particlePosition[i,]) +
        acceleration2 * r2[i] * (globalBestPosition - particlePosition[i,])
    }
    
    # Find new positions
    
    newParticlePosition <- particlePosition + particleVelocity
    
    # Fix in order to keep stablished ranks
    
    for (i in 1:numberPart){
      for (j in 1:k){
        if (newParticlePosition[i,j] < minPrioriProb){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
        if (newParticlePosition[i,j] > maxPrioriProb){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
      }
      
      for (j in (k+1):(2*k)){
        if (newParticlePosition[i,j] < minMeans){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
        if (newParticlePosition[i,j] > maxMeans){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
      }
      
      for (j in (2*k+1):(3*k)){
        if (newParticlePosition[i,j] < minVars){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
        if (newParticlePosition[i,j] > maxVars){
          newParticlePosition[i,j] <- particlePosition[i,j]
        }
      }  
    }
    
    # Update positions
    
    particlePosition <- newParticlePosition
    
    # Find the cost for the updated population
    
    for (i in 1:numberPart){
      particleCost [i] <- objectiveError(prob, particlePosition[i,1:k], particlePosition[i,(k+1):(2*k)], 
                            particlePosition[i,(2*k+1):(3*k)], penalty)
    }
    
    # Update particle best positions
    
    for (i in 1:numberPart){
      if (particleCost[i] < particleBestCost[i]){
        particleBestPosition[i,] <- particlePosition[i,]
        particleBestCost[i] <- particleCost[i]
      }
      
      # Update better global cost
      
      if (particleBestCost[i] < globalBestCost){
        globalBestCost <- particleBestCost[i] 
        globalBestPosition <- particlePosition[i,]
      }
    }
    
    # Modify inertia
    
    inertia <- inertia * 0.99
    
    counter <- counter+1
    
    if(counter > iter){
      break
    }
  }
  
  # Order the means
  
  orderMeans <- sort(globalBestPosition[(k+1):(2*k)])
  
  # Initialize
  
  orderPos <- vector()
  
  # Find positions according to mean order
  
  for (i in 1:length(orderMeans)){
    orderPos[i] <- which(globalBestPosition[(k+1):(2*k)] == orderMeans[i])
  }
  
  # Initialize
  
  orderBestSolution <- vector()
  
  # Order the classes according to order means
  
  for (i in 1:k){
    orderBestSolution[i] <- globalBestPosition[orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestSolution[k+i] <- globalBestPosition[k+orderPos[i]]
  }
  
  for (i in 1:k){
    orderBestSolution[2*k+i] <- globalBestPosition[2*k+orderPos[i]]
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
  