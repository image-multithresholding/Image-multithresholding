############################################################################################

## Find thresholds of the gray levels using particle swarm optimization with between class
## variance as fitness function

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 100 by default
# numberPart number of particles, 40 by default

# Value:
# thresholdPSO returns a list with class "numeric"

############################################################################################

thresholdPSO <- function(img, k, iter=10, numberPart=40){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  ## Fix parameter bounds
  
  minThr <- 0
  maxThr <- L-1
  inertiaInitial <- 0.9
  inertiaFinal <- 0.4
  acceleration1Initial <- 2.5
  acceleration2Initial <- 0.5
  acceleration1Final <- 0.5
  acceleration2Final <- 2.5
  
  # Initialize
  
  particlePosition <- matrix(0, nrow=numberPart, ncol=k-1)
  
  # Generate the initial particle positions
  
  for (i in 1:numberPart){
    for (j in 1:(k-1)){
      particlePosition[i,j] <- round(minThr + runif(1)*(maxThr-minThr))
    }
  }
  
  # Sort thresold candidates
  
  for (i in 1:numberPart){
    particlePosition[i,] <- sort(particlePosition[i,])
  }
  
  # Initialize
  
  particleFitness <- vector()
  
  # Find the particle fitness
  
  for (i in 1:numberPart){
    if (length(unique(particlePosition[i,])) == k-1 & particlePosition[i,1] != 0 & particlePosition[i,k-1] != L-1){
      particleFitness[i] <- betweenClassVar(prob, particlePosition[i,])
    }
    else{
      particleFitness[i] <- 0
    }
  }
  
  # Initialize best positions and best fitness
  
  particleBestPosition <- particlePosition
  particleBestFitness <- particleFitness
  
  # Initialize global best position and global best fitness
  
  globalBestFitness <- max(particleFitness)
  globalBestPosition <- particlePosition[max(which(particleFitness == globalBestFitness)),]
  
  # Initialize
  
  counter <- 0
  particleVelocity <- matrix(0, nrow=numberPart, ncol=k-1)
  
  # Repetition cycle
  
  repeat{
    
    # Inertia variability
    
    inertia <- (inertiaInitial-inertiaFinal) * (iter-counter-1)/iter + inertiaFinal
    
    # Acceleration variability
    
    acceleration1 <- acceleration1Initial + (acceleration1Final - acceleration1Initial) * (counter+1)/iter
    acceleration2 <- acceleration2Initial + (acceleration2Final - acceleration2Initial) * (counter+1)/iter
    
    # Update velocity
    
    for (i in 1:numberPart){
      r1 <- runif(numberPart)
      r2 <- runif(numberPart)
      particleVelocity[i,] <- inertia * particleVelocity[i,] + 
        acceleration1 * r1[i] * (particleBestPosition[i,] - particlePosition[i,]) +
        acceleration2 * r2[i] * (globalBestPosition - particlePosition[i,])
    }
    
    # Find new positions
    
    newParticlePosition <- round(particlePosition + particleVelocity)
    
    # Fix in order to keep stablished ranks
    
    for (i in 1:numberPart){
      if (length(which(newParticlePosition[i,] < minThr))>0 |
          length(which(newParticlePosition[i,] > maxThr))>0){
        newParticlePosition[i,] <- particlePosition[i,]
      }
    }
    
    # Sort thresholds
    
    for (i in 1:numberPart){
      newParticlePosition[i,] <- sort(newParticlePosition[i,])
    }
    
    # Update positions
    
    particlePosition <- newParticlePosition
    
    for (i in 1:numberPart){
      if (length(unique(particlePosition[i,])) == k-1 & particlePosition[i,1] != 0 & particlePosition[i,k-1] != L-1){
        particleFitness[i] <- betweenClassVar(prob, sort(particlePosition[i,]))
      }
      else{
        particleFitness[i] <- 0
      }
    }
    
    # Update particle best positions
    
    for (i in 1:numberPart){
      if (particleFitness[i] > particleBestFitness[i]){
        particleBestPosition[i,] <- particlePosition[i,]
        particleBestFitness[i] <- particleFitness[i]
      }
      
      # Update better global fitness
      
      if (particleBestFitness[i] > globalBestFitness){
        globalBestFitness <- particleBestFitness[i] 
        globalBestPosition <- particleBestPosition[i,]
      }
    }
    
    counter <- counter+1
    
    if(counter > iter){
      break
    }
  }
  
  # Find optimum solution
  
  thr <- sort(globalBestPosition)
  
  # Output 
  
  return(thr)
}
