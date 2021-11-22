############################################################################################

## Find thresholds of the gray levels using ant colony optimization with between class
## variance as fitness function

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 100 by default
# pheromoneIntensity parameter to control the magnitude of pheromone intensity, 1 by default
# breakSTR breaking in state transition rule, 0.68 by default
# pheromoneTrail initial pheromone trail, 0.01 by default
# pheromoneEvaporation parameter to control pheromone evaporation, 0.94 by default
# pheromonePersistence parameter to control pheromone persistence, 0.9 by default
# pheromoneContribution parameter to controls the magnitude of the pheromone contribution, 
#    0.01*pheromoneTrail by default
# populationSize number of ants in population, 40 by default

# Value:
# thresholdACO returns a list with class "numeric"

############################################################################################

thresholdACO <- function(img, k, iter=100, pheromoneIntensity = 1, breakSTR = 0.68,
                         pheromoneTrail = 0.01, pheromoneEvaporation = 0.94,
                         pheromonePersistence = 0.9, 
                         pheromoneContribution = 0.01*pheromoneTrail, populationSize = 40){
  
  # pheromoneContribution es siempre un porcentaje de lo que sea pheromoneTrail
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Initialize hierarchical search range
# k = 3
# HSR = [[0, 1, 2], [0, 1, 2], ... k veces]

  HSR <- matrix(rep(c(0,L-1), k-1), nrow = k-1, byrow = TRUE)
  
  # Initialize pheromones intensities
  
  tau <- matrix(pheromoneTrail, nrow = k-1, ncol = L)
  
  # Initialize
  
  ants <- matrix(0, nrow = populationSize, ncol = k-1)
  
  # Generate the initial ants positions
  
  for (i in 1:populationSize){
    for (j in 1:(k-1)){
      ants[i,j] <- round(runif(1)*(L-1))
    }
  }
  
  # Order ants
  
  for (i in 1:populationSize){
    ants[i,] <- sort(ants[i,])
  }
  
  # Initialize
  
  BCV <- vector()
  
  # Find between class variance per ant
  
  for (i in 1:populationSize){
    if (length(unique(ants[i,])) == k-1 & ants[i,1] != 0 & ants[i,k-1] != L-1){
      BCV[i] <- betweenClassVar(prob, ants[i,])
    } else {
      BCV[i] <- 0
    }
  }
  
  # Find best BCV and best ant
  
  bestBCV <- max(BCV)
  bestAnt <- ants[min(which(BCV == bestBCV)),]
  
  # Initialize
  
  counter <- 0
  
  # Repetition cycle
  
  repeat{
    
    # Build solutions based on the state transition rule
    
    for (h in 1:populationSize){
      
      for (i in 1:(k-1)){
        
        # Generate a random number
        
        q <- runif(1)
        
        # Apply transition rule
        
        if (q < breakSTR){
          
          # Find maximum pheromone intensities
          
          maxTau <- max((tau[i,HSR[i,1]:HSR[i,2]])^pheromoneIntensity)
          
          # Find argmax
          
          argMaxTau <- min(which((tau[i,HSR[i,1]:HSR[i,2]])^pheromoneIntensity == maxTau))
          
          # Change state
          
          ants[h,i] <- argMaxTau+HSR[i,1]-1
        } else {
          ants[h,i] <- randomSelection(HSR[i,1], HSR[i,2], tau[i,HSR[i,1]:HSR[i,2]], pheromoneIntensity)
          
          # Apply the online pheromone update rule
          
          tau[i,] <- (1-pheromoneEvaporation)*tau[i,]
        }
        
        # Update region
        
        if (i != k-1){
          HSR[i+1,] <- c(ants[h,i]+1, L-1)
        }
      }
    }
    
    # Order ants
    
    for (i in 1:populationSize){
      ants[i,] <- sort(ants[i,])
    }
    
    # Compute new between class variance
    
    for (i in 1:populationSize){
      if (length(unique(ants[i,])) == k-1 & ants[i,1] != 0 & ants[i,k-1] != L-1 & ants[i,k-1] != L){
        BCV[i] <- betweenClassVar(prob, ants[i,])
      } else {
        BCV[i] <- 0
      }
    }
    
    # Update best BCV and best ant
    
    if (max(BCV) > bestBCV){
      bestBCV <- max(BCV)
      bestAnt <- ants[min(which(BCV == bestBCV)),]
    }
    
    # Apply the offline pheromone update rule
    
    tau <- pheromonePersistence * tau + (1-pheromonePersistence) * pheromoneContribution * bestBCV^2
    
    counter <- counter+1
    
    if(counter > iter){
      break
    }
  }
  
  # Find optimum solution
  
  thr <- bestAnt
  
  # Output 
  
  return(thr)
}







