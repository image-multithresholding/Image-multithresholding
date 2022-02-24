############################################################################################

## Find thresholds of the gray levels using differential evolution optimization with 
## between class variance as fitness function

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# iter number of iterations, 1000 by default
# numberPop number of population, 40 by default
# mutationFactor value for the mutation factor, 0.5 by default
# crossoverConstant value for the crossover constant, 0.1 by default

# Value:
# thresholdDEO returns a list with class "numeric"

############################################################################################

thresholdDEO <- function(img, k, iter=1000, numberPop=40, mutationFactor=0.5, 
                         crossoverConstant=0.1){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  ## Fix variable bounds
  
  minThr <- 0
  maxThr <- L-1
  
  # Initialize
  
  solutions <- matrix(0, nrow=numberPop, ncol=k-1)
  
  # Generate an initial random solution
  
  for (i in 1:numberPop){
    for (j in 1:(k-1)){
      solutions[i,j] <- round(minThr + runif(1)*(maxThr-minThr))
    }
  }
  
  # Sort thresold candidates
  
  for (i in 1:numberPop){
    solutions[i,] <- sort(solutions[i,])
  }
  
  # Initialize
  
  counter <- 0
  
  # Repetition cycle
  
  repeat{
    
    # Initialize 
    
    # a partir de aca abstraer en una funcion que se pase como argumento
    # fitnessCriterion = gaussianError | betweenClassVar
    # gaussianError toma el de MENOR error
    # betweenClassVar toma el de MAYOR varianza entre clases
    fittingError <- vector()
    
    # Find the fitting errors
    
    for (i in 1:numberPop){
      if (length(unique(solutions[i,])) == k-1 & solutions[i,1] != 0 & solutions[i,k-1] != L-1){
        fittingError[i] <- betweenClassVar(prob, sort(solutions[i,]))
      }
      else{
        fittingError[i] <- 0
      }
    }
    
    # Find the best solution in the population
    
    bestSolution <- solutions[min(which(fittingError == max(fittingError))),]

    # hasta aca
    
    # Initialize
    
    pos1 <- vector()
    pos2 <- vector()
    
    # Find places to mutate
    

    # por cada miembro de la poblacion, encontrar dos indices distintos entre si y distintos al miembro actual
    # armar set con todos los elementos posibles, quitar el actual, y elegir dos al azar
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
    
    mutantVector <- matrix(0, nrow=numberPop, ncol=k-1)
    
    # Apply mutation
    
    for (i in 1:numberPop){
      mutantVector[i,] <- round(bestSolution + mutationFactor * (solutions[pos1[i],]-solutions[pos2[i],]))
    }
    
    # Sort thresold candidates
    
    for (i in 1:numberPop){
      mutantVector[i,] <- sort(mutantVector[i,])
    }
    
    # Fix in order to keep stablished ranks
    # reemplaza los que tengan elementos fuera de rango (< min o > max)
    for (i in 1:numberPop){
      if (length(which(mutantVector[i,] < minThr))>0 |
          length(which(mutantVector[i,] > maxThr))>0){
        mutantVector[i,] <- bestSolution
      }
    }
    
    # Initialize
    
    pos <- vector()
    
    # Find places to crossover
    
    # Por cada fila, elejimos otra fila (puede ser la misma)
    for (i in 1:numberPop){
      pos[i] <- sample(1:numberPop, 1)
    }
    
    # Initialize
    
    rand <- vector()
    
    # Find random crossover constants
    
    # Por cada fila, generamos un numero aleatorio entre 0 y 1
    for (i in 1:numberPop){
      rand[i] <- runif(1)
    }
    
    # Initialize
    
    crossedVector <- matrix(0, nrow=numberPop, ncol=k-1)
    
    # Apply exponentially crossover
    
    for (i in 1:numberPop){
      for (j in 1:(k-1)){
        # Ver si puedo sacar el ciclo de j y reemplazar las filas de una en vez de lugar a lugar
        # Por cada fila, comprobamos si el numero al azar es menor a la constante (o si la fila elegida al azar coincide con la actual)
        if (rand[i] < crossoverConstant | i == pos[i]){
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
      if (length(unique(crossedVector[i,])) == k-1 & crossedVector[i,1] != 0 & crossedVector[i,k-1] != L-1){
              newFittingError[i] <- betweenClassVar(prob, sort(crossedVector[i,]))
      }
      else{
        newFittingError[i] <- 0
      }
    }
    
    # Update the population
    
    for (i in 1:numberPop){
      if (newFittingError[i] > fittingError[i]){
        solutions[i,] <- crossedVector[i,]
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
    finalFittingError[i] <- betweenClassVar(prob, solutions[i,])
  }
  
  # Find best solution
  
  bestSolution <- solutions[min(which(finalFittingError == max(finalFittingError))),]
  
  # Find thresholds
  
  thr <- sort(bestSolution)
  
  # Output 
  
  return(thr)
}
