############################################################################################

## Classify gray levels accordind to the criterior porposed by Chang et al. (2002)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# classes matrix whose rows are clusters of gray levels considering value of -1 as no element

# Value:
# classificationRule returns an object with class "data.frame" containing the following components:
# Gray-level
#    gray level
# class
#    classification

############################################################################################

classificationRule <- function(prob, classes){
  
  # Find the amount of gray levels equal to the number of probabilities
  
  L <- length(prob)
  
  # Find the amount of classes
  
  n <- dim(classes)[1]
  
  # Initialize
  
  est <- matrix(0, ncol=n, nrow=3)
  
  # Find the estimations per class
  
  for (i in 1:n){
    est[i,1] <- clusterEstimations(prob, classes[i,][classes[i,]!=-1])$meanEstimation
    est[i,2] <- clusterEstimations(prob, classes[i,][classes[i,]!=-1])$varianceEstimation
    est[i,3] <- clusterEstimations(prob, classes[i,][classes[i,]!=-1])$proportionEstimation
  }
  
  # Initialize
  
  clust <- vector()
  
  # Assign class
  
  for (i in 1:L){
    
    value <- vector()
    
    for (j in 1:n){
      value[j] <- weightedGaussian(i, est[j,1], est[j,2], est[j,3])
    }
    
    clust[i] <- which(value == max(value))
  }
  
  # Joint data
  
  classification <- as.data.frame(cbind(1:L, clust))
  
  # Name columns
  
  colnames(classification) <- c("Gray-level", "Class")
  
  # Output
  
  return(classification)
}
