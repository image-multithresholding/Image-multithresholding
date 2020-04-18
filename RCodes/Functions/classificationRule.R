############################################################################################

## Classify gray levels accordind to the criterior porposed by Chang et al. (2002)

############################################################################################

# Arguments:
# L amount of gray levels 1,...,L
# est matrix containing the outputs of clusterEstimations function

# Value:
# classificationRule returns an object with class "data.frame" containing the following components:
# GrayLevel
#    gray level
# class
#    classification

############################################################################################

classificationRule <- function(L, est){
  
  # Find the amount of classes
  
  n <- dim(est)[1]
  
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
  
  colnames(classification) <- c("GrayLevel", "Class")
  
  # Output
  
  return(classification)
}
