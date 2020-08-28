############################################################################################

## Compute the optimal number of classes according to the automatic thresholding criterion 
## proposed by Yen et al. (1995)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdATC returns a list with class "integer" 

############################################################################################

thresholdATC <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 0,1,...,L-1
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to the amount of probabilities
  
  L <- length(prob)
  
  # Initialize
  
  newClust <- 0:(L-1)
  thr <- vector()
  
  # Repeat the process for 2,..., k classes
  
  for (i in 1:(k-1)){
    
    # Find the new threshold using maximum total correlation criterion
    
    thr <- c(thr, newClust[argmaxTC(prob[newClust+1]/sum(prob[newClust+1]))])
    
    # Order the thresholds
    
    thr <- thr[order(thr)]
    
    # Find the classes according to the thresholds
    
    clust <- grayClustering(L, thr+1)
    
    # Find the variance per class
    
    varClust <- vector()
    for (j in 1:(i+1)){
      varClust[j] <- clusterVar(prob, clust[j,][clust[j,]!=-1], 0)
      # notice we must remove those fictitious elements; i.e. -1
    }
    
    # Find the argmax of cluster variances
    
    argmaxVar <- which(varClust == max(varClust))
    
    # Define the new lass to be partitioned
    
    newClust <- clust[argmaxVar,][clust[argmaxVar,]!=-1]
  }
  
  # Output
  
  return(thr)
}
