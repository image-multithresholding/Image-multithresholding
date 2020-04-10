############################################################################################

## Compute the optimal number of classes according to the automatic thresholding criterion 
## proposed by Yen et al. (1995)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 0,1,...,L-1
# maxk maximun amount of permited classes

# Value:
# optimalClassesATC returns an list with class "list" containing the following components:
# classes
#    optimal classification number
# thresholds
#    class bounds


############################################################################################

optimalClassesATC <- function(prob, maxk){
  
  # Find the amount of gray levels equal to the amount of probabilities
  
  L <- length(prob)
  
  # Initialize
  
  newClust <- 0:(L-1)
  thr <- vector()
  cost <- vector()
  
  # Repeat the process for 2,..., maxk classes
  
  for (i in 1:(maxk-1)){
    
    # Find the new threshold using maximum total correlation criterion
    
    thr <- c(thr, newClust[argmaxTC(prob[newClust])+1])
    
    # Order the thresholds
    
    thr <- thr[order(thr)]
    
    # Find the classes according to the thresholds
    
    clust <- grayClustering(L, thr)
    
    # Find the cost function for i+1 classes
    
    cost[i] <- costATC(1, prob, thr)
    
    # Find the variance per class
    
    varClust <- vector()
    for (j in 1:(i+1)){
      varClust[j] <- clusterVar(prob, clust[j,][clust[j,]!=-1])
      # notice we must remove those fictitious elements; i.e. -1
    }
    
    # Find the argmax of cluster varaiances
    
    argmaxVar <- which(varClust == max(varClust))
    
    # Define the new lass to be partitioned
    
    newClust <- clust[argmaxVar,][clust[argmaxVar,]!=-1]
  }
  
  # Find the argmin of cost and sum 1 to obtain the optimal number of classes 
  
  kOptimal <- which(cost == min(cost))+1
  
  # Joint the thresholds for KOptimal classes
  
  solutionATC <- list("classes"=kOptimal, "thresholds"=thr)
  
  # Output
  
  return(solutionATC)
}
    

