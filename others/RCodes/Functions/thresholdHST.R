############################################################################################

## Find thresholds of the gray levels according to the method proposed by Gurung and Tamang (2019)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdHST returns an object with class "data.frame" containing the following components:
# Regions
#    amount of considered regions
# Thesholds
#    list of thresholds in each case

# Required
library(numbers)

############################################################################################

thresholdHST <- function(img, k){
  
  # Find the vector of probabilities of the gray leves 1,...,L
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to number of probabilities
  
  L <- length(prob)
  
  # Find valley positions
  
  valPosition <- discreteLocalMin(prob)
  
  # Find possible amount of histogram partitions of equal size
  
  regions <- divisors(L)
  
  # Discard extreme values
  
  regions <- regions[regions !=1 & regions!=L]
  
  # Find amount of possible partitions
  
  n <- length(regions) 
  
  # Initialize
  
  minRegion <- list()
  
  # Find the set of all minimum points in the histogram for each partition
  
  for (i in 1:n){
    minRegion[[i]] <- partitionMin(prob, regions[i])
  }
  
  # Find the possible regions to which intersec according to specified number of classes
  
  intRegions <- Filter(function(x) length(x) > (k-1) | length(x) == (k-1), minRegion)
  
  # Find the amount of intersections
  
  m <- length(intRegions)
  
  # Initialize
  
  intersection <- list()
  
  # Find the intersections
  
  for (i in 1:m){
    intersection[[i]] <- intersect(valPosition, intRegions[[i]])
  }
  
  # Initialize 
  
  thr <- matrix(0, nrow=m, ncol=(k-1))
  numberRegion <- vector()
  
  for (i in 1:m){
    
    # Initialize
    
    clust <- intersection[[i]]
    
    # Complete places
    
    if (length(intersection[[i]]) %% (k-1) != 0){
      clust <- c(intersection[[i]], rep(0, k-1-length(intersection[[i]])%%(k-1)))
    }
    
    # Arrange data
    
    clust <- matrix(clust, nrow=k-1, byrow=T)
    
    # Find thresholds
    
    for (j in 1:(k-1)){
      thr[i,j] <- round(mean(clust[j,][clust[j,] != 0]),0)
    } 
    
    # Find amount of regions 
    
    numberRegion[i] <- regions[i+n-m]
  }
  
  # Joint data
  
  output <- na.omit(as.data.frame(cbind(numberRegion, thr)))
  
  # Name the columns
  
  colnames(output) <- c("Regions", "Thresholds", rep("", k-2))
    
  # Output 
  
  return(output)
}


