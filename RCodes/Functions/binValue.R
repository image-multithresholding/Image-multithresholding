############################################################################################

## Compute p which gives the 2p+1 optimal number of bins for histogram smoothing

############################################################################################

# Arguments:
# img an cimg object
# maxk maximun amount of permited classes

# Value:
# binValue returns an object with class "integer" 

############################################################################################

binValue <- function(img, maxk){
  
  # Find the histogram of gray levels 1,...,L 
  
  originalHist <- imageHistogram(img)$freq
  
  # Initialize 
  
  k <- vector()
  
  for (p in 1:100){
    
    # Smooth the histogram
  
    hist <- smoothedHistogram(originalHist, p)
    
    # Find the valleys
    
    val <- valleys(hist)
    
    # Find the amount of clusters
    
    k[p] <- length(val)+1
  }
  
  # Find how near are the amount of desired classes
  
  diff <- abs(k-maxk)
  
  # Find the first nearest number of classes
  
  p <- min(which(diff == min(diff)))
  
  # output
  
  return(p)
}

