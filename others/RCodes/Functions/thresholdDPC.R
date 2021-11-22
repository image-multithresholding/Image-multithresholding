############################################################################################

## Find thresholds of the gray levels according to the deterministic parameters criterion 
## proposed by Chang et al. (2002)


############################################################################################

# Arguments:
# hist the frequency vector of gray levels 1,...,L after histogram smoothing
# # maxk maximun amount of permited classes

# Value:
# thresholdDPC returns a list with class "integer" 

############################################################################################

thresholdDPC <- function(img, maxk){
  
  # Find the best window size according to desired amount of classes
  
  p <- binValue(img, maxk)
  
  # Find the histogram of gray levels 1,...,L 
  
  originalHist <- imageHistogram(img)$freq
  
  # Smooth the histogram
  
  hist <- smoothedHistogram(originalHist, p)
  
  # Find the vector of probabilities
  
  prob <- imageProbabilities(img)$prob
  
  # Find the amount of gray levels equal to histogram length
  
  L <- length(hist)
  
  # Find the valleys
  
  val <- valleys(hist)
  
  # Find the clusters according to the obtained valleys
  
  clust <- valleyClustering(L, val)
  
  # Find the amount of clusters
  
  n <- dim(clust)[1]
  
  # Initialize
  
  optWin <- matrix(0, nrow=n, ncol=2)
  
  for (i in 1:n){
    
    # Find all the searching windows for the cluster
    
    w <- searchingWindow(clust[i,])
    
    # Find the otimal searching window for the cluster
    
    optWin[i,] <- optimalWindow(prob, w)
  }
  
  # Initialize
  
  estimations <- matrix(0, nrow=n, ncol=3)
  
  # Find the parameter estimations in eanc optimal window
  
  for (i in 1:n){
    estimations[i,1] <- clusterEstimations(prob, optWin[i,1]:optWin[i,2])$meanEstimation
    estimations[i,2] <- clusterEstimations(prob, optWin[i,1]:optWin[i,2])$varianceEstimation
    estimations[i,3] <- clusterEstimations(prob, optWin[i,1]:optWin[i,2])$proportionEstimation
  }
  
  # Classify the gray levels
  
  classes <- classificationRule(L, estimations)
  
  # Find the elements by class
  
  byClass <- split(classes, classes$Class)
  
  # Initialize
  
  rightBound <- vector()
  
  # Find the right bound per class
  
  for (i in 1:(n-1)){
    rightBound[i] <- max(byClass[[i]]$GrayLevel)
  }

  # Find the thresholds 
  
  thr <- sort(rightBound)
  
  # Output
  
  return(thr)
}




