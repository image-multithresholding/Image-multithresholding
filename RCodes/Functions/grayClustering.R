############################################################################################

## Produce clusters of gray levels of a given image according to provided break positions 

############################################################################################

# Arguments:
# glevels the gray levels of the image
# brPos a list of positions used as cluster breaks 

# Value:
# grayClustering returns an object with class "data.frame" whose rows corresponds to the 
# elements of each cluster, considering the value -1 as no element

############################################################################################

grayClustering <- function(glevels, brPos){
  
  # Find the amout of  gray levels and cluster breaks
  
  L <- length(glevels)
  n <- length(brPos)
  
  # Find the clusters
  
  if (n==1){
    # Initialize in case of two clusters
    clust <- matrix(-1, nrow=2, ncol = max(brPos, L-brPos))
    # Define first cluster
    clust[1, 1:brPos] <- glevels[1:brPos]
    # Define second cluster
    clust[2, 1:(L-brPos)] <- glevels[(brPos+1):L]
  }
  # For more than two clusters
  else{
    # Initialize vector of cluster lengths
    leng <- vector()
    # Find the length of each cluster
    leng[1] <- brPos[1]
    for (i in 2:n){
      leng[i] <- brPos[i]-brPos[i-1]
    }
    leng[n+1] <- L-brPos[n]
    
    # Define data dimensions
    
    rows <- n+1
    cols <- max(leng)
    
    # Initialize
    clust <- matrix(-1, nrow = rows, ncol = cols)
    
    clust[1,1:leng[1]] <- glevels[1:brPos[1]]
    clust[n+1,1:leng[n+1]] <- glevels[(brPos[n]+1):L]
    for (i in 2:n){
      clust[i, 1:leng[i]] <- glevels[(brPos[i-1]+1):brPos[i]]
    }
  }
  
  # Output
  return(as.data.frame(clust))
}
