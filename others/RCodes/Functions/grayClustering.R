############################################################################################

## Produce clusters of gray levels of a given image according to provided break positions 

############################################################################################

# Arguments:
# L the amount of gray levels in the image
# brPos a list of positions used as cluster breaks 

# Value:
# grayClustering returns an object with class "data.frame" whose rows corresponds to the 
# elements of each cluster, considering the value -1 as no element

############################################################################################

grayClustering <- function(L, brPos){
  
  # Find the amout of cluster breaks
  
  n <- length(brPos)
  
  # Find the clusters
  
  if (n==1){
    if(brPos == L){
      clust <- t(0:(L-1))
    }
    else{
      # Initialize in case of two clusters
      clust <- matrix(-1, nrow=2, ncol = max(brPos, L-brPos))
      # Define first cluster
      clust[1, 1:brPos] <- (1:brPos)-1
      # Define second cluster
      clust[2, 1:(L-brPos)] <- ((brPos+1):L)-1
    }
  }
  
  # For more than two clusters
  else{
    
    # Discard equal breaks
    
    brPos <- unique(brPos)
    
    # Find amount of levels
    
    m <- length(brPos)
    
    if (m==1){
      if(brPos == L){
        clust <- t(0:(L-1))
      }
      else{
        # Initialize in case of two clusters
        clust <- matrix(-1, nrow=2, ncol = max(brPos, L-brPos))
        # Define first cluster
        clust[1, 1:brPos] <- (1:brPos)-1
        # Define second cluster
        clust[2, 1:(L-brPos)] <- ((brPos+1):L)-1
      }
    }
    else{
      # Initialize vector of cluster lengths
      
      leng <- vector()
      
      # Find the length of each cluster
      
      leng[1] <- brPos[1]
      for (i in 2:m){
        leng[i] <- brPos[i]-brPos[i-1]
      }
      leng[m+1] <- L-brPos[m]
      
      # Define data dimensions
      
      if (leng[m+1] != 0){
        rows <- m+1
      } else{
        rows <- m
      }
      cols <- max(leng)
      
      # Initialize
      clust <- matrix(-1, nrow = rows, ncol = cols)
      
      clust[1,1:leng[1]] <- (1:brPos[1])-1
      
      if (leng[m+1] != 0){
        clust[m+1,1:leng[m+1]] <- ((brPos[m]+1):L)-1
      }
      
      for (i in 2:m){
        clust[i, 1:leng[i]] <- ((brPos[i-1]+1):brPos[i])-1
      }
    }
  }
  
  # Output
  return(as.data.frame(clust))
}
