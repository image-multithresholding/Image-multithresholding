############################################################################################

## Find searching windows of a cluster as defined in Chang et al. (2002), giving first and
## last element of each cluster

############################################################################################

# Arguments:
# clust a list or vector containing the first and last element in a cluster

# Value:
# searchingWindow returns a list with class "matrix" 

############################################################################################

searchingWindow <- function(clust){
  
  # Find length of the initial cluster 
  
  n <- clust[2]-clust[1]+1
  
  # If n=2, nothing to do
  
  if (n==2){
    w <- t(as.matrix(clust))
  }
  
  else{
  
    # Find length of the searching windows
    
    len <- n/2
    
    # Find the amout of searching windows
    
    total <- n - len +1
    
    # Initialize
    
    # w <- matrix(0, nrow = total, ncol = len)
    w <- matrix(0, nrow = total, ncol = 2)
    
    # Find searching windows
    
    for (j in 1:total){
      w[j,] <- c(clust[1]+j-1, ceiling(clust[1]+j+len-2))
    }
  }
  
  # Output
  
  return(w)
}


