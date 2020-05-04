############################################################################################

## Find thresholds of the gray levels according to the method proposed by Arora el al. (2008)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes that must be odd

# Value:
# thresholdFSR returns a list with class "numeric" 

############################################################################################

thresholdFSR <- function(img, k){
  
  # Find the gray values
  
  grays <- imageGrays(img)
  
  # Find the amount of iterations
  
  n <- (k-1)/2-1
  
  # Initialize
  
  a=0
  b=255
  thr = vector()
  interval <- grays
  
  # Find thresholds
  
  for (i in 1:n){
    
    # Find the mean of the interval
    
    mu <- mean(interval)
    
    # Find the standard deviation of the interval
    
    st <- sqrt(var(interval))
    
    # Find bounds
    
    left <- mu-st
    right <- mu+st
    
    # Find positive differences
    
    leftDif <- abs(interval-left)
    rightDif <- abs(interval-right)
    
    # Find subranges boundaries
    
    T1 <- min(which(leftDif == min(leftDif)))+a
    T2 <- max(which(rightDif == min(rightDif)))+a
    
    # Find new interval boundaries
    
    a <- T1+1
    b <- T2-1
    
    # Find interval of grays
    
    interval <- grays[a:b]
    
    # Find threshold
    
    thr <- c(thr, T1, T2)
  } 
  
  # Output
  
  return(sort(thr))
}
