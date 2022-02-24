############################################################################################

## Find the valleys of a histogram of gray levels 

############################################################################################

# Arguments:
# hist frequencies in the histogram of gray levels 0,1,...,L-1

# Value:
# valleys returns a list with class "integer" 

############################################################################################

valleys <- function(hist){
  
  # Find the amount of gray leves equal to histogram length 
  
  L <- length(hist)
  
  # Initialize
  
  toRight <- rep(0, L)
  toLeft <- rep(0, L)
  
  # Find when a frequency is less than or equal to the following one
  
  for (i in 1:(L-1)){
    toRight[i] <- (hist[i] < hist[i+1]) | (hist[i] == hist[i+1])
  }
  
  # Find when a frequency is less than the previous one
  
  for (i in 2:L){
    toLeft[i] <- (hist[i] < hist[i-1])
  }
  
  # Find when both condition hold
  
  both <- toRight + toLeft
  
  # Find the valleys
  
  val <- which(both == 2)
  
  # Output
  
  return(val)
}
