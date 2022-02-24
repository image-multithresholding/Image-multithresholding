############################################################################################

## Identify peaks in a histogram of gray levels

############################################################################################

# Arguments:
# direction a list of arrow directions as output of arrowDirection function

# Value:
# peakIdentification returns a list with class "integer" 

############################################################################################

peakIdentification <- function(direction){
  
  # Find the amount of gray levels equal to the amount of directions
  
  n = length(direction)
  
  # Initialize
  
  peaks <- vector()
  
  # Find peaks
  
  for (i in 2:(n-1)){
    if (direction[i]==0 & direction[i-1]==-1 & direction[i+1]==1){
      peaks <- c(peaks,i)
    }
  }
  
  # Output
  
  return(peaks)
}
