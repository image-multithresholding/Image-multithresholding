############################################################################################

## Define arrow direction at cells according to the algortitm proposed by Tsai and Chen (1992)

############################################################################################

# Arguments:
# freq the frequency vector of gray levels 1,...,L

# Value:
# arrowDirection returns a list with class "numeric" 

############################################################################################

arrowDirection <- function(freq){
  
  # Find the amount of gray levels equal to the amount of frequencies
  
  n = length(freq)
  
  # Initialize
  
  direction = vector()

  # Find arrow direction at first cell
  
  if (freq[1]!=0 & (freq[1]<freq[2] | freq[1]==freq[2])) {
    direction[1] <- -1
    } 
  else {
      direction[1] <- 0
    }
  
  # Find arrow directions at middle cells
  
  for (i in 2:(n-1)){
    if (freq[i]!=0 & freq[i-1]>freq[i+1] & (freq[i-1]>freq[i] | freq[i-1]==freq[i])) {
      direction[i] <- 1
      } 
    else if(freq[i]!=0 & freq[i+1]>freq[i-1] & (freq[i+1]>freq[i] | freq[i+1]==freq[i])) {
      direction[i] <- -1
        } 
    else {
      direction[i] <- 0
    }
  }
  
  # Find arrow direction at last cell
  
  if (freq[n]!=0 & (freq[n-1]>freq[n] | freq[n-1]==freq[n])) {
    direction[n] <- 1
    } 
  else {
      direction[n] <- 0
  }
  
  # Output
  
  return(direction)
}
