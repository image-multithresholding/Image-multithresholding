############################################################################################

## Find valley location (betweeb cells) in a histogram of gray levels

############################################################################################

# Arguments:
# freq the frequency vector of gray levels 1,...,L
# size amount of elements in each cell

# Value:
# valleyLocation returns a list with class "matrix" 

############################################################################################

valleyLocation <- function(freq, size){
  
  # Find the cells
  
  cells <- cellHistogram(freq, size)
  
  # Find arrow direction
  
  direction <- arrowDirection(cells)
  
  # Find amount of cells equal to number of arrow directions
  
  n <- length(direction)
  
  # Initialize
  
  leftCell <- vector()
  rightCell <- vector()
  
  # Find all positions which direction is one
  
  ones <- which(direction == 1)
  
  # Avoid last position
  
  ones <- ones[ones != n]
  
  # Find valleys
  
  for (i in ones){
    
    aux <- direction[(i+1):n]
    
    a <- which(aux !=0)
    
    if (length(a) !=0) {
      h <- which(aux !=0)[[1]]+i
      
      if (direction[h] == -1){
        leftCell <- c(leftCell, i)
        rightCell <- c(rightCell, h)
      }
    }
  }
  
  # Joint data
  
  location <- cbind(leftCell, rightCell)
  
  # Output
  
  return(location)
}

