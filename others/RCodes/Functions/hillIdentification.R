############################################################################################

## Identify given number of peaks in a histogram of gray levels and cell size

############################################################################################

# Arguments:
# freq the frequency vector of gray levels 1,...,L
# k amount of desire peaks

# Value:
# hillIdentification returns a list with class "integer" containing the following components:
# peaks
#    detected peaks
# cellsize
#    size of cells

############################################################################################

hillIdentification <- function(freq, k){ 
  
  # Initialize
  
  size <- 0
  
  # Repeat process until reaching desired number of peaks
  
  repeat{
    
    # Increase cell size
    
    size <- size+1
    
    # Find the cells
    
    cells <- cellHistogram(freq, size)
    
    # Find arrow direction
    
    direction <- arrowDirection(cells)
    
    # Identify peaks
    
    peaks <- peakIdentification(direction)
    
    # Find amount of peaks
    
    numberPeaks <- length(peaks)
    
    if (numberPeaks < k | numberPeaks == k){
      break
    }
  }
  
  # Joint data
  
  output <- list("peaks" = peaks, "cellsize" = size)
  
  # Output
  
  return(output)
}

