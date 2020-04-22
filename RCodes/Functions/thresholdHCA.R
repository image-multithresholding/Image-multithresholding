############################################################################################

## Find thresholds of the gray levels according to Tsai and Chen method (1992)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdHCA returns a list with class "integer" 

############################################################################################

thresholdHCA <- function(img, k){
  
  # Find the frequecies of the gray leves 1,...,L
  
  freq <- imageHistogram(img)$freq
  
  # Find the amount of gray levels 
  
  L <- length(grays)
  
  # Find the cell size 
  
  cellSize <- hillIdentification(freq, k-1)$cellsize
  
  # Find the valley location
  
  valLocation <- valleyLocation(freq, cellSize)
  
  # Find amount of valleys
  
  n <- dim(valLocation)[1]
  
  # Find the cells explicitly
  
  cells <- uncell(L, cellSize)
  
  # Initialize
  
  thr <- vector()
  
  # Find the thresholds
  
  for (i in 1:n){
    
    startLeftCell <- cells[valLocation[i,1],1]
    endRightCell <- cells[valLocation[i,2], cellSize]
    thr[i] <- round(startLeftCell + (endRightCell - startLeftCell)/2, 0)
  }
  
  # Output
  
  return(thr)
}




