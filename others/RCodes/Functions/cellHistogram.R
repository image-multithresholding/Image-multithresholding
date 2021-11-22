############################################################################################

## Find histogram of cells of gray levels

############################################################################################

# Arguments:
# freq the frequency vector of gray levels 1,...,L
# size amount of elements in each cell

# Value:
# cellHistogram returns a list with class "numeric" 

############################################################################################

cellHistogram <- function(freq, size){
  
  # Find the amount of cells
  
  n <- ceiling(length(freq)/size)
  
  # Find the rest of division
  
  rest <- length(freq)%%size
  
  # Find remainig places
  
  complete <- size-rest
  
  # Arrange frequency vector to have length a multiple of cell size
  
  freq <- c(freq, rep(0, complete))
  
  # Split into cells
  
  cuts <- matrix(freq, ncol = size, byrow = T)
  
  # Initialize
  
  cell <- vector()
  
  # Find cells
  
  for (i in 1:n){
    cell[i] <- sum(cuts[i,])}
  
  # Output
  
  return(cell)
}
