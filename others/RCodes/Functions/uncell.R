############################################################################################

## Find the original gray levels from a cell grouping

############################################################################################

# Arguments:
# L amount of gray levels 1,...,L
# size amount of elements in each cell

# Value:
# uncell returns a list with class "matrix" 

############################################################################################

uncell <- function(L, size){
  
  # Find the amount of cells
  
  n <- ceiling(L/size)
  
  # Find the rest of division
  
  rest <- L%%size
  
  # Find remainig places
  
  complete <- size-rest
  
  # Initialize
  
  cellSplit <- c(1:L, rep(0, complete))
  
  # Find the elements in each cell
  
  cellSplit <- matrix(cellSplit, ncol = size, byrow = T)
  
  # Output 
  
  return(cellSplit)
}

