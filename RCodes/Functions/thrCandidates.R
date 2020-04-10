############################################################################################

## Find the nonzero threshold candidates when the L gray-levels are classified into k classes

############################################################################################

# Arguments:
# L number of gray levels
# k number of classes

# Value:
# thrCandidates returns an object with class "matrix" 

############################################################################################

thrCandidates <- function(L,k){
  return(t(combn(1:(L-2), k-1, FUN = NULL, simplify = TRUE)))
}

