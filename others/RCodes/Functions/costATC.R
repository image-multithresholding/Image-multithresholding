############################################################################################

## Compute the function cost of k classes, i.e. using k-1 thresholds according to the 
## automatic thresholding criterion proposed by Yen et al. (1995)

############################################################################################

# Arguments:
# rho a positive weigth constant
# prob the probability vector of gray levels 0,1,...,L-1
# thr the k-1 thresholds definying the classes

# Value:
# costATC returns an object with class "numeric" 

############################################################################################

costATC <- function(rho, prob, thr){
  
  # Find the discrepancy 
  
  dis <- discrepancy(prob, thr)
  
  # Find the number of classe
  
  k <- length(thr)+1
  
  # Find the cost according to the automatic thresholding criterion
  
  cost <- rho*sqrt(dis)+(log(k)/log(2))^2
  
  # Output
  
  return(cost)
}
  
