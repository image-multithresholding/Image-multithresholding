############################################################################################

## Convert chromosomes in binary code to thresholds of gray levels

############################################################################################

# Arguments:
# chromo chromosome in binary code
# K number of thresholds

# Value:
# binaryToDecimal returns a list with class "numeric" 

# Required
library("vipor")

############################################################################################

binaryToDecimal <- function(chromo, K){
  
  # Initialize
  
  partition <- matrix(chromo, nrow=K, byrow=T)
  
  # Initialize
  
  thr <- vector()
  
  
  # Find the thresholds
  
  for (i in 1:K){
    thr[i] <- digits2number(rev(partition[i,]), base = 2)
  }
  
  # Order the thresholds
  
  thr <- sort(thr)
  
  # Output
  
  return(thr)
}
