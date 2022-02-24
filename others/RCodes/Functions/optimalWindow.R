############################################################################################

## Find the optimal window of a cluster as defined in Chang et al. (2002)

############################################################################################

# Arguments:
# prob the probability vector of gray levels 1,...,L
# w bounds of all searching windows of a cluster

# Value:
# optimalWindow returns a list with class "numeric" 

############################################################################################

# Definimos la función

optimalWindow <- function(prob, w){
  
    # Find the amount of searching windows
    
    n <- dim(w)[1]
    
    for (i in 1:n){
      
      # Do nothing if w is of the type a,a+1
    
      if(w[i,2]-w[i,1] == 1){
        bounds <- w[i,]
      }
    
      else{
      
        # Initialize
        
        skew <- vector()
        
        for (i in 1:n){
          skew[i] <- skewness(prob, w[i,1]:w[i,2])
        }
        
        # Find the module of skewnwss
        
        skew <- abs(skew)
        
        # Find the argmin of the module of skewness
        
        argminSkew <- which(skew == min(skew))
        
        # Find the optimal window
        
        optWin <- w[argminSkew, ]
        
        # Find bounds
        
        bounds <- c(optWin[1], optWin[2])
      }
    }
  
  # Output
  
  return(bounds)
}


optimalWindow <- function(prob, w){
  
  n <- dim(w)[1]
  
  # Initialize
  
  skew <- vector()
  
  for (i in 1:n){
    skew[i] <- skewness(prob, w[i,1]:w[i,2])
  }
  
  # Find the module of skewnwss
  
  skew <- abs(skew)
  
  # Find the argmin of the module of skewness
  
  argminSkew <- which(skew == min(skew))
  
  # Find the optimal window
  
  optWin <- w[argminSkew, ]
  
  # Find bounds
  
  bounds <- c(optWin[1], optWin[2])

  # Output
  
  return(bounds)
}
