############################################################################################

## Apply gaussian normal function with a given weigth

############################################################################################

# Arguments:
# x real value in the domain
# mu mean parameter
# sigma standard deviation parameter
# p weight of proportion

# Value:
# weightedGaussian returns an object with class "numeric" 

############################################################################################


weightedGaussian <- function(x, mu, sigma, p){
  
  # Compute the gaussian value
  
  y <- p/(sqrt(2*pi)*sigma) * exp(-0.5 * ((x-mu)/sigma)^2)
  
  # Output
  
  return(y)
}
