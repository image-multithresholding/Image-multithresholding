############################################################################################

## Genetate a vector of a given number of bits

############################################################################################

# Arguments:
# n number of bits

# Value:
# bitVector returns a list with class "numeric" 

############################################################################################

## Definimos una función para generar un vector de n bits

bitVector <- function(n){
  
  # Generate vector
  
  vector <- sample(c(0,1), replace=TRUE, size=n)
  
  # Output
  
  return(vector)
}
