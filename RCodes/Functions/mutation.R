############################################################################################

## Mutation for binary encoding

############################################################################################

# Arguments:
# chromosome bit encoding
# position to mutate

# Value:
# mutation returns a list with class "numeric" 

############################################################################################

mutation <- function(chromosome, position){
  
  # Mutate
  
  chromosome[position] <- 1-chromosome[position]
  
  # Output
  
  return(chromosome)
}
