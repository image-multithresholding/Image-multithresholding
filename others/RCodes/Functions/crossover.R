############################################################################################

## Standard crossover for binary encoding

############################################################################################

# Arguments:
# parent1 bit encoding
# parent2 bit encoding
# position in which crossover is made

# Value:
# crossover returns an object with class "matrix" 

############################################################################################

crossover <- function(parent1, parent2, position){
  
  # Find length of parents
  
  n <- length(parent1)
  
  # Initialize
  
  child1 <- vector()
  child2 <- vector()
  
  # Find children before position
  
  for (i in 1:position){
    child1[i] <- parent1[i]
    child2[i] <- parent2[i]
  }
  
  # Find children after position
  
  for (i in (position+1):n){
    child1[i] <- parent2[i]
    child2[i] <- parent1[i]
  }
  
  # Joint children
  
  children <- rbind(child1, child2)
  
  # Output
  
  return(children)
}
