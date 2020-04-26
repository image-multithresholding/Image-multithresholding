############################################################################################

## Solutions to quadratic equation

############################################################################################

# Arguments:
# a quadratic corefficient
# b linear corefficient
# c independent corefficient

# Value:
# quadraticSolve returns a list with class "numeric" or "character"

############################################################################################


quadraticSolve <- function(a, b, c){
  
  if(b^2-4*a*c > 0){
    
    # Find first solution
    
    root1 <- (-b + sqrt(b^2 - 4*a*c)) / (2*a)
    
    # Find second solution
    
    root2 <- (-b - sqrt(b^2 - 4*a*c)) / (2*a)
    
    # Solution
    
    sol <- c(root1, root2)
    
  }
  
  if(b^2-4*a*c == 0){
    
    # Find unique solution
    
    root <- -b / (2*a)
    
    # Solution
    
    sol <- root
    
  }
  
  if(b^2-4*a*c < 0){
    
    # Solution
    
    sol <- "No solution"
    
  }
  
  # Output
  
  return(sol)
}
