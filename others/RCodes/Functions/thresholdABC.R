############################################################################################

## Find thresholds of the gray levels according to the method proposed by Horng (2011)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes

# Value:
# thresholdABC returns a list with class "numeric" 

# Required
library(imagerExtra)

############################################################################################

thresholdABC <- function(img, k){
  
  # Find the thresholds
  
  thr <- ThresholdML(img, k-1, returnvalue = TRUE)
  
  # Convert gray level to range 0,1,...255
  
  thr <- round(thr*255) 
  
  # Output
  
  return(thr)
}


