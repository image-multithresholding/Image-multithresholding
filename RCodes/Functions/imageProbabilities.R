############################################################################################

## Find the probabilities of the gray levels of a given image 

############################################################################################

# Arguments:
# img an cimg object

# Value:
# imageProbabilities returns a list with class "data.frame" containing the following components:
# grays
#    gray level
# prob
#    probability of the gray level

############################################################################################

imageProbabilities <- function(img){
  
  # Convert image to data
  
  data <- as.data.frame(img)
  
  # Normalize gray levels in the range 0,1,...,255
  
  data$value <- round(data$value*255) 
  
  # Find and sort gray levels
  
  grays <- sort(data[!duplicated(data$value), ]$value)
  
  # Find amount of gray levels 
  
  L <- length(grays)
  
  # Compute the frequencies of gray levels
  
  f <- imageHistogram(img)$freq
  
  # Find the total amount of pixels
  
  numPix <- dim(img)[1]*dim(img)[2]
  
  # Initialize
  
  prob <- vector()
  
  # Compute the probabilities
  
  for (i in 1:L){
    prob[i] <- f[i]/numPix
  }
  
  # Joint data with probability
  
  p <- as.data.frame(cbind(grays, prob))
  
  # Output
  
  return(p)
}
