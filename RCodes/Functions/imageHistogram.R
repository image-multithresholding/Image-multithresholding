#####################################################################################

## Build the histogram of the gray levels of a given image 

#####################################################################################

# Arguments:
# img an cimg object

# Value:
# imageHistogram returns a list with class "data.frame" containing the following components:
# grays
#    gray level
# freq
#    frequency of the gray level

##################################################################################

imageHistogram <- function(img){
  
  # Convert image to data
  
  data <- as.data.frame(img)
  
  # Normalize gray levels in the range 0,1,...,255
  
  data$value <- round(data$value*255) 
  
  # Find and sort gray levels
  
  grays <- sort(data[!duplicated(data$value), ]$value)
  
  # Find amount of gray levels 
  
  L <- length(grays)
  
  # Initialization
  
  freq <- rep(0,L)
  
  # Find the histogram
  
  for (i in 1:L){
    freq[i] <- sum(data$value == grays[i])
  }
  
  # Joint data with frequency
  
  hist <- as.data.frame(cbind(grays, freq))
  
  # Output
  
  return(hist)
}
