############################################################################################

## Produce the thresholded image of a given image according to the given thresholds

############################################################################################

# Arguments:
# img an cimg object
# thr a list of thresholds

# Value:
# thresholdingImage returns an object with class "cimg" and possible values 0,1,...,255

############################################################################################

thresholdingImage <- function(img, thr){
  
  # Convert image to data
  
  data <- as.data.frame(img)
  
  # Normalize gray levels in the range 0,1,...,255
  
  data$value <- round(data$value*255) 
  
  # Find and sort gray levels
  
  grays <- sort(data[!duplicated(data$value), ]$value)
  
  # Find amount of gray levels and thresholds
  
  L <- length(grays)
  K <- length(thr)
  
  # Compute the grayscale mean in each class
  
  avg <- vector()
  avg[1] <- mean(grays[1:thr[1]])
  if(K != 1){
    for (i in 2:K){
    avg[i] <- mean(grays[(thr[i-1]+1):thr[i]])
    }
  }
  avg[K+1] <- mean(grays[(thr[K]+1):L])
  
  # Round the means
  
  avg <- round(avg)
  
  # Replace each gray value in the image by the mean of its class
  
  data$value[data$value %in% grays[1:thr[1]]] <- avg[1]
  if(K != 1){
    for (i in 2:K){
    data$value[data$value %in% grays[(thr[i-1]+1):thr[i]]] <- avg[i]
    }
  }
  data$value[data$value %in% grays[(thr[K]+1):L]] <- avg[K+1]
  
  # Convert to cimg object 
  
  data <- as.cimg(data)

  # Output
  
  return(data)
}

########################################################################

