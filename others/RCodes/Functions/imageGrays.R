############################################################################################

## Find the order list of gray values a given image 

############################################################################################

# Arguments:
# img an cimg object

# Value:
# imageGrays returns a list with class "numeric" 

############################################################################################

imageGrays <- function(img){
  
  # Convert image to data
  
  data <- as.data.frame(img)
  
  # Find and sort gray values
  
  grays <- sort(data[!duplicated(data$value), ]$value)
  
  # Output
  
  return(grays)
}
