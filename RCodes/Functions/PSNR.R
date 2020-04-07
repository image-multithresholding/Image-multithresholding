#####################################################################################

## Function to compute the peak signal to noise ratio (PSNR) measured in decibel (dB)
## of a thersholded image

#####################################################################################

# Inputs:
# img an cimg object
# thImg a thresholded image of img

# Output:
# real value

#####################################################################################

PSNR <- function(img, thImg){
  
  # Find image dimensions
  
  n <- dim(img)[1]
  m <- dim(img)[2]
  
  # Compute the root mean-squared error (RMSE)
  
  rmse <- sum((img - thImg)^2) / (n * m)
  
  # Compute the peak signal to noise ratio (PSNR)
  
  psnr <- 20 * log(255/rmse) / log(10)

  # Give output

  return(psnr)
}


