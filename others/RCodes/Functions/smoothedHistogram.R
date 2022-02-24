############################################################################################

## Smooth a histogram of gray levels using a Gaussian kernel

############################################################################################

# Arguments:
# hist frequencies in the histogram of gray levels 0,1,...,L-1
# p giving the 2p+1 bins (windows size)

# Value:
# smoothedHistogram returns a list with class "numeric" 

############################################################################################

smoothedHistogram <- function(hist, p){
  
  # Find the amount of gray leves equal to histogram length 
  
  L <- length(hist)
  
  # Find the windows size
  
  size <- 2*p+1
  
  # Initialize
    
  bin <- vector()
  
  # Define the Gaussian mask windows 
  
  for (i in 1:size){
    bin[i] <- 0.5*(1-cos(i*pi/p))
    }
  
  # Initialize
  
  smoothHist <- vector()
  
  # Find special bounds
  
  leftBound <- p+1
  rightBound <- L-p
  
  # Find the convolution between the histogram and the Gaussian mask windows in the middle places
  
  for (i in leftBound:rightBound){
    
    term <- vector()
    termPosition <- 1
    
    for (j in -p:p){
      binPosition <- j+p+1
      histPosition <- i+j
      term[termPosition] <- bin[binPosition]*hist[histPosition]
      termPosition <- termPosition + 1
    }
    
    smoothHist[i] <- sum(term)
  }
  
  # Find the convolution between the histogram and the Gaussian mask windows in the left tail
  
  for (i in 1:p){
    
    term <- vector()
    termPosition <- 1
    a <- 1-i
    
    for (j in a:p){
      binPosition <- j+p+1
      histPosition <- i+j
      term[termPosition] <- bin[binPosition]*hist[histPosition]
      termPosition <- termPosition + 1
    }
    
    smoothHist[i] <- sum(term)
  }
  
  # Find the convolution between the histogram and the Gaussian mask windows in the right tail
  
  c <- L-p+1
  
  for (i in c:L){
    
    term <- vector()
    termPosition <- 1
    a <- L-i
    
    for (j in -p:a){
      binPosition <- j+p+1
      histPosition <- i+j
      term[termPosition] <- bin[binPosition]*hist[histPosition]
      termPosition <- termPosition + 1
    }
    
    smoothHist[i] <- sum(term)
  }
  
  # Find the smoothed histogram
  
  smoothHist <- round(smoothHist/size)
  
  # Output
  
  return(smoothHist)
}


