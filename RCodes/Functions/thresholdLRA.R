############################################################################################

## Find thresholds of the gray levels according to the method proposed by Papamarkos and 
## Gatos (1994)

############################################################################################

# Arguments:
# img an cimg object
# k number of classes
# n numerator degree of real rational function to be fitted (2 or 3)
# m denominator degree of real rational function to be fitted (2 or 3)

# Value:
# thresholdLRA returns a list with class "numeric" 

############################################################################################

thresholdLRA <- function(img, k, n, m){
  
  # Find argmin of aproximation of real rational functions according to n and m
  
  # For n=m=2
  
  argminRF22 <- function(x, y){
    
    # Find points to be fitted
    
    points <- as.data.frame(cbind(x, y))
    
    # Find fitting function
    
    fit <- lm(y ~ x + I(x^2) + I(y*x) + I(y*x^2), points)
    
    # Find coefficients
    
    fitCoef <- as.list(coef(fit))
    
    # Define rational function
    
    rational <- function(z){
      (fitCoef$`(Intercept)` + fitCoef$x * z+ fitCoef$`I(x^2)` * z^2)/
        (1 - fitCoef$`I(y * x)` *z - fitCoef$`I(y * x^2)` * z^2)
    }
    
    # Find argmin
    
    argmin <- which(rational(x) == min(rational(x)))
    
    # Output
    
    return(x[argmin])
  }
  
  # For n=2 and m=3
  
  argminRF23 <- function(x, y){
    
    # Find points to be fitted
    
    points <- as.data.frame(cbind(x, y))
    
    # Find fitting function
    
    fit <- lm(y ~ x + I(x^2) + I(x^3) + I(y*x) + I(y*x^2), points)
    
    # Find coefficients
    
    fitCoef <- as.list(coef(fit))
    
    # Define rational function
    
    rational <- function(z){
      (fitCoef$`(Intercept)` + fitCoef$x * z+ fitCoef$`I(x^2)` * z^2 + fitCoef$`I(x^3)` * z^3)/
        (1 - fitCoef$`I(y * x)` *z - fitCoef$`I(y * x^2)` * z^2)
    }
    
    # Find argmin
    
    argmin <- which(rational(x) == min(rational(x)))
    
    # Output
    
    return(x[argmin])
  }
  
  # For n=3 and m=2
  
  argminRF32 <- function(x, y){
    
    # Find points to be fitted
    
    points <- as.data.frame(cbind(x, y))
    
    # Find fitting function
    
    fit <- lm(y ~ x + I(x^2) + I(y*x) + I(y*x^2) + I(y*x^3), points)
    
    # Find coefficients
    
    fitCoef <- as.list(coef(fit))
    
    # Define rational function
    
    rational <- function(z){
      (fitCoef$`(Intercept)` + fitCoef$x * z+ fitCoef$`I(x^2)` * z^2)/
        (1 - fitCoef$`I(y * x)` *z - fitCoef$`I(y * x^2)` * z^2 - fitCoef$`I(y * x^3)` * z^3)
    }
    
    # Find argmin
    
    argmin <- which(rational(x) == min(rational(x)))
    
    # Output
    
    return(x[argmin])
  }
  
  # Find the frequencies of the gray leves 1,...,L
  
  freq <- imageHistogram(img)$freq
  
  # Find the amount of gray levels equal to number of frequencies
  
  L <- length(freq)
  
  # Find the cell size 
  
  cellSize <- hillIdentification(freq, k)$cellsize
  
  # Find the peaks location
  
  peakLocation <- hillIdentification(freq, k)$peaks
  
  # Find the cells explicitly
  
  cells <- uncell(L, cellSize)
  
  # Initialize
  
  thr <- vector()
  
  # Find first valley
  
  if (length(peakLocation) == 1){
    valley <- as.vector(cells[peakLocation,])
  } else{
   valley <- sort(as.vector(cells[peakLocation[1]:peakLocation[2],]))
  }
  
  # Find first threshold
  
  if (n == 2 & m == 2){
    thr[1] <- argminRF22(valley, freq[valley])
  }
  
  if (n == 2 & m == 3){
    thr[1] <- argminRF23(valley, freq[valley])
  }
  
  if (n == 3 & m == 2){
    thr[1] <- argminRF32(valley, freq[valley])
  }
  
  # Find the rest of thresholds
  
  if (k != 2){
    for (i in 2:(k-1)){
      
      if (length(peakLocation) == k-1){
        valley <- as.vector(cells[peakLocation[i]+1,])
      }
      else{
        valley <- sort(as.vector(cells[(peakLocation[i]+1):peakLocation[i+1],]))
      }
      
      if (n ==2 & m == 2){
        thr[i] <- argminRF22(valley, freq[valley])
      }
      
      if (n ==2 & m == 3){
        thr[i] <- argminRF23(valley, freq[valley])
      }
      
      if (n ==3 & m == 2){
        thr[i] <- argminRF32(valley, freq[valley])
      }
    }
  }
  
  
  # Output
  
  return(thr)
}





