library(imager)
library(SpatialPack)

setwd("C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Code/Thresholding/Routines")
source("imageHistogram.R")
source("imageProbabilities.R")
source("probUpToLevel.R")
source("grayClustering.R")
source("clusterMean.R")
source("clusterVar.R")
source("totalCorrelation.R")
source("argmaxTC.R")
source("discrepancy.R")
source("costATC.R")
source("thresholdATC.R")
source("thresholdedImage.R")
source("PSNR.R")

######################################################################################################

## For Synthetic image ##

######################################################################################################

synthetic <- load.image('C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Images/SAR Images/synthetic.jpg')
plot(synthetic, axes=FALSE)

## FIND THRESHOLDS

thr2 <- thresholdATC(synthetic,2) # 2 classes
thr3 <- thresholdATC(synthetic,3) # 3 classes
thr4 <- thresholdATC(synthetic,4) # 4 classes

######################################################################################################

## COMPUTE TIME

system.time(thresholdATC(synthetic,2))
system.time(thresholdATC(synthetic,3))
system.time(thresholdATC(synthetic,4))

######################################################################################################

## FIND THRESHOLDED IMAGES AND SAVE THEM

syntheticATC2 <- thresholdedImage(synthetic, thr2)
syntheticATC3 <- thresholdedImage(synthetic, thr3)
syntheticATC4 <- thresholdedImage(synthetic, thr4)

save.image(syntheticATC2, "C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Images/Thresholded/syntheticATC2.jpg", quality = 0.7)
save.image(syntheticATC3, "C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Images/Thresholded/syntheticATC3.jpg", quality = 0.7)
save.image(syntheticATC4, "C:/Users/Usuario/Dropbox/Angeles-Gisela-Andrea/Images/Thresholded/syntheticATC4.jpg", quality = 0.7)

######################################################################################################

## MAKE A SIMULTANEOUS IMAGE

layout(matrix(c(rep(c(rep(1,4),rep(2,4)),4), rep(c(rep(3,4),rep(4,4)),4)), 
              nrow = 8, ncol = 8, byrow = TRUE))

plot(synthetic, axes=FALSE)
title("Original image", cex.main=0.8)
plot(syntheticATC2, axes=FALSE)
title("2 classes by ATC", cex.main=0.8)
plot(syntheticATC3, axes=FALSE)
title("3 classes by ATC", cex.main=0.8)
plot(syntheticATC4, axes=FALSE)
title("4 classes by ATC", cex.main=0.8)

######################################################################################################

## COMPUTE PSNR

PSNR(synthetic, syntheticATC2)
PSNR(synthetic, syntheticATC3)
PSNR(synthetic, syntheticATC4)

######################################################################################################

## COMPUTE SSIM

SSIM(synthetic, syntheticATC2)$SSIM
SSIM(synthetic, syntheticATC3)$SSIM
SSIM(synthetic, syntheticATC4)$SSIM
