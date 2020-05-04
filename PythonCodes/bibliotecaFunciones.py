from skimage import io,util,exposure, measure
import numpy as np
import numpy.ma as ma

def thresholdedImage(img, thr):
    
    """
thresholdedImage function

Arguments:
img a "numpy.ndarray" object
thr a list of thresholds

Value:
thresholdingImage returns an object with class "numpy.ndarray" and possible values 0,1,...,255

"""

    image = np.copy(img)

    # Normalize gray levels in the range 0,1,...,255
    
    image = util.img_as_ubyte(image) # no iría!!

    #Convert threshold to numpy array

    if(isinstance(thr, int)):
        thr = np.array([thr], dtype=int)

    thr = np.array(thr, dtype=int)

    _, counts = np.unique(thr, return_counts=True) 
    for count in counts:
        if(count == 1):
            continue
        print("[Error!]: there are repetead values in 'thr' (list of thresholds) argument") #Error check
        exit()
    
    # Find and sort gray levels
    
    _, grays = exposure.histogram(img)
    
    # Find amount of gray levels and thresholds

    L = grays.size
    
    K = thr.size
    
    if(K == 0):
        print("[Error!]: 'thr', list of thresholds, is empty") #Error check
        exit()
    if(K>L):
        print("[Error!]: 'thr', list of thresholds, contains more values than the amount of gray levels") #Error check
        exit()

    # Compute the grayscale mean in each class

    avg = np.zeros(1)
    avg[0] = np.mean(grays[0:(thr[0]+1)])
    if(K != 1):
        for i in range(1, K):
            avg = np.append(avg, np.mean(grays[(thr[i-1]+1):(thr[i]+1)]))
    avg = np.append(avg, np.mean(grays[(thr[K-1]+1):]))

    # Round the means

    avg = np.round(avg)

    # Replace each gray value in the image by the mean of its class

    np.place(image, ma.masked_inside(image, grays[0], grays[thr[0]]), avg[0])
    if(K != 1):
        for i in range(1, K):
            np.place(image, ma.masked_inside(image, grays[thr[i-1]+1], grays[thr[i]]), avg[i])
    np.place(image, ma.masked_inside(image, grays[thr[K-1]+1], grays[L-1]), avg[K])


    #Output

    return image



def PSNR(img, thImg):

    """
Compute the peak signal to noise ratio (PSNR) measured in decibel (dB) of a thersholded 
image

Arguments:
img a "numpy.ndarray" object
thImg a thresholded image of img, a "numpy.ndarray" object

Value:
PSNR returns an object with type class 'numpy.float64'
"""

    image = np.copy(img)
    thresholdedImage = np.copy(thImg)

    # Convert images to appropiate data type

    image = np.asarray(image, dtype=np.int32)
    thresholdedImage = np.asarray(thresholdedImage, dtype=np.int32)

    print(image)
    print(thresholdedImage)
    print((image-thresholdedImage)**2)
    # Checking if the images shape match

    if(image.shape != thresholdedImage.shape):
        print("[Error!]: the shape of 'img' and 'thImg' does not match")
        exit()
    
    # Compute the root mean-squared error (RMSE)

    rmse = np.mean((image - thresholdedImage) ** 2)

    # Compute the peak signal to noise ratio (PSNR)

    psnr = 10 * np.log10(255**2 / rmse)

    return psnr


