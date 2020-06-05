from skimage import exposure
import numpy as np
from typing import List, Dict

def thresholded_image(img: np.ndarray, thr: List[int]) -> np.ndarray:
    """
    thresholdedImage function

    Arguments:
    img a numpy.ndarray object representing an image
    thr a list of thresholds

    Value:
    thresholdingImage returns an object with class "numpy.ndarray" and possible values 0,1,...,255

    """
    image = np.copy(img)

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
    
    image[(image>=grays[0]) & (image<=grays[thr[0]])] = avg[0]
    if(K != 1):
        for i in range(1, K):
            image[(image>=grays[thr[i-1]+1]) & (image<=grays[thr[i]])] = avg[i]
    image[image>grays[thr[K-1]]] = avg[-1]

    #Output

    return image



def PSNR(img: np.ndarray, thImg: np.ndarray) -> np.float64:
    """
    Compute the peak signal to noise ratio (PSNR) measured in decibel (dB) of a thersholded 
    image

    Arguments:
    img a "numpy.ndarray" object representing an image
    thImg a thresholded image of img, a "numpy.ndarray" object

    Value:
    PSNR returns an object with type class 'numpy.float64'
    """

    image = np.copy(img)
    thresholded_image = np.copy(thImg)

    # Convert images to appropiate data type

    image = np.asarray(image, dtype=np.int32)
    thresholded_image = np.asarray(thresholded_image, dtype=np.int32)

    # Checking if the images shape match

    if(image.shape != thresholdedImage.shape):
        print("[Error!]: the shape of 'img' and 'thImg' does not match")
        exit()
    
    # Compute the root mean-squared error (RMSE)

    rmse = np.sqrt(np.mean((image - thresholdedImage) ** 2))

    # Compute the peak signal to noise ratio (PSNR)

    psnr = 20 * np.log10(np.max(img) / rmse)

    return psnr

def image_histogram(img):
    """
    Build the histogram of the gray levels of a given image 

    Arguments:
    img a "numpy.ndarray" object

    Value:
    imageHistogram returns a list with class "data.frame" containing the following components:
    grays
    gray level
    freq
    frequency of the gray level
    """
    pass

def image_probabilities(img: np.ndarray) -> Dict[int, float]:

    probabilities = dict()

    # Get the histogram for the given image.
    histogram = image_histogram(img)

    # Calculate the probability of each gray level
    for data_frame in histogram:
        probabilities[data_frame.grays] = data_frame.freq / img.size
    
    return probabilities

def gray_clustering()