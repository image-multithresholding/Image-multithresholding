from skimage import exposure
import numpy as np
from typing import List, Dict

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

    # Checking if the images shape match

    if(image.shape != thresholdedImage.shape):
        print("[Error!]: the shape of 'img' and 'thImg' does not match")
        exit()
    
    # Compute the root mean-squared error (RMSE)

    rmse = np.sqrt(np.mean((image - thresholdedImage) ** 2))

    # Compute the peak signal to noise ratio (PSNR)

    psnr = 20 * np.log10(np.max(img) / rmse)

    return psnr

def image_histogram(img: np.ndarray) -> Dict[int, int]:
    
    """
Build the histogram of the gray levels of a given image 

Arguments:
img a "numpy.ndarray" object 

Value:
image_histogram returns a dictionary with gray levels frequencies

"""
    image = np.copy(img)

    # Find and sort gray levels and frequency
    
    grays, freq = np.unique(image.flatten(), return_counts=True)

    #Create histogram dictionary

    histogram = dict(list(zip(grays, freq)))

    return histogram

    
def cluster_mean(prob: List[float], clust: List[int], start: int) -> float:
    """
Compute the mean of a given cluster 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
clust a cluster of the gray levels (list of elements of class int)
start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 

Value:
cluster_mean returns an object with class float
"""
    #Initialize term list and cluster prbability accumulator

    term = list()
    clusterProb = 0

    # Compute the expected value of each element in the cluster
    # and compute the cluster probability

    for grayLevel in clust:
        term.append(grayLevel * prob[grayLevel - start]) 
        clusterProb += prob[grayLevel - start]

    # Compute the cluster mean

    clusterMean = sum(term) / clusterProb

    return clusterMean


def cluster_var(prob: List[float], clust: List[int], start: int) -> float:
    """
Compute the variance of a given cluster 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
clust a cluster of the gray levels (list of elements of class int)
start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 

Value:
cluster_mean returns an object with class float
"""
    #Initialize term list and cluster prbability accumulator

    term = list()
    clusterProb = 0

    # Compute the cluster mean
    
    clusterMean = cluster_mean(prob, clust, start)

    # Compute the variance of each element in the cluster
    # and compute the cluster probability

    for grayLevel in clust:
        term.append(((grayLevel - clusterMean) ** 2)* prob[grayLevel - start]) 
        clusterProb += prob[grayLevel - start]

    # Compute the variance of the cluster

    clusterVariance = sum(term) / clusterProb

    return clusterVariance


def prob_up_to_level(prob: List[float], )