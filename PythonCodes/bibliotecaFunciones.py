from skimage import exposure
import numpy as np
from typing import List, Dict

def thresholded_image(img: np.ndarray, thr: List[int]) -> np.ndarray:
    """thresholdedImage function
    Arguments:
    img a numpy.ndarray object representing an image
    thr a list of thresholds
    Value:
    thresholdingImage returns an object with class "numpy.ndarray" and possible values 0,1,...,255"""

    image = np.copy(img)
    if isinstance(thr, int):
        thr = np.array([thr], dtype=int)
    thr = np.array(thr, dtype=int)
    _, counts = np.unique(thr, return_counts=True) 
    for count in counts:
        if count > 1:
            raise Exception("[Error!]: there are repetead values in 'thr' (list of thresholds) argument")
    _, grays = exposure.histogram(img)
    L = grays.size
    K = thr.size
    if K == 0:
        raise Exception("[Error!]: 'thr', list of thresholds, is empty")
    if K > L:
        raise Exception("[Error!]: 'thr', list of thresholds, contains more values than the amount of gray levels")

    # Compute the grayscale mean in each class
    avg = np.zeros(1)
    avg[0] = np.mean(grays[0:(thr[0]+1)])
    if(K != 1):
        for i in range(1, K):
            avg = np.append(avg, np.mean(grays[(thr[i-1]+1):(thr[i]+1)]))
    avg = np.append(avg, np.mean(grays[(thr[K-1]+1):]))
    avg = np.round(avg)

    # Replace each gray value in the image by the mean of its class
    
    image[(image>=grays[0]) & (image<=grays[thr[0]])] = avg[0]
    if(K != 1):
        for i in range(1, K):
            image[(image>=grays[thr[i-1]+1]) & (image<=grays[thr[i]])] = avg[i]
    image[image>grays[thr[K-1]]] = avg[-1]

    return image

def PSNR(img: np.ndarray, thImg: np.ndarray) -> np.float64:
    """Compute the peak signal to noise ratio (PSNR) measured in decibel (dB) of a thersholded 
    image
    Arguments:
    img a "numpy.ndarray" object representing an image
    thImg a thresholded image of img, a "numpy.ndarray" object
    Value:
    PSNR returns an object with type class 'numpy.float64'"""

    image = np.asarray(np.copy(img), dtype=np.int32)
    thresholded_image = np.asarray(np.copy(thImg), dtype=np.int32)

    if image.shape != thresholded_image.shape:
        raise Exception("[Error!]: the shape of 'img' and 'thImg' does not match")
    
    # Compute the root mean-squared error (RMSE)
    rmse = np.sqrt(np.mean((image - thresholded_image) ** 2))

    # Compute the peak signal to noise ratio (PSNR)
    psnr = 20 * np.log10(np.max(img) / rmse)

    return psnr

def image_probabilities(img: np.ndarray) -> Dict[int, float]:
    probabilities = dict()
    histogram = image_histogram(img)

    for data_frame in histogram:
        probabilities[data_frame.grays] = data_frame.freq / img.size
    
    return probabilities

def gray_clustering(levels: int, breakPositions: List[int], levelsOffset: int = 0) -> List[List[int]]:
    clusters = [[]]

    # Iterate over every level.
    for x in range(levelsOffset, levelsOffset + levels):
        # If we have to break at this level, start a new list.
        if x in breakPositions:
            clusters.append([x])
        # If not, just add this level to the last created cluster.
        else:
            clusters[-1].append(x)
    
    return clusters

def image_histogram(img: np.ndarray) -> Dict[int, int]:
    """Build the histogram of the gray levels of a given image 
    Arguments:
    img a "numpy.ndarray" object 
    Value:
    image_histogram returns a dictionary with gray levels frequencies"""
    
    image = np.copy(img)

    # Find and sort gray levels and frequency
    grays, freq = np.unique(image.flatten(), return_counts=True)
    histogram = dict(list(zip(grays, freq)))

    return histogram

    
def cluster_mean(prob: List[float], clust: List[int], start: int) -> float:
    """Compute the mean of a given cluster 
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    clust a cluster of the gray levels (list of elements of class int)
    start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 
    Value:
    cluster_mean returns an object with class float"""
    
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
    """Compute the variance of a given cluster 
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    clust a cluster of the gray levels (list of elements of class int)
    start 0 or 1 (int) for gray levels 0,...,L-1 or 1,...,L, respectively 
    Value:
    cluster_mean returns an object with class float"""
    
    term = list()
    clusterProb = 0
    
    clusterMean = cluster_mean(prob, clust, start)

    # Compute the variance of each element in the cluster
    # and compute the cluster probability
    for grayLevel in clust:
        term.append(((grayLevel - clusterMean) ** 2)* prob[grayLevel - start]) 
        clusterProb += prob[grayLevel - start]

    clusterVariance = sum(term) / clusterProb

    return clusterVariance


#def prob_up_to_level(prob: List[float], )
