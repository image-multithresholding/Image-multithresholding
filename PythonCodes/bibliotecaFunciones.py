from skimage import exposure
import numpy as np
from typing import List, Dict
from more_itertools import locate

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


def prob_up_to_level(prob: List[float], levels: List[int]) -> float:
    """
Compute a vector of probabilities up to a list of gray levels 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
levels a list of gray levels which give the breaks 

Value:
prob_up_to_level returns an object with class 'list', a float elements list
"""
    # Check single break value

    if(isinstance(levels, int)):
        levels = list(levels)

    # Find the amount of levels

    amountOfLevels = len(levels)

    if(amountOfLevels == 0):
        print("[Error!]: 'levels', list of breaks, is empty") #Error check
        exit()
    if(amountOfLevels > len(prob)):
        print("[Error!]: 'levels', list of breaks, contains more values than the amount of levels in probability list") #Error check
        exit()
    
    # Initialize probUpToLevel list
    
    probUpToLevel = list()

    probUpToLevel.append(sum(prob[0 : levels[0] + 1])) 

    if (amountOfLevels == 1):
    #Find both probabilities
        probUpToLevel.append(1 - probUpToLevel[0])
    else:
    #Find probabilities up to each level
        for i in range(1, amountOfLevels):
            probUpToLevel.append( sum( prob[(levels[i - 1] + 1) : (levels[i] + 1)] ) )
        
        probUpToLevel.append(1 - sum(probUpToLevel))

    return probUpToLevel


def total_correlation(prob: List[float], levels: List[int]) -> float:
    """
Compute the total correlation according to a list of breaking gray levels 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
levels a list of gray levels which give the breaks 

Value:
total_correlation returns an object with class 'float'
"""

    # Check single break value

    if(isinstance(levels, int)):
        levels = list(levels)

    # Find the probabilities according to the given levels

    probUpToLevel = prob_up_to_level(prob, levels)

    # Find the number of breaks and probabilities
  
    amountOfLevels = len(levels)
    amountOfProbabilities = len(prob)

    # Initialize correlations list

    correlations = list()

    correlations.append( -np.log( sum( np.square( prob[0 : (levels[0] + 1)] ) ) / probUpToLevel[0] ** 2) )

    if (amountOfLevels == 1):
        # Find the correlation of both intervals
        correlations.append( -np.log( sum( np.square( prob[(levels[0] + 1) : amountOfProbabilities + 1] ) ) / probUpToLevel[1] ** 2) )
    else:
        # Find the correlation of each interval
        for i in range(1, amountOfLevels):
            correlations.append( -np.log( sum( np.square( prob[(levels[i - 1] + 1) : (levels[i] + 1)] ) ) / probUpToLevel[i] ** 2) )

        correlations.append( -np.log( sum( np.square( prob[(levels[amountOfLevels - 1] + 1) : amountOfProbabilities] ) ) / probUpToLevel[amountOfLevels] ** 2) )

    # Find the total correlation

    totalCorrelation = sum(correlations)

    return totalCorrelation


def total_entropy(prob: List[float], levels: List[int]) -> float:
    """
Compute the total entropy according to a list of breaking gray levels 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
levels a list of gray levels which give the breaks 

Value:
total_correlation returns an object with class 'float'
"""

    # Check single break value

    if(isinstance(levels, int)):
        levels = list(levels)

    # Find the probabilities according to the given levels

    probUpToLevel = prob_up_to_level(prob, levels)

    # Find the number of breaks and probabilities
  
    amountOfLevels = len(levels)
    amountOfProbabilities = len(prob)

    prob = np.array(prob)

    # Initialize entropies list

    entropies = list()

    entropies.append( -np.sum( np.multiply( prob[0 : levels[0]], np.log( prob[0 : levels[0]] / probUpToLevel[1] ) ) ) / probUpToLevel[1] )

    if (amountOfLevels == 1):
        # Find the entropy of both intervals
        entropies.append( -np.sum( np.multiply( prob[(levels[0] + 1) : amountOfProbabilities], np.log( prob[(levels[0] + 1) : amountOfProbabilities] / probUpToLevel[2] ) ) ) / probUpToLevel[2] )
    else:
        # Find the entropy of each interval
        for i in range(1, amountOfLevels):
            entropies.append( -np.sum( np.multiply( prob[(levels[i - 1] + 1) : (levels[i] + 1)], np.log( prob[(levels[i - 1] + 1) : (levels[i] + 1)] / probUpToLevel[i] ) ) ) / probUpToLevel[i] )

        entropies.append( -np.sum( np.multiply( prob[(levels[amountOfLevels - 1] + 1) : amountOfProbabilities], np.log( prob[(levels[amountOfLevels - 1] + 1) : amountOfProbabilities] / probUpToLevel[amountOfLevels] ) ) ) / probUpToLevel[amountOfLevels] )

    # Find the total entropy

    totalEntropy = sum(entropies)

    return totalEntropy


def argmax_TC(prob: List[float]) -> List[int]:
    """
Compute the levels at which the maximum total correlation is reached 

Arguments:
prob the probability list of gray levels (list of elements of class float, value from 0 to 1) 

Value:
argmax_TC returns an object with class 'list', list of float elements
"""

    # Find the amount of gray levels equal to the amount of probabilities

    amountOfProbabilities = len(prob)

    # Initialize totalCorrelations list

    totalCorrelations = list()

    # Find the total correlation varying the break level 
    # (notice that it makes no sense to consider the interval extrems since no partition holds)

    for i in range(0, amountOfProbabilities - 2):
        totalCorrelations.append(total_correlation(prob, i))

    # Find the maximum total correlation

    maxTotalCorrelation = max(totalCorrelations)

    # Find the level at which the maximum total entropy is reached

    argmax = locate(totalCorrelations, lambda x: x == maxTotalCorrelation)

    return argmax