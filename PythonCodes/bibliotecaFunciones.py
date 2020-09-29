from skimage import exposure
import numpy as np
from typing import List, Dict, Tuple
from math import cos, pi, ceil, sqrt, exp

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

    entropies.append( -np.sum( np.multiply( prob[0 : levels[0] + 1], np.log( prob[0 : levels[0] + 1] / probUpToLevel[0] ) ) ) / probUpToLevel[0] )

    if (amountOfLevels == 1):
        # Find the entropy of both intervals
        entropies.append( -np.sum( np.multiply( prob[(levels[0] + 1) : amountOfProbabilities], np.log( prob[(levels[0] + 1) : amountOfProbabilities] / probUpToLevel[1] ) ) ) / probUpToLevel[1] )
        print(entropies)
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
        totalCorrelations.append(total_correlation(prob, [i]))

    # Find the maximum total correlation

    maxTotalCorrelation = max(totalCorrelations)

    # Find the level at which the maximum total entropy is reached

    argmax = totalCorrelations.index(maxTotalCorrelation)

    return argmax


def image_probabilities(img: np.ndarray) -> List[float]:
    probabilities = [.0 for _ in range(256)]
    histogram = image_histogram(img)

    for gray in histogram:
        probabilities[gray] = histogram[gray] / img.size
    
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


def threshold_ATC(img: np.ndarray, k: int) -> List[int]:
    """
Compute the optimal number of classes according to the automatic thresholding criterion 
proposed by Yen et al. (1995) 

Arguments:
img a numpy.ndarray object
k number of thresholds 

Value:
threshold_ATC returns an object with class 'list', list of integer elements
"""

    if k < 1:
        raise Exception("The number of thresholds must be greater than or equal to 1")

    image = np.copy(img)

    # Find the vector of probabilities of the gray leves 0,1,...,L-1

    prob = np.array(image_probabilities(image))

    amountOfProbabilities = len(prob)

    print("Probabilities", amountOfProbabilities)
    newClust = list(i for i in range(0, amountOfProbabilities))
    thr = list([])

    for i in range(0, k):

        print("Newclust:", newClust)
        # Find the new threshold using maximum total correlation criterion
        
        thr.append(newClust[argmax_TC(prob[newClust[0] : newClust[-1] + 1] / sum(prob[newClust[0] : newClust[-1] + 1]))])

        thr.sort()

        print("thr:", thr)
        # Find the classes according to the thresholds

        clust = gray_clustering(amountOfProbabilities, thr)

        print(clust)
        # Find the variance per class

        varClust = list()
        
        for j in range(0, i+2):

            varClust.append(cluster_var(prob, clust[j], 0))

        # Find the argmax of cluster variances
        print(varClust)
        argMaxVar = varClust.index(max(varClust))

        print("argmax: ", argMaxVar)
        # Define the new lass to be partitioned

        newClust = clust[argMaxVar]

    return thr


def valleys(hist: Dict[int, int]) -> List[int]:
    """
Find the valleys of a histogram of gray levels 

Arguments:
hist frequencies in the histogram of gray levels 0,1,...,L-1 (dictionary) 

Value:
valleys returns an object with class 'list', list of integer values
"""

    L = len(hist)

    toRight = list([False]*L)
    toLeft = list([False]*L)

    # Find when a frequency is less than or equal to the following one

    for i in range(0, L-1):
        toRight[i] = hist[i] <= hist[i+1]

    # Find when a frequency is less than the previous one

    for i in range(1, L):
        toLeft[i] = hist[i] < hist[i-1]
    
    # Find when both condition hold

    both = list(i and j for i,j in zip(toRight, toLeft))

    val = list(i for i, x in enumerate(both) if x == True)

    return val


def valley_clustering(L: int, val: List[int]) -> np.ndarray:
    """
Find limits of the clusters of a histogram of gray levels according to given valleys 

Arguments:
L number of gray levels 1,...,L 
val list of valleys

Value:
valley_clustering returns an object with class 'np.ndarray'
"""

    # Find the amount of clusters

    n = len(val) + 1

    clust = np.zeros((n, 2), dtype=np.uint8)

    # Find clusters

    clust[0] = [0, val[0] - 1]

    for i in range(1, n-1):
        clust[i] = [val[i-1], val[i] - 1]
    
    clust[n-1] = [val[n-2], L-1]

    return clust


def searching_window(clust: List[int]) -> np.ndarray:
    """
Find searching windows of a cluster as defined in Chang et al. (2002), giving first and
last element of each cluster

Arguments:
clust a list containing the first and last element in a cluster

Value:
searching_window returns an object with class 'np.ndarray'
"""

    # Find length of the initial cluster

    n = clust[1] - clust[0] + 1

    if n == 2:
        w = np.transpose(np.array(clust))
    
    else:
        # Find length of the searching windows

        length = ceil(n / 2)

        total = n - length + 1

        w = np.zeros((int(total), 2), dtype=np.uint8)

        # Find searching windows

        for j in range(0, total):
            w[j] = [clust[0] + j , ceil(clust[0] + j + length - 1)]
    
    return w


def cluster_nth_moment(n: int, prob: List[float], clust: List[int]) -> float:
    """
Compute the nth moment of a given cluster of gray levels.

Arguments:
n order moment
prob the probability vector of gray levels 0,...,L-1
clust a cluster of the gray levels

Value:
cluster_nth_moment returns an object with class float
"""

    clustMean = cluster_mean(prob, clust, 0)

    length = len(clust)

    numerator = list()
    denominator = list()

    # Find nth moment factors

    for i in range(0, length):
        numerator.append( (clust[i] - clustMean) ** n * prob[ clust[i] ] ) 
        denominator.append( prob[ clust[i] ] )
    
    clustMom = sum(numerator) / sum(denominator)

    return clustMom

def skewness(prob: List[float], clust: List[int]) -> float:
    """
Compute the skewness of a given cluster of gray levels.

Arguments:
prob the probability vector of gray levels 0,...,L-1
clust a cluster of the gray levels

Value:
skewness returns an object with class float
"""

    mom2 = cluster_nth_moment(2, prob, clust)
    mom3 = cluster_nth_moment(3, prob, clust)

    skewness = mom3 / sqrt( mom2 ** 3 )

    return skewness


def optimal_window(prob: List[float], w: np.ndarray) -> List[int]:
    """
Find the optimal window of a cluster as defined in Chang et al. (2002).

Arguments:
prob the probability vector of gray levels 0,...,L-1
w bounds of all searching windows of a cluster

Value:
optimal_window returns an object with class list (list of integers)
"""

    # Find the amount of searching windows

    n = np.size(w, 0)

    for i in range(0, n):

        # Do nothing if w is of the type a,a+1
        if w[i][1] - w[i][0] == 1: bounds = w[i].tolist()
        
        else:
            skew = list()

            for i in range(0, n):
                skew.append( skewness(prob, list( range( w[i][0], w[i][1] + 1) ) ) )
            
            skew = [abs(x) for x in skew]

            argminSkew = skew.index(min(skew))

            optimalWindow = w[argminSkew]

            bounds = [optimalWindow[0], optimalWindow[1]]
    
    return bounds


def cluster_estimations(prob: List[float], clust: List[int]) -> List[float]:
    """
Find estimations of mean, variance and proportion of cluster of gray levels.

Arguments:
prob the probability vector of gray levels 0,...,L-1
clust cluster of gray levels

Value:
cluster_estimations returns an object with class tuple containing the following components:
meanEstimation, estimation of the cluster mean 
varianceEstimation, estimation of the cluster variance 
proportionEstimation, estimation of the cluster proportion
"""

    meanEstimation = cluster_mean(prob, clust, 0)
    varianceEstimation = cluster_var(prob, clust, 0)
    proportionEstimation = sum(prob[i] for i in clust)

    clustEstimations = [meanEstimation, varianceEstimation, proportionEstimation]

    return clustEstimations


def weighted_gaussian(x: float, mu: float, sigma: float, p: float) -> float:
    """
Apply gaussian normal function with a given weigth.

Arguments:
x real value in the domain
mu mean parameter
sigma standard deviation parameter
p weight of proportion

Value:
weighted_gaussian returns an object with class float
"""

    #Compute the gaussian value

    gV = p / ( sqrt(2 * pi) * sigma ) * exp( -0.5 * ( (x-mu) / sigma ) ** 2 )

    return gV


def classification_rule(L: int, est: np.ndarray) -> Dict[int, int]:
    """
Classify gray levels accordind to the criterior porposed by Chang et al. (2002).

Arguments:
L amount of gray levels 0,...,L-1
est tuple containing the outputs of cluster_estimations function

Value:
classification_rule returns an object with class dict containing the following components:
grayLevel:classification
"""

    n = np.size(est, 0)

    classification = dict()

    # Assign class

    for i in range(0, L):
        
        value = list()

        for j in range(0, n):
            value.append(weighted_gaussian(i, est[j][0], est[j][1], est[j][2]))
        
        classification[i] = value.index(max(value))
    
    return classification


def bin_value(img: np.ndarray, maxk: int) -> int:
    """
Compute p which gives the 2p+1 optimal number of bins for histogram smoothing.

Arguments:
L amount of gray levels 0,...,L-1
est tuple containing the outputs of cluster_estimations function

Value:
classification_rule returns an object with class dict containing the following components:
grayLevel:classification
"""
    return