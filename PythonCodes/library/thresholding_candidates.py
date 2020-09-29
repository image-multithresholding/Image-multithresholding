from skimage import exposure
import numpy as np
import itertools
from typing import List, Dict, Tuple, Callable
from PythonCodes.library.thresholding_base import *
from PythonCodes.library.thresholding_levels import *

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

def threshold_candidates(gray_levels: List[int], k: int) -> itertools.combinations:
    """Returns a generator of all combinations of k elements from gray_levels, never taking the first or last element"""
    return itertools.combinations([x for x in range(len(gray_levels))], k)

def threshold_candidate_generic(img: np.ndarray, k: int, candidate_function: Callable[[List[float], List[int]], float]):
    prob = image_probabilities(img)

    max_generic = 0
    max_generic_candidate = 0

    # Get the candidate with the highest total value for the candidate_function
    for candidate in threshold_candidates(prob, k):
        value = candidate_function(prob, candidate)
        if value > max_generic:
            max_generic = value
            max_generic_candidate = candidate
    
    return max_generic_candidate

def threshold_mcc(img: np.ndarray, k: int):
    return threshold_candidate_generic(img, k, total_correlation)

def threshold_mec(img: np.ndarray, k: int):
    return threshold_candidate_generic(img, k, total_entropy)

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

    newClust = list(i for i in range(0, amountOfProbabilities))
    thr = list([])

    for i in range(0, k):

        # Find the new threshold using maximum total correlation criterion
        
        thr.append(newClust[argmax_TC(prob[newClust[0] : newClust[-1] + 1] / sum(prob[newClust[0] : newClust[-1] + 1]))])

        thr.sort()

        # Find the classes according to the thresholds

        clust = gray_clustering(amountOfProbabilities, thr)

        # Find the variance per class

        varClust = list()
        
        for j in range(0, i+2):

            varClust.append(cluster_var(prob, clust[j], 0))

        # Find the argmax of cluster variances
        argMaxVar = varClust.index(max(varClust))

        # Define the new lass to be partitioned

        newClust = clust[argMaxVar]

    return thr