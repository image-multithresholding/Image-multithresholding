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

def between_class_var(prob: List[float], levels: List[int]) -> float:
    """Compute the variance between classes"""
    probUpToLevel = prob_up_to_level(prob, levels)
    amountOfProbs = len(prob)
    cluster = gray_clustering(amountOfProbs, levels)

    newCluster = list()
    mu = list()

    for value in cluster:
        newCluster.append(value)
        mu.append(cluster_mean(prob, newCluster, 0))
    totalMean = cluster_mean(prob, newCluster, 0) # TODO: Consultar esto

    term = list()
    for i, m in enumerate(mu):
        term.append(probUpToLevel[i] * m-totalMean**2)

    return sum(term)

def total_correlation(prob: List[float], levels: List[int]) -> float:
    """Compute the total correlation according to a list of breaking gray levels
    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    levels a list of gray levels which give the breaks 
    Value:
    total_correlation returns an object with class 'float'"""
    
    probUpToLevel = prob_up_to_level(prob, levels)
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

    return sum(correlations)

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

# TODO: Ask about this. This interface doesn't match the others (levels List vs level int)
def mom1_up_to_level(prob: List[float], levels: List[int]) -> float:
    term = list()

    # Find the first-order moment of each element
    for i, p in enumerate(prob):
        term.append((i-1)*p)

    return sum(term)

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

def threshold_otsu(img: np.ndarray, k: int):
    return threshold_candidate_generic(img, k, between_class_var)
