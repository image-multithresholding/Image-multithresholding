from skimage import exposure
import numpy as np
import itertools
import pandas as pd
from statsmodels.formula.api import ols
from typing import List, Dict, Tuple, Callable
from PythonCodes.library.thresholding_base import *
from PythonCodes.library.thresholding_levels import *
from PythonCodes.library.hca import *


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
    totalMean = cluster_mean(prob, newCluster, 0)  # TODO: Consultar esto

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

    amountOfProbabilities = len(prob)
    levels = [0] + levels + [amountOfProbabilities]
    amountOfLevels = len(levels)

    # Initialize correlations list
    correlations = list()

    # Find the correlation of each interval
    for i in range(1, amountOfLevels):
        correlations.append(-np.log(
            sum(np.square(prob[(levels[i - 1]): (levels[i])])) / probUpToLevel[i - 1] ** 2))

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

    amountOfProbabilities = len(prob)
    levels = [0] + levels + [amountOfProbabilities]
    amountOfLevels = len(levels)

    prob = np.array(prob)

    # Initialize entropies list

    entropies = list()

    # Find the entropy of each interval
    for i in range(1, amountOfLevels):
        entropies.append(-np.sum(np.multiply(prob[(levels[i - 1]): (levels[i])], np.log(
            prob[(levels[i - 1]): (levels[i])] / probUpToLevel[i - 1]))) / probUpToLevel[i - 1])

    # Find the total entropy
    totalEntropy = sum(entropies)

    return totalEntropy


def mom1_up_to_level(prob: List[float], level: int) -> float:
    term = list()

    # Find the first-order moment of each element
    for i, p in enumerate(prob[:level + 1]):
        term.append(i*p)

    return sum(term)


def threshold_candidates(gray_levels: List[int], k: int) -> itertools.combinations:
    """Returns a generator of all combinations of k elements from gray_levels, never taking the first or last element"""
    return itertools.combinations([x for x in range(1, len(gray_levels) - 1)], k)


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


def threshold_fom(img: np.ndarray, k: int):
    prob = image_probabilities(img)
    L = len(prob)

    P = {(i, j): prob_up_to_level(prob, [j])[0] - prob_up_to_level(prob, [i - 1])[0]
         for i in range(L) for j in range(i, L)}

    S = {(i, j): mom1_up_to_level(prob, j) - mom1_up_to_level(prob, i - 1)
         for i in range(L) for j in range(i, L)}

    H = {(i, j): S[(i, j)]**2 / P[(i, j)]
         for i in range(L) for j in range(i, L)}
    maxModifiedBCVar = 0
    optCandidate = None
    for candidate in threshold_candidates([x for x in range(L)], k):
        n = len(candidate)

        if k == 1:
            for i, _ in enumerate(candidate):
                h = 0
                h += H[(0, candidate[i])]
                h += H[(candidate[i] + 1, L - 1)]
                if h > maxModifiedBCVar:
                    maxModifiedBCVar = h
                    optCandidate = candidate

        else:
            for i, _ in enumerate(candidate):
                h = 0
                h += H[(0, candidate[0])]
                for j in range(k - 1):
                    h += H[(candidate[j] + 1, candidate[j+1])]
                h += H[(candidate[k-1] + 1, L - 1)]
                if h > maxModifiedBCVar:
                    maxModifiedBCVar = h
                    optCandidate = candidate

    return optCandidate


def threshold_hca(img: np.ndarray, k: int):
    freq = image_histogram(img)
    L = len(freq)
    cellSize = hill_identification(freq, k-1)[1]
    cells = uncell(L, cellSize)
    valleyLocation = valley_location(freq, cellSize)

    threshold = list()
    for valLoc in valleyLocation:
        startLeft = cells[valLoc[0]][0]
        endRight = cells[valLoc[1]][-1]
        threshold.append(round(startLeft + (endRight - startLeft)/2))

    return threshold


def threshold_lra(img: np.ndarray, k: int, n: int, m: int):
    def arg_min_rf22(x, y):
        #x = np.arange(10)+1
        #y = [4, 5, 20, 14, 32, 22, 38, 43, 1, 1]

        d = {'x': x, 'x2': x*x, 'xy': x*y, 'x2y': x*x*y}
        df = pd.DataFrame(data=d)

        model = ols("y ~ x + x2 + xy + x2y", data=df)
        fit = model.fit()
        model.df_model
