import numpy as np
from typing import List
from image_multi_thresholding.base import _image_probabilities, _gray_clustering, _total_correlation, _cluster_var


def _argmax_TC(prob: List[float]) -> List[int]:
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

    for i in range(1, amountOfProbabilities - 1):
        totalCorrelations.append(_total_correlation(prob, [i]))

    # Find the maximum total correlation

    maxTotalCorrelation = max(totalCorrelations)

    # Find the level at which the maximum total entropy is reached

    argmax = totalCorrelations.index(maxTotalCorrelation)

    return argmax


def threshold_atc(img: np.ndarray, k: int) -> List[int]:
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
        raise Exception(
            "The number of thresholds must be greater than or equal to 1")

    image = np.copy(img)

    # Find the vector of probabilities of the gray leves 0,1,...,L-1

    prob = _image_probabilities(image)

    amountOfProbabilities = len(prob)

    newClust = list(i for i in range(0, amountOfProbabilities))
    thr = list([])

    for i in range(0, k):

        # Find the new threshold using maximum total correlation criterion

        thr.append(newClust[_argmax_TC(
            prob[newClust[0]: newClust[-1] + 1] / sum(prob[newClust[0]: newClust[-1] + 1]))])

        thr.sort()

        # Find the classes according to the thresholds

        clust = _gray_clustering(amountOfProbabilities, thr)

        # Find the variance per class

        varClust = list()

        for j in range(0, i+2):
            varClust.append(_cluster_var(prob, clust[j], 0))

        # Find the argmax of cluster variances
        argMaxVar = varClust.index(max(varClust))

        # Define the new lass to be partitioned

        newClust = clust[argMaxVar]

    return thr
