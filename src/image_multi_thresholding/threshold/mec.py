import numpy as np
from typing import List
from image_multi_thresholding.base import _prob_up_to_level, _threshold_candidate_generic


def _total_entropy(prob: List[float], levels: List[int]) -> float:
    """
    Compute the total entropy according to a list of breaking gray levels

    Arguments:
    prob the probability list of gray levels (list of elements of class float, value from 0 to 1)
    levels a list of gray levels which give the breaks

    Value:
    total_correlation returns an object with class 'float'
    """

    # Find the probabilities according to the given levels

    probUpToLevel = _prob_up_to_level(prob, levels)

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


def threshold_mec(img: np.ndarray, k: int):
    return _threshold_candidate_generic(img, k, _total_entropy)
