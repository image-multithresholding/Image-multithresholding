import numpy as np
from PythonCodes.library.base import _threshold_candidate_generic, _prob_up_to_level, _threshold_candidates, _image_probabilities, _cluster_mean, _gray_clustering
from typing import List


def _between_class_var(prob: List[float], levels: List[int]) -> float:
    """Compute the variance between classes"""
    probUpToLevel = _prob_up_to_level(prob, levels)
    amountOfProbs = len(prob)
    cluster = _gray_clustering(amountOfProbs, levels)

    newCluster = list()
    mu = list()

    for value in cluster:
        newCluster.append(value)
        mu.append(_cluster_mean(prob, newCluster, 0))
    totalMean = _cluster_mean(prob, newCluster, 0)  # TODO: Consultar esto

    term = list()
    for i, m in enumerate(mu):
        term.append(probUpToLevel[i] * m-totalMean**2)

    return sum(term)


def _mom1_up_to_level(prob: List[float], level: int) -> float:
    term = list()

    # Find the first-order moment of each element
    for i, p in enumerate(prob[:level + 1]):
        term.append(i*p)

    return sum(term)


def threshold_otsu(img: np.ndarray, k: int):
    return _threshold_candidate_generic(img, k, _between_class_var)


def threshold_fom(img: np.ndarray, k: int):
    prob = _image_probabilities(img)
    L = len(prob)

    P = {(i, j): _prob_up_to_level(prob, [j])[0] - _prob_up_to_level(prob, [i - 1])[0]
         for i in range(L) for j in range(i, L)}

    S = {(i, j): _mom1_up_to_level(prob, j) - _mom1_up_to_level(prob, i - 1)
         for i in range(L) for j in range(i, L)}

    H = {(i, j): S[(i, j)]**2 / P[(i, j)]
         for i in range(L) for j in range(i, L)}
    maxModifiedBCVar = 0
    optCandidate = None
    for candidate in _threshold_candidates([x for x in range(L)], k):

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
