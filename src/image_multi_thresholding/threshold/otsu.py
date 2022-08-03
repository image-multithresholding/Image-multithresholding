import numpy as np
from image_multi_thresholding.base import _between_class_var, _threshold_candidate_generic, _prob_up_to_level, _threshold_candidates, _image_probabilities, _cluster_mean, _gray_clustering
from typing import List


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

    H = {(i, j): S[(i, j)]**2 / P[(i, j)] if P[(i, j)] != 0 else 0
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
