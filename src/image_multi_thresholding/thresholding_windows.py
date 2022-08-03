import numpy as np
from typing import List, Dict
from math import ceil


def _valleys(hist: Dict[int, int]) -> List[int]:
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

    both = list(i and j for i, j in zip(toRight, toLeft))

    val = list(i for i, x in enumerate(both) if x == True)

    return val


def _valley_clustering(L: int, val: List[int]) -> np.ndarray:
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


def _searching_window(clust: List[int]) -> np.ndarray:
    # Find length of the initial cluster
    n = clust[1] - clust[0] + 1

    if n == 2:
        w = np.zeros((1, 2), dtype=np.uint16)
        w[0] = np.transpose(np.array(clust))

    else:
        # Find length of the searching windows

        length = ceil(n / 2)

        total = n - length + 1

        w = np.zeros((int(total), 2), dtype=np.uint16)

        # Find searching windows

        for j in range(total):
            w[j] = [clust[0] + j, ceil(clust[0] + j + length - 1)]

    return w
