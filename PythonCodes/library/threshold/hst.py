from PythonCodes.library.base import _image_probabilities, _discrete_local_min

import math
from typing import List
import numpy as np


def _partition_min(prob: List[float], n: int):
    min_prob = 0
    min_region = 0

    for i, p in enumerate(prob):
        if p < min_prob:
            min_prob = p
            min_region = i // n

    return min_region


def threshold_hst(img: np.ndarray, k: int):
    prob = _image_probabilities(img)
    val_position = _discrete_local_min(prob)
    regions = _divisors(len(prob))[1:-1]

    # TODO: Debería ser una lista pero es un int?
    min_region = [_partition_min(prob, region) for region in regions]
    int_regions = filter(lambda x: len(x) >= k-1, min_region)

    intersection = [[x for x in region if x in val_position]
                    for region in int_regions]

    thresholds = []
    number_region = []
    for i, clust in enumerate(intersection):
        thresholds.append([])  # TODO: Preguntar por promedio?
        number_region.append(regions[i+len(regions)-len(int_regions)])

    # TODO: ???


def _divisors(n):
    divs = [1]
    for i in range(2, int(math.sqrt(n))+1):
        if n % i == 0:
            divs.append(i)
            if n / i not in divs:
                divs.append(n / i)
    divs.append(n)
    return divs
