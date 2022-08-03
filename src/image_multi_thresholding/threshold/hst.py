from collections import defaultdict
from image_multi_thresholding.base import _image_probabilities, _discrete_local_min

import math
from typing import List
import numpy as np
import statistics


def _partition_min(prob: List[float], n: int):
    valleys = {}
    valley_values = {}
    section_len = len(prob)//n
    for i, p in enumerate(prob):
        section = i // section_len
        if p <= valley_values.get(section, 1):
            if p < valley_values.get(section, 1):
                valleys[section] = []
            valleys[section].append(i)
            valley_values[section] = p

    res = []
    for val in valleys.values():
        for x in val:
            res.append(x)

    return res


def threshold_hst(img: np.ndarray, k: int, region_count: int = 128):
    prob = _image_probabilities(img)
    val_position = _discrete_local_min(prob)
    regions = _divisors(len(prob))[1:-1]

    min_region = [_partition_min(prob, region)
                  for region in regions]  # Minimos por región
    # Quedarse con los que tienen al menos k-1
    int_regions = list(filter(lambda x: len(x) >= k, min_region))

    intersection = [[x for x in region if x in val_position]
                    for region in int_regions]

    thresholds = defaultdict(list)
    for th in range(k):
        for i, clust in enumerate(intersection):
            size = math.ceil(len(clust)/k)
            cluster_section = clust[size*th:size*(th+1)]
            if not cluster_section:
                continue
            thresholds[regions[i]].append(math.ceil(
                statistics.mean(cluster_section)))

    return thresholds[region_count]


def _divisors(n):
    divs = [1]
    for i in range(2, int(math.sqrt(n))+1):
        if n % i == 0:
            divs.append(i)
            if n / i not in divs:
                divs.append(n // i)
    divs.append(n)
    return sorted(divs)
