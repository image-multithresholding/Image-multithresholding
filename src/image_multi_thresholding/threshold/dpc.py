from image_multi_thresholding.base import _weighted_gaussian, _image_histogram, _optimal_window, _cluster_estimations, _discrete_local_min
from image_multi_thresholding.thresholding_windows import _valleys, _valley_clustering, _searching_window
from typing import List
import numpy as np
from math import pi, cos
from collections import defaultdict


def _classification_rule(L, estimations) -> List[int]:
    n = len(estimations)
    cluster = []
    for i in range(1, L+1):
        maxValue = -1
        maxValueIndex = -1
        for j in range(n):
            wg = _weighted_gaussian(
                i, estimations[j][0], estimations[j][1], estimations[j][2])
            if wg > maxValue:
                maxValue = wg
                maxValueIndex = j
        cluster.append(maxValueIndex)
    return cluster


def _smoothed_histogram(img: np.ndarray, p: int) -> List[int]:
    hist = _image_histogram(img)
    L = len(hist)
    size = 2*p + 1
    b = [0.5*(1-cos(i*pi/p)) for i in range(1, size+1)]

    smooth = []

    def sum_func(i, left, right):
        s = 0
        for j in range(left, right):
            s += b[j+p-1]*hist[i+j-1]
        return s

    for i in range(p):
        smooth.append(sum_func(i, 1-i, p+2))
    for i in range(p, L-p):
        smooth.append(sum_func(i, -p+1, p+2))
    for i in range(L-p, L):
        smooth.append(sum_func(i, -p+1, L-i+1))

    return [round(s/size) for s in smooth]


def _bin_value(img: np.ndarray, mink: int) -> int:
    k = []
    for p in range(1, 101):
        smooth = _smoothed_histogram(img, p)
        val = _valleys(smooth)
        k.append(len(val) + 1)

    nearest = abs(k[0] - mink)
    nearestIndex = 0
    for i, curr in enumerate(k):
        diff = abs(curr-mink)
        if diff < nearest:
            nearest = diff
            nearestIndex = i

    return nearestIndex + 1


def threshold_dpc(img: np.ndarray, mink: int) -> List[int]:
    # Add 1 to k to set the maximum amount of classes (to be consistent)
    mink += 1
    p = _bin_value(img, mink)
    hist = _smoothed_histogram(img, p)

    hist_sum = sum(hist)
    prob = [h / hist_sum for h in hist]

    val = _valleys(hist)
    clust = [[x + 1 for x in c] for c in _valley_clustering(len(hist), val)]

    opt_win = [_optimal_window(prob, _searching_window(c)) for c in clust]

    estimations = [_cluster_estimations(
        prob, list(range(w[0], w[1]+1))) for w in opt_win]

    classes = _classification_rule(len(hist), estimations)

    byClass = defaultdict(int)

    for i, x in enumerate(classes):
        byClass[x] = i

    return sorted(list(byClass.values()))[:-1]
