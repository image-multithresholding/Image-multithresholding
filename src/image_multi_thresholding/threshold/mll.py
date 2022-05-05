import math
import numpy as np
from typing import List
from image_multi_thresholding.base import _cluster_var, _gray_clustering, _prob_up_to_level, _threshold_candidate_generic

def _dual_max_likelihood(prob, levels):
    w = _prob_up_to_level(prob, levels)
    L = len(prob)
    clust = _gray_clustering(L, levels, 1)

    var = [_cluster_var(prob, c, 1) for c in clust]
    return sum([wi * math.log(var[i]) if var[i] != 0 else 100 for i, wi in enumerate(w)])

def threshold_mll(img: np.ndarray, k:int) -> List:
    return _threshold_candidate_generic(img, k, _dual_max_likelihood, is_max=False)
