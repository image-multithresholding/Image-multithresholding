import numpy as np
from image_multi_thresholding.base import _threshold_candidate_generic, _total_correlation


def threshold_mcc(img: np.ndarray, k: int):
    return _threshold_candidate_generic(img, k, _total_correlation)
