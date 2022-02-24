import numpy as np
from PythonCodes.library.base import _threshold_candidate_generic, _total_correlation


def threshold_mcc(img: np.ndarray, k: int):
    return _threshold_candidate_generic(img, k, _total_correlation)
