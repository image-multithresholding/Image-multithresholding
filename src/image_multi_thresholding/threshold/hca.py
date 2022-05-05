import numpy as np
import math
from typing import List, Tuple
from image_multi_thresholding.base import _arrow_direction, _peak_identification, _hill_identification, _cell_histogram, _image_histogram, _uncell


def _valley_location(freq: List[float], size: int) -> List[float]:
    cells = _cell_histogram(freq, size)
    direction = _arrow_direction(cells)

    limits = list()

    # Get all positions where direction is 1, avoiding first and last position
    ones = [i for i, x in enumerate(direction[:-1]) if x == 1]

    for i in ones:
        nextDirections = [dir for dir in direction[i+1:]]
        if nextDirections:
            aux = []
            if 1 in nextDirections:
                aux.append(nextDirections.index(1))
            if -1 in nextDirections:
                aux.append(nextDirections.index(-1))
            h = min(aux)
            if direction[h + i + 1] == -1:
                limits.append((i, h + i + 1))

    return limits


def threshold_hca(img: np.ndarray, k: int):
    freq = _image_histogram(img)
    L = len(freq)
    cellSize = _hill_identification(freq, k-1)[1]
    cells = _uncell(L, cellSize)
    valleyLocation = _valley_location(freq, cellSize)

    threshold = list()
    for valLoc in valleyLocation:
        startLeft = cells[valLoc[0]][0]
        endRight = cells[valLoc[1]][-1]
        threshold.append(round(startLeft + (endRight - startLeft)/2))

    return threshold
