import math
from typing import List, Tuple
from PythonCodes.library.thresholding_analysis import arrow_direction, peak_identification

def cell_histogram(freq: List[float], size: int) -> List[float]:
    n = math.ceil(len(freq)/size)
    cuts = [freq for _ in range(size)]

    return [sum(cuts[index:]) for (index, _) in enumerate(cuts[:n])]

def hill_identification(freq: List[float], k: int) -> Tuple[int, int]:
    size = 0
    peaks = []
    while len(peaks) <= k:
        size += 1
        cells = cell_histogram(freq, size)
        direction = arrow_direction(cells)
        peaks = peak_identification(direction)

    return peaks, size

def valley_location(freq: List[float], size: int) -> List[float]:
    pass