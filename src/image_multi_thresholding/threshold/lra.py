import numpy as np
import pandas as pd
from image_multi_thresholding.base import _image_histogram, _hill_identification, _uncell
from statsmodels.formula.api import ols


def _arg_min_rf(x, y, n, m):
    x = np.arange(x[0], x[-1]+1)
    d = {**{'x' + str(i): x**i for i in range(1, n+1)}, **
         {'yx' + str(i): y*x**i for i in range(1, m+1)}}
    df = pd.DataFrame(data=d)

    x_string = ' + '.join(['x' + str(i) for i in range(1, n+1)])
    xy_string = ' + '.join(['yx' + str(i) for i in range(1, m+1)])
    model = ols('y ~ ' + x_string + ' + ' + xy_string, data=df)
    result = model.fit()
    coef = result.params

    def rational(z): return (coef['Intercept'] + sum([coef['x' + str(i)]*z**i for i in range(
        1, n+1)])) / (1 - sum([coef['yx' + str(i)]*z**i for i in range(1, m+1)]))

    return min([(i, rational(i)) for i in x], key=lambda x: x[1])[0]


def threshold_lra(img: np.ndarray, k: int, n: int, m: int):
    freq = _image_histogram(img)
    peakLocations, cellSize = _hill_identification(freq, k)
    cells = _uncell(len(freq), cellSize)

    thresholds = []
    if len(peakLocations) == 1:
        valley = cells[peakLocations[0]]
        thresholds.append(_arg_min_rf(
            valley, freq[valley[0]:valley[-1]], n, m))
    else:
        prevPeak = peakLocations[0]
        for i, _ in enumerate(peakLocations[:-1]):
            valley = []
            for j in range(prevPeak, peakLocations[i+1]+1):
                valley = [*valley, *cells[j]]
            prevPeak = peakLocations[i+1]+1
            thresholds.append(_arg_min_rf(
                valley, freq[valley[0]-1:valley[-1]], n, m))

    return thresholds
