import numpy as np
from dataclasses import dataclass
import random

from image_multi_thresholding.base import _between_class_var, _image_probabilities

"""
Find thresholds of the gray levels using shuffled frog-leaping algorithm with between 
class variance as fitness function.
"""


def _is_valid_frog(frog, L):
    return (len(set(frog)) == len(frog) and frog[0] != 0 and frog[-1] != L-1)


@dataclass()
class SFLOptions:
    """Options to be passed to the threshold_sfl function."""
    number_memeplex: int = 4
    """Number of memeplexes."""
    number_frog: int = 10
    """Number of frogs in each memeplex."""
    number_evolution: int = 10
    """Total of replication in memeplex evolution."""


def threshold_sfl(
        img: np.ndarray,
        k: int,
        iter: int = 100,
        options: SFLOptions = SFLOptions()):
    """Find thresholds of the gray levels using shuffled frog-leaping algorithm.

    Uses between class variance as a fitness function. SFLOptions has default recommended
    values for this algorithm, but you can change them by creating a new instance of it
    with your preferred values.

    **Arguments**:

        img: A 2D numpy.ndarray containing the pixel values of the image.
        k: Number of thresholds to find.
        iter: Number of iterations for the algorithm.
        options: If set, overrides the default options for the algorithm.

    **Typical usage example**:

        img = base.load_image('/route/to/image.png')
        options = SFLOptions(
            number_memeplex = 42
        )
        thresholds = threshold_sfl(
            img = img,
            k = 10,
            options = options
        )
    """

    prob = _image_probabilities(img)
    L = len(prob)

    pop_size = options.number_memeplex * options.number_frog
    frogs = np.array([[random.randint(0, L)
                     for _ in range(k)] for _ in range(pop_size)])
    frogs.sort()

    bcv = np.array([_between_class_var(prob, frog)
                   if _is_valid_frog(frog, L) else 0 for frog in frogs])

    best_bcv = max(bcv)
    best_global_frog = frogs[np.argmax(bcv)]

    counter = 0

    while counter < iter:
        sort_indeces = np.flip(np.argsort(bcv))
        sorted_frogs = np.array([frogs[i] for i in sort_indeces])
        all_frogs = []
        for m in range(options.number_memeplex):
            memeplex_frogs = np.array(
                [sorted_frogs[n+m*options.number_frog] for n in range(options.number_frog)], dtype=np.int16)
            evolution = 0
            while evolution < options.number_evolution:
                bcv_memeplex = np.array([_between_class_var(prob, frog)
                                         if _is_valid_frog(frog, L) else 0 for frog in memeplex_frogs])
                best_frog = memeplex_frogs[np.argmax(bcv_memeplex)]
                worst_frog = memeplex_frogs[np.argmin(bcv_memeplex)]
                worst_position = np.argmin(bcv_memeplex)
                new_worst_frog = np.sort(np.array(
                    worst_frog + random.random()*(best_frog - worst_frog), dtype=np.int16))

                if _is_valid_frog(new_worst_frog, L):
                    if _between_class_var(prob, new_worst_frog) > _between_class_var(prob, worst_frog):
                        memeplex_frogs[worst_position] = new_worst_frog
                    else:
                        new_worst_frog = np.sort(
                            worst_frog + random.random()*(best_global_frog - worst_frog)).astype(np.int16)
                        if _is_valid_frog(new_worst_frog, L) and _between_class_var(prob, new_worst_frog) > _between_class_var(prob, worst_frog):
                            memeplex_frogs[worst_position] = new_worst_frog
                        else:
                            memeplex_frogs[worst_position] = np.array(
                                [random.random() * (L-1) for _ in range(k)])
                evolution = evolution + 1

            if len(all_frogs) == 0:
                all_frogs = memeplex_frogs
            else:
                all_frogs = np.concatenate((all_frogs, memeplex_frogs))

        bcv = np.array([_between_class_var(prob, frog)
                        if _is_valid_frog(frog, L) else 0 for frog in all_frogs])
        best_bcv = max(bcv)
        best_global_frog = all_frogs[np.argmax(bcv)]

        counter += 1

    return sorted(best_global_frog.tolist())
