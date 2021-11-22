import numpy as np
from dataclasses import dataclass
import random

from src.image_multi_thresholding.base import _between_class_var, _image_probabilities


def _is_valid_frog(frog, L):
    return (len(set(frog)) == len(frog) and frog[0] != 0 and frog[-1] != L-1)


@dataclass()
class SFLOptions:
    number_memeplex: int = 4
    number_frog: int = 10
    number_evolution: int = 10


def threshold_sfl(img: np.ndarray, k: int, iter: int = 100, options: SFLOptions = SFLOptions()):
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
        # print(f'{sort_indeces=}')
        sorted_frogs = np.array([frogs[i] for i in sort_indeces])
        # print(f'{sorted_frogs=}')
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
                        # print(f'{new_worst_frog=}')
                        if _is_valid_frog(new_worst_frog, L) and _between_class_var(prob, new_worst_frog) > _between_class_var(prob, worst_frog):
                            memeplex_frogs[worst_position] = new_worst_frog
                        else:
                            memeplex_frogs[worst_position] = np.array(
                                [random.random() * (L-1) for _ in range(k)])
                evolution = evolution + 1

            if len(all_frogs) == 0:
                all_frogs = memeplex_frogs
            else:
                # print(f'{all_frogs=}')
                # print(f'{memeplex_frogs=}')
                all_frogs = np.concatenate((all_frogs, memeplex_frogs))

        bcv = np.array([_between_class_var(prob, frog)
                        if _is_valid_frog(frog, L) else 0 for frog in all_frogs])
        best_bcv = max(bcv)
        best_global_frog = all_frogs[np.argmax(bcv)]

        counter += 1

    return best_global_frog
