from dataclasses import dataclass
import random
import numpy as np

from image_multi_thresholding.base import _between_class_var, _image_probabilities


def _is_valid_bee(bee, L):
    return (len(set(bee)) == len(bee) and bee[0] != 0 and bee[-1] != L-1)


@dataclass()
class ABCOptions:
    internal_iteration_limit: int = 100
    population_count: int = 30


def threshold_abc(img: np.ndarray, k: int, iter: int = 100, options: ABCOptions = ABCOptions()):
    prob = _image_probabilities(img)
    L = len(prob)

    bees = np.array([[random.randint(1, L-1)
                    for _ in range(k)] for _ in range(options.population_count)])
    bcv = np.array([_between_class_var(prob, bee)
                    if _is_valid_bee(bee, L) else 0 for bee in bees])
    for x in range(iter):
        employed_bees = bees.copy()
        new_bees = bees.copy()
        # 1
        for i in range(options.population_count):
            # 1.a
            h = random.randint(0, k-1)
            l = random.choice(
                [j for j in range(options.population_count) if j != i])
            phi = random.random() * 2 - 1  # In range [-1, 1]
            employed_bees[i][h] += phi*(bees[i][h] - bees[l][h])

        # 1.b
        new_bcv = np.array([_between_class_var(prob, bee)
                            if _is_valid_bee(bee, L) else 0 for bee in employed_bees])
        new_bees = np.array([employed_bees[i] if new_bcv[i] > bcv[i] else bees[i]
                             for i in range(options.population_count)])
        bcv = np.array([new_bcv[i] if new_bcv[i] > bcv[i] else bcv[i]
                        for i in range(options.population_count)])
        solution_likelihood = np.array(
            [bee_bcv / sum(bcv) for bee_bcv in bcv])
        best_employed_bee_index = np.argmax(solution_likelihood)
        r = random.random()
        for j, s in enumerate(solution_likelihood):
            if r < s:
                new_bees[j] = employed_bees[best_employed_bee_index]
                break

        # 2
        min_bee = np.ndarray(shape=(k,), dtype=int)
        max_bee = np.ndarray(shape=(k,), dtype=int)
        for i, col in enumerate(bees.transpose()):
            min_cell = np.argmin(col)
            max_cell = np.argmax(col)
            min_bee[i] = col[min_cell]
            max_bee[i] = col[max_cell]

        for i, bee in enumerate(bees):
            if np.equal(bee, new_bees[i]).all():
                s = random.random()
                new_bees[i] = min_bee + s*(max_bee - min_bee)
        bees = new_bees

    return bees[np.argmax(bcv)]
