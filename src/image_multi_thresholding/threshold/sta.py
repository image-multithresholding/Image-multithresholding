from typing import Callable, List, Tuple
import random
import numpy as np
from functools import partial
from image_multi_thresholding.base import _between_class_var, _image_probabilities


def _expansion(x: np.array, expansionFactor: float) -> np.array:
    n = len(x)
    mat = np.diag(np.random.normal(0, 1, n))
    return x + np.matmul(expansionFactor * mat, x)


def _rotation(x: np.array, rotationFactor: float):
    n = len(x)
    mat = np.matrix([[random.random()*2-1 for _ in range(n)]
                    for _ in range(n)])
    return (x + np.matmul(rotationFactor/(n*np.linalg.norm(x))*mat, x)).A1


def _axesion(x: np.array, axesionFactor: float):
    n = len(x)
    mat = np.zeros((n, n))
    pos = random.randint(0, n-1)
    value = np.random.normal(0, 1, 1)

    mat[pos][pos] = value[0]

    return x + np.matmul(axesionFactor * mat, x)


def _translation(x1: np.array, translationFactor: float, x0: np.array = None):
    n = len(x1)
    mat = random.random()
    return x1 + translationFactor * mat * (x1-x0)/np.linalg.norm(x1-x0)


def _calculate_fitness(prob: List[float], state: np.array, k: int) -> float:
    if len(np.unique(state)) == k and state[0] != 0 and state[-1] != len(prob)-1:
        return _between_class_var(prob, state)
    return 0


def _transform(prob: List[float], k: int, transform_fun: Callable, state: np.array, factor: float, min: int, max: int):
    new_state = np.round(transform_fun(state, factor)).astype(int)
    new_state = np.sort(new_state)
    if (new_state[0] < min) or (new_state[-1] > max):
        return state, False
    else:
        state_fit = _calculate_fitness(prob, state, k)
        new_state_fit = _calculate_fitness(prob, new_state, k)
        if new_state_fit > state_fit:
            return new_state, True
        return state, False


def _transform_and_translate(prob: List[float], k: int, transform_fun: Callable, state: np.array, fun_factor: float, translation_factor: float, min: int, max: int):
    new_solution, has_better_fit = _transform(
        prob, k, transform_fun, state, fun_factor, min, max)
    if has_better_fit:
        return _transform(prob, k, partial(_translation, x0=state), new_solution, translation_factor, min, max)[0]
    return new_solution


def threshold_sta(img: np.ndarray, k: int, iter: int = 10, max_rotation_factor: float = 1,
                  min_rotation_factor: float = 0.0001, translation_factor: float = 1, expansion_factor: float = 1,
                  axesion_factor: float = 1, lessening_coef: float = 2, search_enforcement: int = 30) -> np.array:

    prob = _image_probabilities(img)
    L = len(prob)
    min_thr = 0
    max_thr = L-1
    solutions = np.zeros((search_enforcement, k), int).astype(int)

    for i in range(search_enforcement):
        for j in range(k):
            #solutions[i][j] = random.randint(min_thr, max_thr)
            solutions[i][j] = round(
                min_thr + random.random()*(max_thr-min_thr))

    solutions = np.sort(solutions)
    counter = 0
    rotation_factor = random.random()

    while counter <= iter:
        if rotation_factor < min_rotation_factor:
            rotation_factor = max_rotation_factor
        for i in range(search_enforcement):
            solutions[i] = _transform_and_translate(
                prob, k, _expansion, solutions[i], expansion_factor, translation_factor, min_thr, max_thr)
        for i in range(search_enforcement):
            solutions[i] = _transform_and_translate(
                prob, k, _rotation, solutions[i], rotation_factor, translation_factor, min_thr, max_thr)
        for i in range(search_enforcement):
            solutions[i] = _transform_and_translate(
                prob, k, _axesion, solutions[i], axesion_factor, translation_factor, min_thr, max_thr)

        rotation_factor = rotation_factor / lessening_coef
        counter += 1

    fitness = [_between_class_var(prob, solution) for solution in solutions]
    return solutions[np.argmax(fitness)]
