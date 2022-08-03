import numpy as np
import random
import math
from dataclasses import dataclass
from image_multi_thresholding.base import _between_class_var, _image_probabilities


@dataclass()
class PSOOptions:
    number_part: int = 40
    inertia_initial: float = 0.9
    inertia_final: float = 0.4
    acceleration_1_initial: float = 2.5
    acceleration_1_final: float = 0.5
    acceleration_2_initial: float = 0.5
    acceleration_2_final: float = 2.5


def is_valid_position(vector: np.ndarray, k: int, L: int):
    return len(set(vector)) == k and vector[0] != 0 and vector[-1] != L-1


def threshold_pso(img: np.ndarray, k: int, options: PSOOptions = PSOOptions(), iterations: int = 10):
    prob = _image_probabilities(img)
    L = len(prob)

    particle_position = np.array(
        [[random.randint(0, L-1) for _ in range(k)] for _ in range(options.number_part)])
    particle_position = np.sort(particle_position)

    particle_fitness = [0 if not is_valid_position(
        v, k, L) else _between_class_var(prob, v) for v in particle_position]
    particle_best_position = particle_position
    particle_best_fitness = particle_fitness
    global_best_fitness = max(particle_fitness)
    for i, f in enumerate(particle_fitness):
        if f == global_best_fitness:
            best_index = i
            break
    global_best_position = particle_position[best_index]

    counter = 0
    particle_velocity = np.array([[0 for _ in range(k)]
                                 for _ in range(options.number_part)])

    while counter < iterations:
        inertia = (options.inertia_initial - options.inertia_final) * \
            (iterations-counter-1)/iterations + options.inertia_final
        acceleration_1 = options.acceleration_1_initial + \
            (options.acceleration_1_final -
             options.acceleration_1_initial) * (counter+1)/iterations
        acceleration_2 = options.acceleration_2_initial + \
            (options.acceleration_2_final -
             options.acceleration_2_initial) * (counter+1)/iterations

        for i in range(options.number_part):
            r1 = random.random()
            r2 = random.random()
            particle_velocity[i] = inertia * particle_velocity[i] + \
                acceleration_1 * r1 * (particle_best_position[i] - particle_position[i]) + \
                acceleration_2 * r2 * \
                (global_best_position - particle_position[i])

        new_particle_position = np.round(particle_position + particle_velocity)
        new_particle_position = np.sort(new_particle_position)

        for i in range(options.number_part):
            for i, new_part in enumerate(new_particle_position):
                if new_part[0] <= 0 or new_part[-1] >= L-1:
                    new_particle_position[i] = particle_position[i]

        particle_position = new_particle_position
        particle_fitness = [0 if not is_valid_position(
            v, k, L) else _between_class_var(prob, v) for v in particle_position]
        for i in range(options.number_part):
            if (particle_fitness[i] > particle_best_fitness[i]):
                particle_best_position[i] = particle_position[i]
                particle_best_fitness[i] = particle_fitness[i]

            if particle_best_fitness[i] > global_best_fitness:
                global_best_fitness = particle_best_fitness[i]
                global_best_position = particle_best_position[i]

        counter += 1

    return global_best_position
