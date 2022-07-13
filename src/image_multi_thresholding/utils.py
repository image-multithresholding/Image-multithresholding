from PIL.Image import Image, new
import numpy as np

COLORS = [
    (93, 46, 70),
    (173, 106, 108),
    (208, 173, 167),
    (232, 214, 203)
]

def get_thresholded_image(image: np.ndarray, thresholds: list) -> Image:
    img = new("RGB", (image.shape[1], image.shape[0]))
    for x in range(image.shape[1]):
        for y in range(image.shape[0]):
            pixel = image[y][x][0]
            for i, thr in enumerate(thresholds):
                if pixel < thr:
                    img.putpixel((x, y), COLORS[i])
                    break
            else:
                img.putpixel((x, y), COLORS[len(thresholds)])
    return img
