from PIL.Image import Image, new
import numpy as np

def get_thresholded_image(image: np.ndarray, thresholds: list, colors: list) -> Image:
    if len(thresholds) > len(colors) + 1:
        raise Exception("number of colors doesn't match number of classes")
    img = new("RGB", (image.shape[1], image.shape[0]))
    for x in range(image.shape[1]):
        for y in range(image.shape[0]):
            pixel = image[y][x][0]
            for i, thr in enumerate(thresholds):
                if pixel < thr:
                    img.putpixel((x, y), colors[i])
                    break
            else:
                img.putpixel((x, y), colors[len(thresholds)])
    return img
