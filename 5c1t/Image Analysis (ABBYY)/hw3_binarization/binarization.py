import numpy as np
from os.path import join, basename

from image import read_image, save_image, sliding_window_centered, ImageStatsCalculator
from paths import list_files, PATH_DATA, PATH_RESULTS


class Binarizer:
    def binarize(self, image):
        raise NotImplementedError()


class SauvolaBinarizer(Binarizer):
    def __init__(self, k=0.5, r=128, window_size=(64, 64)):
        self._k = k
        self._r = r
        self._window_size = window_size

    def binarize(self, image):
        stats = ImageStatsCalculator(image)
        result = np.zeros(image.shape, dtype=np.uint8)
        for row, col, window, window_bounds in sliding_window_centered(image, *self._window_size):
            mean = stats.calc_mean(*window_bounds)
            std = stats.calc_std(*window_bounds)
            threshold = mean * (1 + self._k * (std / self._r - 1))
            result[row, col] = image[row, col] >= threshold
        return result * 255


if __name__ == '__main__':
    import time

    start = time.time()
    for path_input in list_files(PATH_DATA):
        print(basename(path_input))
        image = read_image(path_input)
        image_binarized = SauvolaBinarizer().binarize(image)

        path_result = join(PATH_RESULTS, 'sauvola', basename(path_input))
        save_image(image_binarized, path_result)
    print(time.time() - start)
