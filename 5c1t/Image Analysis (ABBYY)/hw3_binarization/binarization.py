import numpy as np

from image import sliding_window, sliding_window_centered, ImageStatsCalculator


class Binarizer:
    def binarize(self, image):
        raise NotImplementedError()


class SauvolaBlockBinarizer(Binarizer):
    def __init__(self, k=0.5, r=128, window_size=(64, 64)):
        self._k = k
        self._r = r
        self._window_size = window_size

    def binarize(self, image):
        stats = ImageStatsCalculator(image)
        result = np.zeros(image.shape, dtype=np.uint8)
        for row, col, window in sliding_window(image, *self._window_size,
                                               stride=(self._window_size[0], self._window_size[1])):
            row_end = min(row + self._window_size[0], image.shape[0])
            col_end = min(col + self._window_size[1], image.shape[1])
            mean = stats.calc_mean(row, row_end, col, col_end)
            std = stats.calc_std(row, row_end, col, col_end)
            threshold = mean * (1 + self._k * (std / self._r - 1))
            result[row:row_end, col:col_end] = window >= threshold
        return result * 255


class SauvolaPixByBlockBinarizer(Binarizer):
    def __init__(self, k=0.5, r=128, window_size=(65, 65)):
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


class OtsuBinarizer(Binarizer):
    def __init__(self, window_size=(64, 64)):
        self._window_size = window_size

    @staticmethod
    def _calc_threshold_in_window(window):
        histogram = np.histogram(window, 256, (0, 255))[0]
        count_back, count_front = 0, np.prod(window.shape)
        sum_back, sum_front = 0, np.sum(window)

        threshold_best = None
        sigma_max = -1
        for threshold in range(1, 256):
            count_back += histogram[threshold - 1]
            count_front -= histogram[threshold - 1]
            sum_current = (threshold - 1) * histogram[threshold - 1]
            sum_back += sum_current
            sum_front -= sum_current
            if count_back == 0:
                continue
            if count_front == 0:
                break

            mean_back, mean_front = sum_back / count_back, sum_front / count_front
            sigma_between = count_back * count_front * (mean_back - mean_front) ** 2
            if sigma_max < sigma_between:
                sigma_max = sigma_between
                threshold_best = threshold
        if threshold_best is None:
            raise ValueError('Image has only one unique intensity value')
        return threshold_best

    def binarize(self, image):
        result = np.zeros(image.shape, dtype=np.uint8)
        for row, col, window, _ in sliding_window_centered(image, *self._window_size):
            threshold = self._calc_threshold_in_window(window)
            result[row, col] = image[row, col] >= threshold
        return result * 255
