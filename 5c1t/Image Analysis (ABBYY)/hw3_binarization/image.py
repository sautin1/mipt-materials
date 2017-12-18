import cv2
import numpy as np


def read_image(path):
    image = cv2.imread(path, cv2.IMREAD_COLOR)
    if image is None:
        raise ValueError(f'Cannot read image: {path}')
    image_grey = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    return image_grey


def save_image(image, path):
    cv2.imwrite(path, image)


def sliding_window(image, height, width, stride=(1, 1)):
    for row in range(0, image.shape[0] - height, stride[0]):
        for col in range(0, image.shape[1] - width, stride[1]):
            yield row, col, image[row:row + height, col:col + width]


def sliding_window_centered(image, height, width, stride=(1, 1)):
    for row in range(0, image.shape[0], stride[0]):
        for col in range(0, image.shape[1], stride[1]):
            row_begin, col_begin = row - height // 2, col - width // 2
            row_end, col_end = min(row_begin + height, image.shape[0]), min(col_begin + width, image.shape[1])
            row_begin, col_begin = max(row_begin, 0), max(col_begin, 0)
            yield row, col, image[row_begin:row_end, col_begin:col_end], (row_begin, row_end, col_begin, col_end)


class ImageStatsCalculator:
    def __init__(self, image):
        self._sums = self.calc_partial_sums(image)
        self._sqr_sums = self.calc_partial_sums(np.power(image.astype(np.uint64), 2))

        self._image = image

    @staticmethod
    def calc_partial_sums(image):
        sums = np.zeros(image.shape, dtype=np.uint64)
        for row in range(0, image.shape[0]):
            for col in range(0, image.shape[1]):
                sums[row, col] = image[row, col]
                if row > 0:
                    sums[row, col] += sums[row - 1, col]
                    if col > 0:
                        sums[row, col] -= sums[row - 1, col - 1]
                if col > 0:
                    sums[row, col] += sums[row, col - 1]
        return sums

    @staticmethod
    def _calc_sum(sums, row_begin, row_end, col_begin, col_end):
        row_end, col_end = row_end - 1, col_end - 1
        result = sums[row_end, col_end]
        if row_begin > 0:
            result -= sums[row_begin - 1, col_end]
            if col_begin > 0:
                result += sums[row_begin - 1, col_begin - 1]
        if col_begin > 0:
            result -= sums[row_end, col_begin - 1]
        return result

    def _calc_mean(self, sums, row_begin, row_end, col_begin, col_end):
        size = (row_end - row_begin) * (col_end - col_begin)
        sum = self._calc_sum(sums, row_begin, row_end, col_begin, col_end)
        return sum / size

    def calc_sum(self, row_begin, row_end, col_begin, col_end):
        return self._calc_sum(self._sums, row_begin, row_end, col_begin, col_end)

    def calc_mean(self, row_begin, row_end, col_begin, col_end):
        return self._calc_mean(self._sums, row_begin, row_end, col_begin, col_end)

    def calc_var(self, row_begin, row_end, col_begin, col_end):
        sqr_mean = self._calc_mean(self._sqr_sums, row_begin, row_end, col_begin, col_end)
        mean = self.calc_mean(row_begin, row_end, col_begin, col_end)
        return sqr_mean - np.power(mean, 2)

    def calc_std(self, row_begin, row_end, col_begin, col_end):
        return np.sqrt(self.calc_var(row_begin, row_end, col_begin, col_end))