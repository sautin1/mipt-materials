import cv2
import numpy as np
from collections import namedtuple


def sliding_window(image, height, width):
    for row in range(image.shape[0] - height):
        for col in range(image.shape[1] - width):
            yield row, col, image[row:row + height, col:col + width]


class ImageFractalCompressor:
    """Image fractal compression.
    Block is a subimage of shape (pattern_size * 2, pattern_size * 2),
    pattern in a compressed representation of block with shape (pattern_size, pattern_size)
    """
    BlockCompressParams = namedtuple('BlockDecompressParameters', ['row', 'col', 'intensity_params'])

    def __init__(self, pattern_size=4, variance_threshold=5):
        self._pattern_size = pattern_size
        # self._variance_lower_bound = variance_threshold
        self._pattern_to_block = None

    @staticmethod
    def _calc_compression_parameters(pattern, block):
        scale = np.floor((np.minimum((np.std(pattern) / np.std(block)), 1)) * 255).astype(np.uint8)
        shift = np.floor(np.mean(pattern) - np.mean(block)).astype(np.byte)
        return scale.item(), shift.item()

    @staticmethod
    def _compress_block(block, parameters):
        scale, shift = parameters
        block_resized = cv2.resize(block, None, fx=0.5, fy=0.5)
        return np.floor(block_resized * (scale / 255)) + shift

    @staticmethod
    def _calc_distance(pattern1, pattern2):
        return np.linalg.norm(pattern1 - pattern2)

    def _find_similar_block(self, pattern):
        block_size = 2 * self._pattern_size
        compression_params = [self.BlockCompressParams(row, col, self._calc_compression_parameters(pattern, block))
                              for row, col, block in sliding_window(image, block_size, block_size)]
        blocks_compressed = (self._compress_block(block, params.intensity_params)
                             for (_, _, block), params in zip(sliding_window(image, block_size, block_size),
                                                              compression_params))
        # find block with lexicographically minimal pair: (distance to pattern, -variance)
        return min(zip(blocks_compressed, compression_params),
                   key=lambda pair: (self._calc_distance(pattern, pair[0]), -np.var(pair[0])))[1]

    def fit(self, image):
        self._pattern_to_block = {}
        for pattern_row, pattern_col, pattern in sliding_window(image, self._pattern_size, self._pattern_size):
            self._pattern_to_block[(pattern_row, pattern_col)] = self._find_similar_block(pattern)
        return self._pattern_to_block

    def uncompress(self, image):
        if self._pattern_to_block is None:
            raise ValueError('uncompress cannot be called before fit')
        for pattern_row, pattern_col, pattern in sliding_window(image, self._pattern_size, self._pattern_size):
            block_row, block_col, _ = self._pattern_to_block[(pattern_row, pattern_col)]


if __name__ == '__main__':
    from os.path import join

    image_path = join('data', 'Boat.bmp')
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)

    compressor = ImageFractalCompressor()
    compressor.fit(image)
