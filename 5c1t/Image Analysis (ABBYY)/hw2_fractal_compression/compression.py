import cv2
import numpy as np
from collections import namedtuple


def sliding_window(image, height, width, stride=(1, 1)):
    for row in range(0, image.shape[0] - height, stride[0]):
        for col in range(0, image.shape[1] - width, stride[1]):
            yield row, col, image[row:row + height, col:col + width]


class ImageFractalCompressor:
    """Image fractal compression.
    Block is a subimage of shape (pattern_size * 2, pattern_size * 2),
    pattern in a compressed representation of block with shape (pattern_size, pattern_size)
    """
    BlockCompressParams = namedtuple('BlockDecompressParameters', ['row', 'col', 'intensity_params'])
    DTYPE_INTENSITY_PARAMS = np.uint8
    DTYPE_COORDINATES = np.uint16

    def __init__(self, pattern_size=4, variance_threshold=5):
        self._pattern_size = pattern_size
        # self._variance_lower_bound = variance_threshold

        # auxiliary fields for fitting
        self._sums_sqr = None
        self._sums = None
        self._image = None
        self._pattern_to_block = None

    @staticmethod
    def _calc_compression_parameters(pattern, block):
        dtype = ImageFractalCompressor.DTYPE_INTENSITY_PARAMS
        scale = np.floor((np.minimum((np.std(pattern) / np.std(block)), 1)) * 255).astype(dtype)
        shift = (np.floor(np.mean(pattern) - np.mean(block)) + 255).astype(dtype)
        return np.array([scale, shift])

    @staticmethod
    def _compress_block(block, parameters):
        scale, shift = parameters
        block_resized = cv2.resize(block, None, fx=0.5, fy=0.5)
        return np.floor(block_resized * (scale / 255)) + (shift - 255)

    @staticmethod
    def _calc_distance(pattern1, pattern2):
        return np.linalg.norm(pattern1 - pattern2)

    def _find_similar_block(self, pattern):
        block_size = 2 * self._pattern_size
        compression_params = [self.BlockCompressParams(row, col, self._calc_compression_parameters(pattern, block))
                              for row, col, block in sliding_window(image, block_size, block_size,
                                                                    (block_size, block_size))]
        blocks_compressed = (self._compress_block(block, params.intensity_params)
                             for (_, _, block), params in zip(sliding_window(image, block_size, block_size,
                                                                             (block_size, block_size)),
                                                              compression_params))
        # find block with lexicographically minimal pair: (distance to pattern, -variance)
        return min(zip(blocks_compressed, compression_params),
                   key=lambda pair: (self._calc_distance(pattern, pair[0]), -np.var(pair[0])))[1]

    def fit(self, image):
        self._image = image
        self._sums = np.cumsum(image).resize(image.shape)
        self._sums_sqr = np.cumsum(np.power(image, 2)).resize(image.shape)
        self._pattern_to_block = {}
        for pattern_row, pattern_col, pattern in sliding_window(image,
                                                                self._pattern_size,
                                                                self._pattern_size,
                                                                (self._pattern_size, self._pattern_size)):
            self._pattern_to_block[(pattern_row, pattern_col)] = self._find_similar_block(pattern)
        return self._pattern_to_block

    def save(self, path):
        if self._pattern_to_block is None:
            raise ValueError('save_compressed cannot be called before fit or load')
        with open(path, 'wb') as fout:
            for pattern_row, pattern_col, _ in sliding_window(self._image, self._pattern_size, self._pattern_size,
                                                              (self._pattern_size, self._pattern_size)):
                compression_params = self._pattern_to_block[(pattern_row, pattern_col)]
                coordinates = np.array([compression_params.row, compression_params.col], dtype=self.DTYPE_COORDINATES)
                intensity_params = compression_params.intensity_params
                # 2*2 bytes to store row and col of a block
                coordinates.tofile(fout)
                # 2*1 bytes to store intensity params
                intensity_params.tofile(fout)

    def load(self, path):
        self._pattern_to_block = {}
        with open(path, 'rb') as fin:
            for pattern_row, pattern_col, _ in sliding_window(self._image, self._pattern_size, self._pattern_size,
                                                              (self._pattern_size, self._pattern_size)):
                coordinates = tuple(np.fromfile(fin, self.DTYPE_COORDINATES, 2))
                intensity_params = np.fromfile(fin, self.DTYPE_INTENSITY_PARAMS, 2)
                self._pattern_to_block[(pattern_row, pattern_col)] = self.BlockCompressParams(*coordinates,
                                                                                              intensity_params)

    def uncompress(self, image):
        if self._pattern_to_block is None:
            raise ValueError('uncompress cannot be called before fit or load')
        for pattern_row, pattern_col, pattern in sliding_window(image, self._pattern_size, self._pattern_size,
                                                                (self._pattern_size, self._pattern_size)):
            block_row, block_col, _ = self._pattern_to_block[(pattern_row, pattern_col)]


if __name__ == '__main__':
    from os.path import join, splitext

    image_name = 'Boat64.bmp'
    path_image = join('data', image_name)
    image = cv2.imread(path_image, cv2.IMREAD_GRAYSCALE)

    compressor = ImageFractalCompressor()
    print('Fit')
    compressor.fit(image)

    path_result = join('data_compressed', splitext(image_name)[0])
    print('Save')
    compressor.save(path_result)
    a = compressor._pattern_to_block
    print('Load')
    compressor.load(path_result)
    b = compressor._pattern_to_block
    print(a)
    print(b)
