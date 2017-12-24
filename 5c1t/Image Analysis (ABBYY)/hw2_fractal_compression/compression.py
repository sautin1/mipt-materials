import cv2
import numpy as np
from tqdm import tqdm
from collections import namedtuple, OrderedDict

from image import ImageStatsCalculator
from metrics import calc_mse


def sliding_window(image, height, width=None, stride=(1, 1)):
    width = width or height
    if isinstance(stride, int):
        stride = (stride, stride)
    for row in range(0, image.shape[0] - height + 1, stride[0]):
        for col in range(0, image.shape[1] - width + 1, stride[1]):
            yield row, col, image[row:row + height, col:col + width]


class ImageFractalCompressor:
    """Image fractal compression.
    Block is a subimage of shape (pattern_size * 2, pattern_size * 2),
    pattern in a compressed representation of block with shape (pattern_size, pattern_size)
    """
    BlockCompressParams = namedtuple('BlockCompressParameters', ['row', 'col', 'intensity_params'])
    DTYPE_INTENSITY_PARAMS = np.uint8
    DTYPE_COORDINATES = np.uint16

    def __init__(self, pattern_size=4):
        self._pattern_size = pattern_size
        self._block_size = self._pattern_size * 2

        self._pattern_to_block = None

    def _calc_compression_parameters(self, stats, pattern_start, block_start):
        pattern_coords = (pattern_start[0], pattern_start[0] + self._pattern_size,
                          pattern_start[1], pattern_start[1] + self._pattern_size)
        block_coords = (block_start[0], block_start[0] + self._block_size,
                        block_start[1], block_start[1] + self._block_size)

        dtype = ImageFractalCompressor.DTYPE_INTENSITY_PARAMS
        scale = np.minimum(stats.calc_std(*pattern_coords) / stats.calc_std(*block_coords), 1)
        # fit into {0, ..., 255}
        scale = np.floor(scale * 255).astype(dtype)

        shift = stats.calc_mean(*pattern_coords) - stats.calc_mean(*block_coords)
        # now shift is a float in interval [-255, 255]
        # fit it into {0, ..., 255} so that 0 maps to 128
        shift = (np.floor(shift + 255) / 2 + 0.5).astype(dtype)
        return scale, shift

    def _compress_block(self, block, parameters):
        scale, shift = parameters
        scale = scale.astype(np.float) / 255
        shift = (shift.astype(np.float) - 0.5) * 2 - 255

        # resize by taking mean of all 2x2 non-overlapping subblocks
        block_resized = np.zeros((self._block_size // 2, self._block_size // 2), dtype=np.float)
        for offset_row, offset_col, subblock in sliding_window(block, 2, stride=2):
            block_resized[offset_row // 2, offset_col // 2] = np.mean(subblock)
        return np.clip(np.floor(block_resized * scale + shift), 0, 255).astype(np.uint8)

    @staticmethod
    def _calc_distance(pattern1, pattern2):
        return calc_mse(pattern1, pattern2)

    def _find_similar_block(self, image, stats, pattern, pattern_start):
        compression_params = [self.BlockCompressParams(row, col,
                                                       self._calc_compression_parameters(stats, pattern_start,
                                                                                         (row, col)))
                              for row, col, block in sliding_window(image, self._block_size)]
        blocks_compressed = (self._compress_block(block, params.intensity_params)
                             for (_, _, block), params in zip(sliding_window(image, self._block_size),
                                                              compression_params))
        return min(zip(blocks_compressed, compression_params),
                   key=lambda pair: self._calc_distance(pattern, pair[0]))[1]

    def fit(self, image):
        self._pattern_to_block = OrderedDict()
        stats = ImageStatsCalculator(image)
        for pattern_row, pattern_col, pattern in tqdm(sliding_window(image, self._pattern_size,
                                                                     stride=self._pattern_size),
                                                      total=np.prod(image.shape) // np.power(self._pattern_size, 2)):
            pattern_start = (pattern_row, pattern_col)
            self._pattern_to_block[pattern_start] = self._find_similar_block(image, stats, pattern, pattern_start)
        return self._pattern_to_block

    def save(self, path):
        if self._pattern_to_block is None:
            raise ValueError('save_compressed cannot be called before fit or load')
        with open(path, 'wb') as fout:
            for block_row, block_col, params in self._pattern_to_block.values():
                coordinates = np.array([block_row, block_col], dtype=self.DTYPE_COORDINATES)
                intensity_params = np.array(params, dtype=self.DTYPE_INTENSITY_PARAMS)
                # 2*2 bytes to store row and col of a block
                coordinates.tofile(fout)
                # 2*1 bytes to store intensity params
                intensity_params.tofile(fout)

    def load(self, path, image_shape):
        self._pattern_to_block = {}
        image_dummy = np.zeros(image_shape, dtype=np.uint8)
        with open(path, 'rb') as fin:
            for pattern_row, pattern_col, _ in sliding_window(image_dummy, self._pattern_size,
                                                              stride=self._pattern_size):
                coordinates = tuple(np.fromfile(fin, self.DTYPE_COORDINATES, 2))
                intensity_params = tuple(np.fromfile(fin, self.DTYPE_INTENSITY_PARAMS, 2))
                self._pattern_to_block[(pattern_row, pattern_col)] = self.BlockCompressParams(*coordinates,
                                                                                              intensity_params)

    def uncompress(self, image):
        if self._pattern_to_block is None:
            raise ValueError('uncompress cannot be called before fit or load')
        pattern_size = self._pattern_size
        result = np.zeros(image.shape, dtype=np.uint8)
        for (pattern_row, pattern_col), (block_row, block_col, params) in self._pattern_to_block.items():
            block = image[block_row:block_row + self._block_size, block_col:block_col + self._block_size]
            result[pattern_row:pattern_row + pattern_size,
                   pattern_col:pattern_col + pattern_size] = self._compress_block(block,
                                                                                  params)
        return result


if __name__ == '__main__':
    from os.path import join, splitext
    from os import makedirs, listdir
    from matplotlib import pyplot as plt

    from image import save_image, read_image
    from metrics import calc_peak_signal_to_noise_ratio

    PSNR_THRESHOLD = 40
    MAX_ITER_COUNT = 50
    PATTERN_SIZE = 4

    images_names = ['Boat64.bmp']  # listdir('data')
    for image_name in images_names:
        print(image_name)
        path_image = join('data', image_name)
        path_results = join('results', f'{PATTERN_SIZE}x{PATTERN_SIZE}', splitext(image_name)[0])
        path_compressed = join(path_results, 'compressed')
        path_restored = join(path_results, 'restored')
        path_metrics = join(path_results, 'psnr')
        makedirs(path_restored, exist_ok=True)

        image_original = read_image(path_image, cv2.IMREAD_GRAYSCALE)

        compressor = ImageFractalCompressor(PATTERN_SIZE)
        print('Fitting')
        compressor.fit(image_original)
        compressor.save(path_compressed)

        image_restored = np.full(image_original.shape, 128, dtype=np.uint8)
        psnrs = []
        print('Restoring')
        for i in tqdm(range(MAX_ITER_COUNT)):
            image_restored_new = compressor.uncompress(image_restored) if i > 0 else image_restored
            psnr = calc_peak_signal_to_noise_ratio(image_original, image_restored_new)
            save_image(image_restored_new, join(path_restored, str(i) + '.png'))
            psnrs.append(psnr)
            if i > 0 and psnr >= PSNR_THRESHOLD:
                break
            image_restored = image_restored_new
        with open(path_metrics + '.txt', 'w') as fout:
            fout.write('\n'.join(map(str, psnrs)))
        fig = plt.figure()
        plt.plot(psnrs)
        fig.savefig(path_metrics + '.png')
        print()
