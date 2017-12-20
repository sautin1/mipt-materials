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

    def __init__(self, pattern_size=4):
        self._pattern_size = pattern_size

        # auxiliary fields for fitting
        self._image = None
        self._pattern_to_block = None

    @staticmethod
    def _calc_compression_parameters(pattern, block):
        dtype = ImageFractalCompressor.DTYPE_INTENSITY_PARAMS
        scale = np.floor((np.minimum((np.std(pattern) / np.std(block)), 1)) * 255).astype(dtype)
        # np.mean(pattern) - np.mean(block) is a float in interval [-255, 255]
        # convert it into np.uint8
        shift = (np.floor((np.mean(pattern) - np.mean(block) + 255) / 2)).astype(dtype)
        return np.array([scale, shift])

    @staticmethod
    def _compress_block(block, parameters):
        scale, shift = parameters
        scale = scale.astype(np.float) / 255
        shift = shift.astype(np.float) * 2 - 255
        block_resized = cv2.resize(block, None, fx=0.5, fy=0.5)
        return np.clip(np.floor(block_resized * scale + shift), 0, 255).astype(np.uint8)

    @staticmethod
    def _calc_distance(pattern1, pattern2):
        return np.linalg.norm(pattern1 - pattern2)

    def _find_similar_block(self, pattern):
        block_size = 2 * self._pattern_size
        compression_params = [self.BlockCompressParams(row, col, self._calc_compression_parameters(pattern, block))
                              for row, col, block in sliding_window(image_original, block_size, block_size,
                                                                    (block_size, block_size))]
        blocks_compressed = (self._compress_block(block, params.intensity_params)
                             for (_, _, block), params in zip(sliding_window(image_original, block_size, block_size,
                                                                             (block_size, block_size)),
                                                              compression_params))
        # find block with lexicographically minimal pair: (distance to pattern, -variance)
        return min(zip(blocks_compressed, compression_params),
                   key=lambda pair: (self._calc_distance(pattern, pair[0]), -np.var(pair[0])))[1]

    def fit(self, image):
        self._image = image
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
        pattern_size = self._pattern_size
        block_size = 2 * self._pattern_size
        result = np.zeros(image.shape, dtype=np.uint8)
        for (pattern_row, pattern_col), (block_row, block_col, params) in self._pattern_to_block.items():
            block = image[block_row:block_row + block_size, block_col:block_col + block_size]
            result[pattern_row:pattern_row + pattern_size,
                   pattern_col:pattern_col + pattern_size] = self._compress_block(block, params)
        return result


if __name__ == '__main__':
    from os.path import join, splitext
    from os import makedirs, listdir
    from tqdm import tqdm
    from matplotlib import pyplot as plt

    from image import save_image, read_image
    from metrics import peak_signal_to_noise_ratio


    PSNR_THRESHOLD = 40
    MAX_ITER_COUNT = 50

    images_names = listdir('data')
    for image_name in tqdm(images_names):
        path_image = join('data', image_name)
        path_results = join('results', splitext(image_name)[0])
        path_compressed = join(path_results, 'compressed')
        path_restored = join(path_results, 'restored')
        path_metrics = join(path_results, 'psnr')
        makedirs(path_restored, exist_ok=True)

        image_original = read_image(path_image, cv2.IMREAD_GRAYSCALE)

        compressor = ImageFractalCompressor(4)
        compressor.fit(image_original)
        compressor.save(path_compressed)

        image_restored = np.full(image_original.shape, 128, dtype=np.uint8)
        psnrs = []
        for i in range(MAX_ITER_COUNT):
            image_restored = compressor.uncompress(image_restored)
            psnr = peak_signal_to_noise_ratio(image_original, image_restored)
            save_image(image_restored, join(path_restored, str(i) + '.png'))
            psnrs.append(psnr)
            if psnr >= PSNR_THRESHOLD:
                break
        with open(path_metrics + '.txt', 'w') as fout:
            fout.write('\n'.join(map(str, psnrs)))
        fig = plt.figure()
        plt.plot(psnrs)
        fig.savefig(path_metrics + '.png')
