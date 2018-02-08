from sys import argv
import os.path
import numpy as np
from tqdm import tqdm

from common.io import save_image, read_image


def apply_box_filter(image, width, height):
    cumsum = np.cumsum(np.cumsum(image, axis=0), axis=1)
    result = np.zeros(image.shape, dtype=np.float)

    filter_shape = np.array([height, width])
    filter_half = (filter_shape - 1) // 2
    image_shape = np.array(image.shape)
    with tqdm(total=np.prod(image.shape)) as progress_bar:
        for row in range(0, image.shape[0]):
            for col in range(0, image.shape[1]):
                coords = np.array([row, col])
                filter_start = np.maximum(coords - filter_half, (0, 0))
                filter_end = np.minimum(coords + filter_shape - filter_half, image_shape) - 1
                filter_size = np.prod(filter_end - filter_start + 1)

                pixel = cumsum[filter_end[0], filter_end[1]]
                if filter_start[0] > 0:
                    pixel -= cumsum[filter_start[0] - 1, filter_end[1]]
                    if filter_start[1] > 0:
                        pixel += cumsum[filter_start[0] - 1, filter_start[1] - 1]
                if filter_start[1] > 0:
                    pixel -= cumsum[filter_end[0], filter_start[1] - 1]

                result[row, col] = pixel / filter_size
                progress_bar.update(1)
    return np.clip(np.round(result), 0, 255).astype(np.uint8)


def apply_box_filter_and_save(src_path, dst_path, w, h):
    image = read_image(src_path, convert_to_zero_one=False)
    image = apply_box_filter(image, w, h)
    save_image(dst_path, image, convert_to_uint8=False)


if __name__ == '__main__':
    assert len(argv) == 5
    assert os.path.exists(argv[1])
    argv[3] = int(argv[3])
    argv[4] = int(argv[4])
    assert argv[3] > 0
    assert argv[4] > 0

    apply_box_filter_and_save(*argv[1:])
