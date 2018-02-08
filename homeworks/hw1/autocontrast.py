from sys import argv
import os.path
import numpy as np

from common.io import read_image, save_image


def autocontrast(img, white_percent, black_percent):
    histogram = np.bincount(img.ravel(), minlength=256)
    percentage = np.cumsum(histogram) / np.prod(img.shape)
    black_percentile = percentage.searchsorted(black_percent, side='right')
    white_percentile = percentage.searchsorted(1 - white_percent, side='left')
    if white_percentile <= black_percentile:
        return img
    contrast = 255 / (white_percentile - black_percentile)
    intensity = -black_percentile * contrast
    return np.clip(img * contrast + intensity, 0, 255).astype(np.uint8)


def autocontrast_and_save(src_path, dst_path, white_perc, black_perc):
    if white_perc + black_perc > 1.0:
        raise ValueError('Sum of white pixel percentage and'
                         'black pixel percentage cannot exceed 1.0')
    img = read_image(src_path, convert_to_zero_one=False)
    img_contrasted = autocontrast(img, white_perc, black_perc)
    save_image(dst_path, img_contrasted, convert_to_uint8=False)


if __name__ == '__main__':
    assert len(argv) == 5
    assert os.path.exists(argv[1])
    argv[3] = float(argv[3])
    argv[4] = float(argv[4])

    assert 0 <= argv[3] < 1
    assert 0 <= argv[4] < 1

    autocontrast_and_save(*argv[1:])
