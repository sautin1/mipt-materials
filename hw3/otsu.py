from sys import argv
import os.path
import numpy as np

from common.io import read_image, save_image


def calc_otsu_threshold(image):
    histogram = np.histogram(image, 256, (0, 255))[0]
    count_back, count_front = 0, np.prod(image.shape)
    sum_back, sum_front = 0, np.sum(image)

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
    # subtract 1 to divide pixels into (<= thr, > thr), not (< thr, >= thr)
    return threshold_best - 1


def binarize_with_otsu(image):
    threshold = calc_otsu_threshold(image)
    return np.greater(image, threshold).astype(np.uint8) * 255


def apply_otsu_and_save(src_path, dst_path):
    image = read_image(src_path, convert_to_zero_one=False)
    image_binarized = binarize_with_otsu(image)
    save_image(dst_path, image_binarized, convert_to_uint8=False)


if __name__ == '__main__':
    assert len(argv) == 3
    assert os.path.exists(argv[1])
    apply_otsu_and_save(*argv[1:])
