import numpy as np


def calc_mse(image_original, image_noise, normed=False):
    result = np.linalg.norm(image_original - image_noise)
    return result / np.prod(image_original.shape) if normed else result


def calc_peak_signal_to_noise_ratio(image_original, image_noise, max_intensity=255):
    mse = calc_mse(image_original, image_noise, normed=True)
    return 20 * np.log10(max_intensity) - 10 * np.log10(mse)
