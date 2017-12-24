import numpy as np


def calc_mse(image_original, image_noise):
    return np.mean(np.power(image_original - image_noise, 2))


def calc_peak_signal_to_noise_ratio(image_original, image_noise, max_intensity=255):
    mse = calc_mse(image_original, image_noise)
    return 10 * np.log10(max_intensity ** 2 / mse)
