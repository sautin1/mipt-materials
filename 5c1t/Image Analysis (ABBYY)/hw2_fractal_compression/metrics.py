import numpy as np


def peak_signal_to_noise_ratio(image_original, image_noise, max_intensity=255):
    mse = 1 / np.prod(image_original.shape) * np.sum(np.power(image_original - image_noise, 2))
    return 20 * np.log10(max_intensity) - 10 * np.log10(mse)
