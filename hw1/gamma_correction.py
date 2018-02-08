from sys import argv
import os.path
import numpy as np

from common.io import save_image, read_image


def gamma_correction(src_path, dst_path, a, b):
    img = read_image(src_path)
    img_corrected = a * np.power(img, b)
    save_image(dst_path, img_corrected)

if __name__ == '__main__':
    assert len(argv) == 5
    assert os.path.exists(argv[1])
    argv[3] = float(argv[3])
    argv[4] = float(argv[4])

    gamma_correction(*argv[1:])
