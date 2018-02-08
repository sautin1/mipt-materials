import numpy as np
import cv2


def read_image(path, is_color=False, convert_to_zero_one=True):
    mode = cv2.IMREAD_COLOR if is_color else cv2.IMREAD_GRAYSCALE
    image = cv2.imread(path, mode)
    if convert_to_zero_one:
        image = image.astype(np.float) / 255
    if is_color:
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    return image


def save_image(path, image, convert_to_uint8=True):
    if convert_to_uint8:
        image = np.clip(np.round(image * 255), 0, 255).astype(np.uint8)
    if len(image.shape) == 3:
        # color image
        image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
    cv2.imwrite(path, image)
