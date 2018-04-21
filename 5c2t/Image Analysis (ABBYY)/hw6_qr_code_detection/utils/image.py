import numpy as np
import cv2


def detect_pixel_interval(image):
    return image.min(), image.max()


def convert_pixels_to_ones(image, interval=None):
    interval = interval or detect_pixel_interval(image)
    if interval == (-1, 1):
        return image
    center = (interval[0] + interval[1]) / 2
    scale = interval[1] - center
    return np.clip((image - center) / scale, -1, 1)


def convert_pixels_to_zero_one(image, interval=None):
    interval = interval or detect_pixel_interval(image)
    if interval == (0, 1):
        return image
    if interval != (-1, 1):
        image = convert_pixels_to_ones(image, interval=interval)
    return np.clip((image + 1) / 2, 0, 1)


def convert_pixels_to_uint8(image, interval=None):
    interval = interval or detect_pixel_interval(image)
    if interval == (0, 255):
        return image
    if interval != (0, 1):
        image = convert_pixels_to_zero_one(image, interval)
    return np.round(image * 255).astype(np.uint8)


def invert_image(image):
    return -image


def move_channels_axis(image, position='first'):
    if position not in {'first', 'last'}:
        raise ValueError('Wrong position. Must be \'first\' or \'last\'')
    permutation = (2, 0, 1) if position == 'first' else (1, 2, 0)
    return np.transpose(image, permutation)


def convert_to_opencv_format(image, convert_color=True):
    is_grey = image.shape[0] == 1
    # pixel conversion will implicitly invert the image
    image = invert_image(image)
    image = convert_pixels_to_uint8(image, interval=(-1, 1))
    if is_grey:
        image = image[0]
    else:
        image = move_channels_axis(image, position='last')
        if convert_color:
            image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
    return image


def convert_to_tensor_format(image, convert_color=True):
    is_grey = len(image.shape) == 2
    if is_grey:
        image = np.expand_dims(image, 0)
    else:
        if convert_color:
            image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        image = move_channels_axis(image, position='first')
    image = convert_pixels_to_ones(image, interval=(0, 255))
    # pixel conversion implicitly inverts the image
    image = invert_image(image)
    return image
