import numpy as np


def find_first_non_green_cell(image):
    for row in range(image.shape[0]):
        for col in range(image.shape[1]):
            for channel in [0, 2]:
                if image[row, col, channel] > 0:
                    return dict(row=row, col=col, channel=channel)
    return None


def detect_config(image):
    non_green_cell = find_first_non_green_cell(image)
    if non_green_cell is None:
        raise ValueError('Cannot detect bayer filter configuration: image is black')
    channel = non_green_cell['channel']
    if non_green_cell['row'] % 2 == 0:
        if non_green_cell['col'] % 2 == 0:
            configuration = [[channel, 1], [1, 2 - channel]]
        else:
            configuration = [[1, channel], [2 - channel, 1]]
    else:
        if non_green_cell['col'] % 2 == 0:
            configuration = [[2 - channel, 1], [1, channel]]
        else:
            configuration = [[1, 2 - channel], [channel, 1]]
    return np.array(configuration)


def shift_config(config, shift):
    shift = np.array(shift) % 2
    return np.roll(config, shift, [0, 1])


def get_channel(config, row, col):
    return config[row % 2, col % 2]
