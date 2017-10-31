import numpy as np
from functools import partial
from tqdm import tqdm

from bayer import detect_config, shift_config, get_channel


def _get_pixel_by_offset(matrix, config, offset):
    channel = get_channel(config, offset[0], offset[1])
    return matrix[offset[0], offset[1], channel]


class DemosaicingVNG:
    center = np.array([2, 2])

    def __init__(self, k1=1.5, k2=0.5):
        self.k1 = k1
        self.k2 = k2

    def demosaic(self, image):
        bayer_config = detect_config(image)
        image = image.astype(np.float)
        result = np.zeros(image.shape, dtype=np.float)

        image = self._extend_image(image)

        with tqdm(total=(image.shape[0] - 4) * (image.shape[1] - 4)) as progress_bar:
            for row in range(image.shape[0] - 4):
                for col in range(image.shape[1] - 4):
                    matrix = image[row:row + 5, col:col + 5]
                    config = shift_config(bayer_config, (row, col))
                    result[row, col] = self._interpolate_central_pixel(matrix, config)
                    progress_bar.update(1)
        return np.round(np.clip(result, 0, 255)).astype(np.uint8)

    @staticmethod
    def _extend_image(image):
        image_extended = np.zeros(np.array(image.shape) + (4, 4, 0), dtype=np.float)
        image_extended[2:-2, 2:-2, :] = image
        image_extended[:2, 2:-2, :] = image[:2, :, :]
        image_extended[-2:, 2:-2, :] = image[-2:, :, :]
        image_extended[2:-2, :2, :] = image[:, :2, :]
        image_extended[2:-2, -2:, :] = image[:, -2:, :]

        image_extended[:2, :2, :] = image[:2, :2, :]
        image_extended[:2, -2:, :] = image[:2, -2:, :]
        image_extended[-2:, :2, :] = image[-2:, :2, :]
        image_extended[-2:, -2:, :] = image[-2:, -2:, :]
        return image_extended

    def _interpolate_central_pixel(self, matrix, config):
        gradients = GradientCalculator().calculate(matrix, config)
        threshold = self._calc_threshold(gradients)
        directions = [np.array(key) for key, value in gradients.items() if value <= threshold]
        return self._interpolate_pixel_by_directions(matrix, config, directions)

    def _calc_threshold(self, gradients):
        gradient_min, gradient_max = min(gradients.values()), max(gradients.values())
        return self.k1 * gradient_min + self.k2 * (gradient_max - gradient_min)

    @staticmethod
    def _interpolate_pixel_by_directions(matrix, config, directions):
        interpolate = DemosaicingVNG._interpolate_green_pixel_by_directions
        if config[0, 0] != 1:
            interpolate = DemosaicingVNG._interpolate_non_green_pixel_by_directions
        return interpolate(matrix, config, directions)

    @staticmethod
    def _interpolate_green_pixel_by_directions(matrix, config, directions):
        c = DemosaicingVNG.center  # center
        get_pixel = partial(_get_pixel_by_offset, matrix, config)
        sums = np.array([0, 0, 0], dtype=np.float)  # red sum, green sum, blue sum
        for d in directions:
            step_from_center = c + d
            if d[0] == 0 or d[1] == 0:
                channel_in_direction = get_channel(config, *step_from_center)
                channel_orthogonal = 2 - channel_in_direction
                d_orth = d[::-1]  # orthogonal direction
                sums[channel_in_direction] += get_pixel(step_from_center)
                for offset in [d_orth, -d_orth, d_orth + 2 * d, -d_orth + 2 * d]:
                    sums[channel_orthogonal] += get_pixel(c + offset) / 4
                sums[1] += (get_pixel(step_from_center + d) + get_pixel(c)) / 2
            else:
                vert = c + (d[0], 0)
                hor = c + (0, d[1])
                channel_vertical = get_channel(config, *vert)
                channel_horizontal = 2 - channel_vertical
                sums[channel_vertical] += get_pixel(vert) / 2
                sums[channel_vertical] += get_pixel(vert + (0, 2 * d[1])) / 2
                sums[channel_horizontal] += get_pixel(hor) / 2
                sums[channel_horizontal] += get_pixel(hor + (2 * d[0], 0)) / 2
                sums[1] += get_pixel(step_from_center)
        diffs = (sums - sums[1]) / len(directions) if directions else sums
        return diffs + get_pixel(c)

    @staticmethod
    def _interpolate_non_green_pixel_by_directions(matrix, config, directions):
        c = DemosaicingVNG.center  # center
        get_pixel = partial(_get_pixel_by_offset, matrix, config)
        channel_center = config[0, 0]
        channel_diagonal = 2 - channel_center
        sums = np.array([0, 0, 0], dtype=np.float)  # red sum, green sum, blue sum
        for d in directions:
            step_from_center = c + d
            if d[0] == 0 or d[1] == 0:
                d_orth = d[::-1]  # orthogonal direction
                sums[channel_center] += (get_pixel(step_from_center + d) + get_pixel(c)) / 2
                sums[channel_diagonal] += get_pixel(step_from_center + d_orth) / 2
                sums[channel_diagonal] += get_pixel(step_from_center - d_orth) / 2
                sums[1] += get_pixel(step_from_center)
            else:
                sums[channel_center] += (get_pixel(step_from_center + d) + get_pixel(c)) / 2
                sums[channel_diagonal] += get_pixel(step_from_center)
                for offset in [(-1, 0), (0, -1), (0, 1), (1, 0)]:
                    sums[1] += get_pixel(step_from_center + offset) / 4
        diffs = (sums - sums[channel_center]) / len(directions)
        return diffs + get_pixel(c)


class GradientCalculator:
    center = np.array([2, 2])

    def calculate(self, matrix, config):
        if config[0, 0] == 1:
            calc_diagonal = self._calc_diagonal_for_green_center
        else:
            calc_diagonal = self._calc_diagonal_for_non_green_center
        gradients = {}
        for row_direction in [-1, 0, 1]:
            for col_direction in [-1, 0, 1]:
                if row_direction == 0 and col_direction == 0:
                    continue
                direction = (row_direction, col_direction)
                if row_direction == 0 or col_direction == 0:
                    gradients[direction] = self._calc_straight(matrix, config, direction)
                else:
                    gradients[direction] = calc_diagonal(matrix, config, direction)
        return gradients

    @staticmethod
    def _calc_straight(matrix, config, direction):
        c, d = GradientCalculator.center, np.array(direction)  # center, direction
        get_pixel = partial(_get_pixel_by_offset, matrix, config)

        is_hor = direction[0] == 0  # is horizontal
        m5 = c + ((-1, 0) if is_hor else (0, -1))  # aux for component#5
        m6 = c + ((1, 0) if is_hor else (0, 1))  # aux for component#6
        d_double = d * 2
        components = np.abs([
            get_pixel(c + d) - get_pixel(c - d),
            get_pixel(c + d_double) - get_pixel(c),
            get_pixel(c + ((-1, 1) if is_hor else (1, -1))) - get_pixel(c + (-1, -1)),
            get_pixel(c + (1, 1)) - get_pixel(c + ((1, -1) if is_hor else (-1, 1))),
            get_pixel(m5 + d_double) - get_pixel(m5),
            get_pixel(m6 + d_double) - get_pixel(m6)
        ]).astype(np.float)
        return np.sum(components[:2]) + np.sum(components[2:]) / 2

    @staticmethod
    def _calc_diagonal_for_green_center(matrix, config, direction):
        c, d = GradientCalculator.center, np.array(direction)  # center, direction
        get_pixel = partial(_get_pixel_by_offset, matrix, config)

        m3 = c + ((0, -1) if d[1] == 1 else (0, 1))  # aux for component#3
        m4 = c + ((-1, 0) if d[0] == 1 else (1, 0))  # aux for component#4
        d_double = d * 2
        components = np.abs([
            get_pixel(c + d) - get_pixel(c - d),
            get_pixel(c + d_double) - get_pixel(c),
            get_pixel(m3 + d_double) - get_pixel(m3),
            get_pixel(m4 + d_double) - get_pixel(m4)
        ]).astype(np.float)
        return np.sum(components)

    @staticmethod
    def _calc_diagonal_for_non_green_center(matrix, config, direction):
        c, d = GradientCalculator.center, np.array(direction)  # center, direction
        get_pixel = partial(_get_pixel_by_offset, matrix, config)

        m5 = c + (-1, 0)  # aux for component#5
        m6 = c + (1, 0)  # aux for component#6

        components = np.abs([
            get_pixel(c + d) - get_pixel(c - d),
            get_pixel(c + 2 * d) - get_pixel(c),
            get_pixel(c + (0, d[0] * d[1])) - get_pixel(m5),
            get_pixel(m6) - get_pixel(c + (0, -d[0] * d[1])),
            get_pixel(m5 + d * (2 if d[0] == 1 else 1)) - get_pixel(m5 + d * (1 if d[0] == 1 else 0)),
            get_pixel(m6 + d * (2 if d[0] == -1 else 1)) - get_pixel(m6 + d * (1 if d[0] == -1 else 0)),
        ]).astype(np.float)
        return np.sum(components[:2]) + np.sum(components[2:]) / 2

if __name__ == '__main__':
    import cv2
    from os.path import join

    filename = 'RGB_CFA.bmp'
    image = cv2.imread(join('images', 'source', filename), cv2.IMREAD_COLOR)
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

    image_interpolated = DemosaicingVNG().demosaic(image)
    image_interpolated = cv2.cvtColor(image_interpolated, cv2.COLOR_RGB2BGR)
    cv2.imwrite(join('images', 'results', filename), image_interpolated)
