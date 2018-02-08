from __future__ import print_function
from sys import argv
import cv2
import numpy as np
import math
from functools import partial
from os.path import join, basename


def gradient_img(img):
    hor_grad = (img[1:, :] - img[:-1, :])[:, :-1]
    ver_grad = (img[:, 1:] - img[:, :-1])[:-1, :]
    magnitude = np.sqrt(hor_grad ** 2 + ver_grad ** 2)

    return magnitude


def hough_transform(image_gradient, theta_step, rho_step, gradient_threshold=None):
    gradient_threshold = gradient_threshold or (image_gradient.max() * 0.1)

    rho_max = math.sqrt(image_gradient.shape[0] ** 2 + image_gradient.shape[1] ** 2)
    rhos = np.arange(0, rho_max, rho_step)
    thetas = np.arange(-math.pi / 2 + theta_step / 2, math.pi, theta_step)
    ht_map = np.zeros((rhos.shape[0], thetas.shape[0]), dtype=np.float)

    for y in range(image_gradient.shape[0]):
        for x in range(image_gradient.shape[1]):
            if image_gradient[y, x] < gradient_threshold:
                continue
            for theta_bin_idx, theta in enumerate(thetas):
                rho = (y + 0.5) * math.cos(theta + theta_step / 2)
                rho += (x + 0.5) * math.sin(theta + theta_step / 2)
                if rho <= 0:
                    continue
                rho_bin_idx = int(rho / rho_step)
                ht_map[rho_bin_idx, theta_bin_idx] += image_gradient[y, x]
    return ht_map, thetas + theta_step / 2, rhos + rho_step / 2


def are_lines_close(min_delta_rho, min_delta_theta, line1, line2):
    return abs(line1[0] - line2[0]) < min_delta_rho and abs(line1[1] - line2[1]) < min_delta_theta


def convert_line_from_polar_to_normal(rho, theta):
    return math.sin(theta), math.cos(theta), -rho


def get_lines(ht_map, thetas, rhos, n_lines, min_delta_rho, min_delta_theta):
    rho_theta_pairs = [(rho_idx, theta_idx)
                       for rho_idx in range(ht_map.shape[0])
                       for theta_idx in range(ht_map.shape[1])]
    rho_theta_pairs.sort(key=lambda pair: ht_map[pair[0], pair[1]], reverse=True)

    lines_best = []
    are_close = partial(are_lines_close, min_delta_rho, min_delta_theta)
    for index_pair in rho_theta_pairs:
        line = (rhos[index_pair[0]], thetas[index_pair[1]])
        close_line = next((l for l in lines_best if are_close(line, l)), None)
        if close_line is None:
            lines_best.append(line)
            if n_lines <= len(lines_best):
                break

    # vertical lines cannot be given by y=kx+b
    # that's why Ax+By+C=0 will be used.
    return list(map(lambda p: convert_line_from_polar_to_normal(p[0], p[1]), lines_best))


def get_line_y(x, a, b, c):
    if b == 0:
        raise ValueError('Cannot solve for y')
    return int(round((-c - a * x) / b))


if __name__ == '__main__':
    assert len(argv) == 8
    src_path, dst_path, theta, rho, \
    n_lines, min_delta_rho, min_delta_theta = argv[1:]
    with open(join(dst_path, 'call_parameters.txt'), 'w') as fout:
        argv[0] = basename(argv[0])
        fout.write(' '.join(argv))

    theta = float(theta)
    rho = float(rho)
    n_lines = int(n_lines)
    min_delta_rho = float(min_delta_rho)
    min_delta_theta = float(min_delta_theta)

    assert theta > 0.0
    assert rho > 0.0
    assert n_lines > 0
    assert min_delta_rho > 0.0
    assert min_delta_theta > 0.0

    image = cv2.imread(src_path, 0)
    assert image is not None

    image = image.astype(float)
    gradient = gradient_img(image)
    cv2.imwrite(join(dst_path, 'gradients.png'), (gradient / gradient.max() * 255).astype(np.uint8))

    ht_map, thetas, rhos = hough_transform(gradient, theta, rho)
    ht_map_zero_one = ht_map / np.max(ht_map)
    cv2.imwrite(join(dst_path, 'hough_space.png'), np.round(ht_map_zero_one * 255).astype(np.uint8))

    lines = get_lines(ht_map, thetas, rhos, n_lines, min_delta_rho, min_delta_theta)
    with open(join(dst_path, 'lines.txt'), 'w') as fout:
        for line in lines:
            fout.write('%0.3f, %0.3f, %0.3f\n' % line)

    # draw lines
    image_with_lines = cv2.cvtColor(image.astype(np.uint8), cv2.COLOR_GRAY2BGR)
    cols_default = np.arange(0, image.shape[1], 1)
    epsilon = 1e-4
    for idx, (a, b, c) in enumerate(lines):
        if abs(b) < epsilon:
            col = int(round(-c / a))
            cv2.line(image_with_lines,
                     (col, 0),  # accepts "column" coordinate first
                     (col, image.shape[0] - 1),  # accepts "column" coordinate first
                     (255, 0, 0), 4)
        else:
            x0, x1 = 0, image.shape[1] - 1
            y0, y1 = get_line_y(x0 + 0.5, a, b, c), get_line_y(x1 + 0.5, a, b, c)
            cv2.line(image_with_lines,
                     (x0, y0),  # accepts "column" coordinate first
                     (x1, y1),  # accepts "column" coordinate first
                     (255, 0, 0), 4)
    cv2.imwrite(join(dst_path, 'lines.png'), image_with_lines)
